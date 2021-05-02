

;+
; :Description:
;               Hard-coded anomaly present in the Chiron CCD  (Blue orders - right hand side )
;               The image at this point in the software is as follows :
;               from left to right ( 4112 pixel - along the dispersion direction).
;               bottom (red orders ) to top (blue orders) 
;               It also account for the upper-right section of the ccd where the signal of the orders
;               gets to the level of the noise 
;
; : Input :
;          column : column number in the ccd.
;          order  : indexed order number in the ccd. 
;-
function hasAnomaly, column, order   
  anomalyColumns = [3387,3424] 
  anomalyIndexedOrders = [60,75]; from redest to bluest 
  
  noiseColumns = [3840,4111] ; These columns are at the noise level 
  noiseOrders = [62,75]
    
  if order ge  anomalyIndexedOrders[0] then begin   
   if  (column ge  anomalyColumns[0] and column le anomalyColumns[1])   or (column gt noiseColumns[0] )  then return, 1 else return, 0
  endif else return,0

end















;+
; :Description:
;        Returns a Y-value to store in order_ys. This is local Y-value, thus there is further processing to do with the pixel
;
;  Input :
;       i_swath : (2D-ARRAY)  Section of the image array  with approximatly 4112 x 22
;       i_template :  (1D-ARRAY)  12 pixel that define a template for the given order 
;       column_i : Present column in the image that is running 
;       scipy_signal:  Reference to the python method used for cross-correlate 
;       prev_local_row : Previous local pixel output by this function  
;       idx_order : Present indexed order number running  
;       
;  Output: 
;        local Y-value, thus there is further processing to do bring the Local Y-value in the context of the ccd 
;  
;-

function cross_correlate, i_swath,i_template , column_i,  scipy_signal, prev_local_row=prev_local_row, idx_order=idx_order, debug=debug 
      compile_opt idl2
      
      debug=0
      i_swath = reform(i_swath)
      min_val_swath =  min(i_swath)  ; Bring everything down to 0 level momentarely       
      i_swath = double(i_swath - min_val_swath)        
      i_template= reform(i_template)   
      min_val_template=min(i_template)
      i_template=double(i_template - min_val_template )  ; Bring everything down to 0 level momentarely   
      
      
      
      section = reform(i_swath[column_i,* ] )  
      corr= scipy_signal.correlate(section, i_template, mode='same' ) ; Python Cross correlation. Return an array with the same size as 'section'
      
      ;The peak of corr already gives back the middle point of the template
      ; We find all the peaks and pick that one closest to the prev local pixel selected by this function.
      
      corr_peaks_idx =  find_peaks( corr )   
      closest_to_prev = abs(corr_peaks_idx - prev_local_row )   ;Get the closest to prev_local_row
      closest_to_prev = min( closest_to_prev , min_idx)          ; dummy we only care about the idx 
      closest_to_prev = corr_peaks_idx[min_idx]
      
      
      if abs(closest_to_prev  - prev_local_row)  gt 1 and  hasAnomaly(column_i,idx_order) then begin
        
          ; If the proposed value 'fails' and is within the anomaly  (hard coded sectio) 
          ; Or the blue section that is the noise level then we assign whatever the prev value was 
          return, prev_local_row
      endif else begin
        
            
          ; Otherwise we can do some plotting + return the actual local peak found
          if debug gt 0 then begin
            print, 'Previosuly Peak was : ' +strt(prev_local_row)
            print, ' Current Peak is  : ' +strt(closest_to_prev)
            t='column :' +strt(column_i)
            p =plot( section,  LAYOUT=[1,2,1] , title = t)
            p =plot( i_template,  color='blue', /overplot, LAYOUT=[1,2,1] )
            p=plot(corr,/CURRENT,  LAYOUT=[1,2,2] )
            min_val =min(corr)
            max_val =max(corr)
            p=plot([closest_to_prev, closest_to_prev] , [min_val, max_val],   color='red', /CURRENT,  LAYOUT=[1,2,2], /overplot )
            p=plot([prev_local_row, prev_local_row] , [min_val, max_val], LINESTYLE=2,  color='green', /CURRENT,  LAYOUT=[1,2,2], /overplot )
            stop, 'Check Plots. Type  cont. '
          endif
  
  
          return, closest_to_prev ; Recall THE ACTUAL Y VALUE will be  wrt to the general image not the the local peak that we find in here
        
      endelse  

end



























;+
; :Description:
;      Finds the middle peaks of each order. Note it will return the 76 orders but 
;      usually the 1 first order and the last 2 are not taken into account cause they 
;      are incomplete (off the ccd )          ; 
;      OR uses saved polynomial to recalculate the middle order peaks.
;     
; 
; Input : 
;       method_str :  'from_scratch' : Finds the middle peaks of each order from scratch 
;                  :  'use_poly' : Used the previously stored polynomial 
;       
; Output :
;        y_peaks  :(1-d array [76])   the middle peaks  of each order.     
;
;-



function all_order_peaks, flat, redpar, method_str= method_str
  compile_opt idl2

    debug=0
    
    ;; USE THIS SECTION TO MANUALLY ADD OR REMOVE PEAKS. Refer to the plot by setting debug =1 
    ; DON'T FORGET TO COME BACK HERE AND LEAVE THEM EMPTY ONCE IS
    
    
    ; Image is  4112X 1432 for the slicer mode
    img_size=size(flat)     ;size of image passed: e.g. Master Flat
    n_rows= img_size[1]     ;number of rows in image
    n_columns=img_size[2]   ;number of cols in image
      
      
;    if method_str eq 'from_scratch' then begin      
          above_noise_level=5.0 ; Defines how many times bgger(wrt to the Background) the intensity values should get picked
                                 ; Found experimentally. Change if neccesary 
                                
          middle = flat[  round(n_rows/2.0),*]
          middle= reform(middle)
          
          
          noise_level = background_level(middle, noise_width=redpar.ron, section_width=redpar.xwids[redpar.mode] ,  noise_str='band_fragments', debug=debug) ; Has the same with as readout noise
                        ; ron(input) : Read out noise of the frame                                    
                        ; section_width(input) : Used to define the width of the  24.0
                        ; now noise level(output) is a vector with the same length as the middle vector
          
         
              
          idx_peaks = find_peaks(middle, noise_level ,read_out_noise=redpar.ron, above_noise=above_noise_level) ; The value of 10.0 was experimentally found
          
        
          if debug eq 1 then print, 'Number of peaks detected : '+string(n_elements(idx_peaks))      
          peak_intensities = middle[idx_peaks]  
          
    
          if debug gt 0 then begin
              p=plot(middle, title= 'Debug-Plot', xtitle='Cross-dispersion direction', ytitle='electrons count')     
              p=plot(noise_level, color='red', /overplot)
              upper_noise_level= noise_level+ ( redpar.ron * above_noise_level )
              p=plot(upper_noise_level, color='red', /overplot)
              p=plot( idx_peaks,peak_intensities,'+g'   , /overplot)
          endif   
         
    
          
    ; *******************************************
    ; ******  Assuring 3 peaks per order    
    ; *******************************************
          iord=INDGEN(76)
          pkcoefs               = redpar.pkcoefs_slicer      
          middle_peaks_ref      = poly( iord, pkcoefs )  ; restoring them just to check   
          middle_peaks_ref      = round(middle_peaks_ref)
          
          ; Based on these peaks we create groups nearby. We create a list of list
          fixed_peak_indices = INTARR(76*3) ; Since CHIRON CDD is meant to capture 76 orders (3 peaks per order.)
          order_counter = 0 
          
          foreach ref, middle_peaks_ref do begin
               
                 if debug gt 0 then begin
                  print, ''
                  print , '********************'
                  print, 'Order number :' +strt(order_counter)
                endif
    
    
                ;if  ref eq 164 then stop, 'stopped continue '
    
                ;if order_counter lt 18 then pixel_window= 6 else  pixel_window= 8  ; hack to get red orders  just right
                pixel_window= 6 
    
                low_ref  = ref-pixel_window > 0
                upper_ref = ref+pixel_window <  (n_columns-1)
    
    
                indices_of_idx =where( (idx_peaks ge low_ref) and (idx_peaks le upper_ref) , n_group  )
                peaks_group = list() ; new group at every iteration
    
    
                ; >> MORE THAN 3 peaks : need to get rid of some
                ; *******************************************
                if n_group gt 3 then begin
    
                  ; Find the closest to one another
                  selected_idx = idx_peaks[indices_of_idx]
    
                  diff_indices= INTARR(n_elements(selected_idx)-1) ; empt yarray
    
                  for index = 0L, n_elements(selected_idx)-2 do begin
                    diff_indices[index]  = selected_idx[index+1] - selected_idx[index]
                  endfor
    
                  ; need to know which indices to include based on how close they are
                  sorted_diff_indices =  sort( diff_indices)   ; ascending. I am onlty interested in 2 diff (the very first 2 once sorted )
    
    
                  peaks_group.add, selected_idx[ sorted_diff_indices[0] ]
                  peaks_group.add, selected_idx[ sorted_diff_indices[0] +1 ]
    
                  peaks_group.add, selected_idx[ sorted_diff_indices[1] ]
                  peaks_group.add, selected_idx[ sorted_diff_indices[1] +1 ]
    
    
                  ; need to get rid of the repeated one
                  for i= 0L, peaks_group.count()-2 do begin
                    to_delete_idx= peaks_group.where( peaks_group[i], count=c)
                    if c gt 1 then begin
                      peaks_group.REMOVE, to_delete_idx[0]
                      break
                    endif
                  endfor
    
                  if debug gt 0 then begin
                    print, 'Peaks_group : '
                    print, peaks_group
                  endif
    
    
    
    
                  ; >> LESS THAN 3 peaks : need to get rid of some
                  ; *******************************************
                endif else if n_group lt 3 then begin
                  ;  We make the assumption that the peak closest to indices_of_idx is the middle peak
                  ; then we add a peak based on where the second peak is . E.g. If second peak is greater than middle then add the lower peak
                  
                  if n_group eq 0 then stop, '>> Error << : No peaks were found for the present order. Please use a previous order tracing file. '
                    
                  if n_group eq 1 then begin 
                        ; >> EXACT 1 peak : add 2 peaks
                        ; *******************************************
                        selected_idx = idx_peaks[indices_of_idx]
                        peaks_group.add, selected_idx[0]
                        peaks_group.add, selected_idx[0]  + 3 ; since is slicer we make the followign assumption : the three peaks are within 6 pixels
                        peaks_group.add, selected_idx[0]  - 3
                        
                  endif else begin
                        ; >> EXACT 2 peaks : add 1 peak
                        ; *******************************************
                        selected_idx = idx_peaks[indices_of_idx]
                        ; Here, I can assume there are only 2 peaks
                        if abs(selected_idx[0] -ref ) lt abs(selected_idx[1] -ref ) then middle_pk = 0 else   middle_pk = 1
  
                        ; Depengin on what middle peak is we add by left or right
                        if  middle_pk eq 0 then begin  ; >  middle  peak is:  selected_idx[0]
  
                          if selected_idx[1] gt selected_idx[0] then  begin ; this means that the second peak is by the right. so we need to add a peak by the LEFT
                            peaks_group.add, selected_idx[0]
                            peaks_group.add, selected_idx [1]
                            peaks_group.add, selected_idx[0] -abs( selected_idx[1] - selected_idx[0] )
                          endif else begin ; this mean that the 2nd peak is by the left so we need to add a peak by the RIGHT
                            peaks_group.add, selected_idx[0]
                            peaks_group.add, selected_idx [1]
                            peaks_group.add, selected_idx[0]  + abs( selected_idx[1] - selected_idx[0] )
                          endelse
  
                        endif else begin    ; >  middle  peak is:  selected_idx[1]
  
                          if selected_idx[0] gt selected_idx[1] then  begin ; this means that the second peak is by the right. so we need to add a peak by the LEFT
                            peaks_group.add, selected_idx[0]
                            peaks_group.add, selected_idx [1]
                            peaks_group.add, selected_idx[1] -abs( selected_idx[1] - selected_idx[0] )
                          endif else begin ; this mean that the 2nd peak is by the left so we need to add a peak by the RIGHT
                            peaks_group.add, selected_idx[0]
                            peaks_group.add, selected_idx [1]
                            peaks_group.add, selected_idx[1]  + abs( selected_idx[1] - selected_idx[0] )
                          endelse
  
                        endelse
  
                        if debug  gt 0 then begin
                          print, 'Peaks_group : '
                          print, peaks_group
                        endif
                      
                  endelse
                  
                  
                  
    
    
    
                  ; >> EXACT 3 peaks : need to get rid of some
                  ; *******************************************
    
                endif else begin
    
                  peaks_group.add, idx_peaks[ indices_of_idx[0] ]
                  peaks_group.add, idx_peaks[ indices_of_idx[1] ]
                  peaks_group.add, idx_peaks[ indices_of_idx[2] ]
    
                  if debug  gt 0 then begin
                    print, 'Peaks_group : '
                    print, peaks_group
                  endif
    
    
                endelse
    
                every_3_ref = (order_counter*3)+1
                fixed_peak_indices[every_3_ref-1:every_3_ref+1] = peaks_group.toarray()
                order_counter = order_counter+1
               
    
          endforeach ; iterating over the middle ref peak of a n order 
          
          
    
          
         ; nearby_peaks ; are the indices of all the peaks in the middle column   
          if debug gt 0 then begin 
              p=plot(middle, title= 'Final Test with all the peaks' )
              p=plot(fixed_peak_indices, middle[fixed_peak_indices],  'Hr', /overplot )
          endif 
          
           idx_peaks = fixed_peak_indices
    
     ; *******************************************
     ; ******   END - Assuring 3 peaks per order
     ; *******************************************
    
          try_new_peaks:
           
           dummy= where(idx_peaks eq  0 , c)
        
          ; We assuming there could be giving you 75 or 76 orders
          IF  ( n_elements(idx_peaks) eq 225 or n_elements(idx_peaks) eq 228) and c eq 0  then begin    
              
              ; All good as expected           
              ; It gets saved in a different procedure 
              
              idx_peaks= idx_peaks[sort(idx_peaks)]
              return, idx_peaks  ; I am returning all 3 peaks of an order now  to later iterate every three. THIS SHOULD BE CLOSES TO CONSISTEN AS FAR 
                                 ; THE MIDDLE COLUMN IS 1432 Pixels long
                     
          endif  else begin 
            
            print, '>> Error << The program failed to recognized all peaks in the middle column (cross-dispersion direction). '
            print, ''
            print, ' We have two options : '
            print, ' 1) Set the variable use_prev_tracing=1 in the ctio.par file.     '
            print, ' This will use an order tracing susccesfully found of a different night (The closest to this this one) '
            print, ' Please answer with the letter n if you opt for option 1. The program will end and you are expected to change the variable and run again.'
            print,''
            print,  '2) Enter to debug mode. This will produce a plot in the cross-dispersion direction of the approx. 76 orders.
            print, ' you are expected to define the index number (the x value in the plot) of the missing peaks by inspection and enter them in the idl console.'
            print, ''


            B=''
            READ, B, PROMPT=' ctio.par Do you want to enter debug mode ? (y/n) :'


            if B eq 'y' then begin

                  ; Start DEBUG MODE
                  print, ' All peaks found by the software are marked with a cross in the plot.
                  print, ' E.g. If the peaks at x=160 and x=1139 are missing then insert:
                  print, ' idl> 160'
                  print, ' idl> 1139'
                  print, ' idl> -1'
                  print , 'The -1 will let the program know you are done. Next, the program will ask if there are any extra peaks '
                  print,  'that should not be considred.If there is not any, simply insert -1.
                  print,  'Please insert the value as the e.g. above and -1 to terminate '
    
                  print, 'Please take your time to see the plot !! and write down the peaks. The plot will freeze once you continue.'
    
                  p=plot(middle, title= 'Debug-Plot', xtitle='Cross-dispersion direction', ytitle='electrons count')
                  p=plot(noise_level, color='red', /overplot)
                  upper_noise_level= noise_level+ ( redpar.ron * above_noise_level )
                  p=plot(upper_noise_level, color='red', /overplot)
                  p=plot( idx_peaks,peak_intensities,'+g'   , /overplot)
    
                  stop, 'Please type   .cont  :'
    
                  inputC = 0
                  missing_peaks= list()
    
                  while (inputC ne -1 ) do begin
    
                    READ, inputC, PROMPT='MISSING Peak : '
                    if inputC ne -1 then missing_peaks.add, inputC
                  endwhile
    
                  missing_peaks =missing_peaks.toarray()
    
                  inputC = 0
                  extra_peaks= list()
    
                  while (inputC ne -1 ) do begin
                    READ, inputC, PROMPT='EXTRA  Peak  : '
                    if inputC ne -1 then extra_peaks.add, inputC
                  endwhile
    
    
    
                  print, "Summary of debug mode "
                  print, 'Exta Peaks :'
                  print, extra_peaks
                  print, 'Missing Peaks : '
                  print, missing_peaks
    
                  print, 'Number of peaks previously detected :'+strt(n_elements(idx_peaks))
    
                  if extra_peaks.count()  gt 0 then begin

                      foreach missing_peak, missing_peaks do begin
                        indices =where(idx_peaks ne missing_peak, n ) ; these are goood peaks ; THESE SHOULD BE ALL IF NONE IS CHOOSE
                        idx_peaks =idx_peaks[indices]
                      endforeach

                  endif



                  idx_peaks=[idx_peaks, missing_peaks]
                  print, 'Number of peaks detected NOW :'+strt(n_elements(idx_peaks))

                  GOTO, try_new_peaks

              endif else stop, ' >>  Program stopped  at OPTIMAL_TRACING.PRO << Run again with prev_order_tracing=1 in the ctio.par'


            
          endelse ; else of debug mode 
            
          
                          
        
;    endif else stop,  '>> ERROR <<  Argument method_str does not match any avaialble option '
 
  
  
end














;+
; :Description: 
;            Defines the templates to be used to trace the orders
;            
; Input: 
;       flat (2d-array) : equivalent to the master flat frame used for tracing the orders
;       redpar          : Structure with global variables. 
; 
; Output : 
;        order_templates : (1d-array of structures )  E.g. 74 x Structure  ({middle, up, down })  Each defines a template
;        up and down define the pixel taken  to define a template  (the diff between up and down = 12 pixels )
;        and middle is the middle between down and up 
;
;-
function define_templates, flat, redpar
  
  print, 'OPTIMAL_ORDER_TRACING: Defining templates to be used for cross-correlation. Please Wait ....'
  debug = 0 ; Make it greater than 1 for detailed plotting

  y_peaks=all_order_peaks(flat, redpar, method_str='from_scratch')  ; Finds the all peaks of each order. Note it will return the 76 orders but 
                                                                       ; we will get rid of some of them 
                                                                       ; Need to iterate for every thee
  
  if debug  then p=plot(y_peaks, title='These are all the peaks found', xtitle='Indices of peaks', ytitle='indices of peaks wrt column')                                                                       
  img_size=size(flat)     ;size of image passed: e.g. Master Flat ; Image is 4112 x 1366 for the slicer mode
  n_columns=img_size[1]   ;number of cols in image E.g. 4112
  n_rows= img_size[2]     ;number of rows in image E.g 1366
  
  middle_column = round( n_columns/2.0 ) 
  
  if debug gt 0 then begin 
      print, 'Order width : ' +strt(redpar.xwids[redpar.mode] )
  endif 
 
  orders_to_extract = redpar.nords  ; There are really 76 Orders in the cdd However since the very first and 
                                    ; the very last go off the cdd then, we can really get a max of 74 orders 
                                               
  ;>> Initiate info to be returned
  order_templates= {middle:fltarr(orders_to_extract ), up:fltarr(orders_to_extract ), down:fltarr(orders_to_extract)}

  
  
  middle_peak_idx=4 ; Starts from 4 cause this is the first middle peak since we don t account for the first red order, Then increse every three . E.g 1,4,7
                    ; this acts as reference to keep iterating every thee 
  
  
  ;>> Extract +-3 of xwid  just to validate the peak already selected.
  ; Here is where the order are ommited if neccessary. The first red order is not included and the number 
  ; of blue orders to include (up to) is defined by the ctio.par file. 
  ; Take into account that even tho there are 76 order the max number order tested was 73 (implies 74 orders )
  for index = 1L, orders_to_extract do begin ; Iteration for each template
     
        
        ;Hack to compensate for the overlapping in the red ordesr. 
        if  index gt  28 then extra_width=3 else   extra_width =1
        
        
        ;Definning the middle peaks  + the other side peaks of a given order

       
;        low= round(y_peaks[middle_peak_idx] - (  (redpar.xwids[redpar.mode]/2.0 )  +  extra_width )  )
;        up= round( y_peaks[middle_peak_idx]  + (  (redpar.xwids[redpar.mode]/2.0 )  +  extra_width ) )
        
        if debug  gt 0 then begin 
            print, " *************** "
            print, " *************** "
            print, 'Order : '+strt(index)
;            print, 'Template to work with : [' +strt(low) +' , '+strt(up) +' ]'
;            print, 'y-peak found : '+strt(round(y_peaks[index]) )

        endif 
 

         low_bound = y_peaks[middle_peak_idx-1] 
         up_bound =  y_peaks[middle_peak_idx+1] 
        
        if debug gt 0 then begin 
            print, ' low_bound :'+strt(low_bound)
            print, ' up_bound :'+strt(up_bound)
        endif 


            ;*********************************
            ;* Creating Optimal Template
            ;*********************************         
             
            pixel_diff  = abs(up_bound - low_bound ) +1 ; to account cause when substracting not taking low bound into accoun . E.g 9-2 = 7 (But pixel considered = 8 )
            
            if debug gt 0 then begin
                print, 'After finding the 3 peaks. The difference between the 2 furthest peaks is : '+strt( pixel_diff) 
                print, 'where the bounds are : [' +strt(low_bound) + ' , '+strt(up_bound) + ' ]'
            endif
            
            if pixel_diff gt redpar.xwids[redpar.mode] then stop, ' >> Error <<  The pixel diff cannot be greater than 12 pixel (For the slicer mode ). The peaks we supposed to be within the 12 pixels '
             
            missing_px_num = redpar.xwids[redpar.mode]  -  pixel_diff   ; plus+ to account for substraction 
            
            if missing_px_num mod 2 eq 0 then begin ; if even 
                 low_bound = low_bound - round(missing_px_num/2)
                 up_bound = up_bound + round(missing_px_num/2)
            endif else  begin ; is odd for some reason 
                missing_px_num =missing_px_num-1 ; to make it even 
                low_bound = low_bound - round(missing_px_num/2)
                up_bound = up_bound + round(missing_px_num/2) 
                
                ; >> We add the +1 pixel to complete the 12 pixels based on which pixel either lower or upper is higher (this translates to if the pixel belongs to the order or  not )
                if flat[middle_column, low_bound-1  ] gt flat[middle_column,  up_bound+1  ] then begin
                  low_bound =low_bound -1 
                endif else up_bound = up_bound+1
              
            endelse
            
            if debug gt 0 then begin              
              print, 'After  first  *compensating for missing pixels ' 
              print, ' the bounds are : [' +strt(low_bound) + ' , '+strt(up_bound) + ' ]'
              print, 'The total number of pixels is now : ' +strt(up_bound-low_bound+1)
            endif
            
            ; Making sure we got the 12 pixel 
            if up_bound - low_bound  +1 ne redpar.xwids[redpar.mode] then stop, '>> Error << the potential optimal template must have size of xwdith but has size = ' +strt(n_elements(flat[low_bound: up_bound ]))
            
            
            
            ;*********************************
            ;* 2nd Compensation : Last check by producing a shift of +/1 pixel only for middle + blue orders 
            ;*********************************
            
            ; Shifting by +/- 1 pixel to make sure got optimal template
            ; This only runs for the middle and red orders . Cause the background is large enough. We assume after the 10th red order will be ok . 
           
              if index  ge 10 then begin 
                potential_minus_low_bound =  low_bound -1
                potential_minus_up_bound =   up_bound  -1
    
                potential_plus_low_bound  =low_bound   +1
                potential_plus_up_bound  = up_bound  +1
                
                ; Recall to add low  to pass from local to the general spectrum 
                sum_no_shift = total( reform( flat[middle_column,  low_bound: up_bound]   ) )
                sum_minus    = total( reform( flat[middle_column,  potential_minus_low_bound : potential_minus_up_bound] ) )
                sum_plus     = total( reform( flat[middle_column,  potential_plus_low_bound : potential_plus_up_bound]   ) )
    
                sorted_idx = REVERSE( sort([sum_no_shift, sum_minus, sum_plus])) ; descending
    
    
                if sorted_idx[0] eq 0 then begin
                  ; This is the sum spectra with no shift
                  ; Dont do anything. The bounds will remain as thye are
                endif else if sorted_idx[0] eq 1 then begin
                  up_bound = potential_minus_up_bound
                  low_bound =  potential_minus_low_bound
    
                endif else if sorted_idx[0] eq 2 then begin
                  up_bound = potential_plus_up_bound
                  low_bound =  potential_plus_low_bound
    
                endif else stop, '>> Error <<   sorted _idx shoud only take values of 0,1,2'
    
                ; END  the optimal profile is from [low_bound :up_bound ] This should have 12 pixel such that the middle pixel
    
                
                if debug gt 0 then begin

                  print, 'After  2nd  *compensating for missing pixels '
                  print, ' the bounds are : [' +strt(low_bound) + ' , '+strt(up_bound) + ' ]'
                  print, 'number of pixels is now : ' +strt(up_bound-low_bound+1)
                endif
                
              endif
              
              ;*********************************
              ;* Store order-template information
              ;*********************************
               ; Index -1 is to compensate the loop starting from 1
              order_templates.middle[index-1] =  low_bound + round(redpar.xwids[redpar.mode] /2.0 ) ; NEED TO DUBLE CHECK BOXCAR TO FIND HOW IT DEFINES THE MIDDLE
              order_templates.down[index-1]   =  low_bound   ; + low is to compensate for the fact that the pixel template are taken as local when they are part of somthing bigger
              order_templates.up[index-1]     =  up_bound
              
              if debug gt 0 then  begin
                   print, 'Order-template information : middle : ' +strt(order_templates.middle[index-1] ) + ' | down :'+strt(order_templates.down[index-1] ) + ' | up:'+strt(order_templates.up[index-1] )
                           
              
              endif
      
      middle_peak_idx = middle_peak_idx+3 ; reference for the middle peak of an orde r       
             
  endfor
  
  if debug gt 1 then begin
     
     p=plot(flat[middle_column, *  ])
      ; Now plot the choosen bounds one by one 
     min_flux = min(reform(flat[middle_column, *  ]) )
     max_flux =max( reform(flat[middle_column, *  ]) )
     
      for i= 0 , 73 do begin
        
        p=plot( [ order_templates.down[i] , order_templates.down[i] ], [min_flux, max_flux], color = 'red' , /overplot)
        p=plot( [ order_templates.up[i]   , order_templates.up[i]   ], [min_flux, max_flux], color = 'red' , /overplot)

      endfor
  
  endif



  return, order_templates
end 
  




















;Summary:
;      ** Main Function **
;       called from ctio_dord
;       Main method calls 
;       Only optimized for slicer but can be applied to other modes with minor adjustments
;       
;       In Generatl Keepp in mind that Chiron's ccd records 76 orders  ([0,75]) 
;       However the 1st [0] and last 2 orders [74,75] are incomplete and therefore are not considered 
;        
;       * This has only been tested for the slicer mode
;Input:
;      img:  2-D image of the master flat
;      initial_order_peaks:
;
;Output:
;      array with the  middles of all the orders for all the columns (4112 ) E.g.  74 x 4112
;      
;;Notes:
;      the polynomial "pkcoefs_slicer" was found from the 0th order to the nth order.
;      If extracting 74 order for example it would be from 0 to 74
; Written by Jorge Lozano 2021-14-03

function optimal_order_tracing, img, redpar

  ;------------------------------
  ; >> Constants
  ;------------------------------
  debug    = 0; redpar.debug
  flat     = img
  img_size = size(flat)      ;size of image passed: e.g. Master Flat  4112 x 1366 for the slicer mode
  n_columns   = img_size[1]     ;number of rows in image
  n_rows      = img_size[2]     ;number of cols in image
 
  nord     = redpar.nords    ;total number of orders
  iord     = findgen(nord)   ;Generated Orders in range [0,nord]  
  orcdeg = 4.          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome)
  middle_column = round( n_columns/2.0 )
  scipy_signal= Python.Import('scipy.signal')
  slicer_width= round(redpar.xwids[redpar.mode] )

  ;------------------------------
  ; >> define n  templates (for each order)
  ;------------------------------  
  

  
  
  templates = define_templates(flat , redpar) ; n orders structure  with middle, up and down for every order (central column)
  

  if debug gt 0 then stop, " ** Templates recognizition  Sucessfull *** "
  
  ;------------------------------
  ; >> Cross-correlate every order 
  ;------------------------------
  

  orc= dblarr(orcdeg+1,nord) ; Initializing array to store polynomia of each order. +1 cause the contant 
  ; > Iterate over each order
  ; > cross correlate along the order,  such that I end up with 4112 points per order
  ; > Fit polynomial to 4112 points
  
  order_ys = MAKE_ARRAY(n_elements(templates.middle ), n_columns, /FLOAT, VALUE = 0.0) ; (#of Orders, # X Pixels (alond dispersion) )

debug_flag = 0
  
 print, 'OPTIMAL_ORDER_TRACING: Tracing the orders. Please wait ....'
 for ord_idx = 0L, n_elements(templates.middle )-1 do begin   
                     
       print, 'OPTIMAL_ORDER_TRACING: Tracing  indexed order: ' +strt(ord_idx)     
        i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                           ; Initial Template
        i_swath =     flat [ *,  templates.middle[ord_idx]  -slicer_width : templates.middle[ord_idx] + slicer_width-1 ]  ; Initial Swath        
        order_ys[ ord_idx,middle_column] = templates.middle[ord_idx] ; Insert value for  the middle
   

        prev_local_row =slicer_width
        
        ; >> Left side of Order
        ;----------------------.
       
        FOR x = middle_column, 1,-1  DO BEGIN
          back_x = x-1          
          
          ;  > Returns Y value to store in order_ys. This Y value is such that when add + 6 and -6 will give back the order perfectly       
          local_row= cross_correlate( i_swath,i_template , back_x, scipy_signal, prev_local_row = prev_local_row , idx_order=ord_idx,  debug =debug )   
          ; Returns local_row wrt to i_swath
          
          sz_swath= size(i_swath)   ; Make sure swath is even at all times
          order_ys[ord_idx,back_X] = ( order_ys[ord_idx,back_X+1] -  round(sz_swath[2]/2.0 ) )  + local_row
                                     ; Initial point of the swath used for the present iteration
  
          ;  > Define new i_swath for the next iteration
          i_swath = flat [ *,  order_ys[ord_idx,back_X] -slicer_width  : order_ys[ord_idx,back_X] +slicer_width-1  ]
          

          ;  > Tricky . Better to define with we already got cause if ther is a new swath then the peak had to shift along with the new swath
          ;  If section went up we want to substract if section went down we want to add
          prev_local_row = local_row  -(order_ys[ord_idx,back_X] - order_ys[ord_idx,back_X+1] ); Problem is that if I move swath ref then prev local will shouls be moved as well
          

        ENDFOR
        
        
        
        ; >> Right side of Order
        ;-----------------------

        i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                        ; Define Section
        i_swath =     flat [ *,  templates.middle[ord_idx] -slicer_width : templates.middle[ord_idx] + slicer_width-1   ]  ; Define Swath
        prev_local_row =  slicer_width       
        
        FOR x=middle_column, n_columns-2  DO BEGIN       
          ; Same Idea as for Left side
          forward_x = x+1
          local_row= cross_correlate( i_swath,i_template , forward_x, scipy_signal, prev_local_row=prev_local_row, idx_order=ord_idx , debug =debug )
          sz_swath= size(i_swath)
          
          order_ys[ord_idx,forward_x] = (order_ys[ord_idx,forward_x-1] - round(sz_swath[2]/2.0 ) ) + local_row            
          i_swath = flat [ *,  order_ys[ord_idx,forward_x] -slicer_width : order_ys[ord_idx,forward_x] +slicer_width-1  ]          
           prev_local_row = local_row  -(order_ys[ord_idx,forward_x] - order_ys[ord_idx,forward_x-1] )

        ENDFOR
        
        
        if  debug gt 3 then  begin
           
           if flag eq 0 then  p=image(flat  ) else debug_flag =1
           
           
           bellow = round(reform(order_ys[ord_idx,*])) -6
           above =round( reform( order_ys[ord_idx,*])) +6  ; +6 just for plotting since the plot will draw the line at the beginning of the plot
           
           p=plot( bellow, color='red' ,/overplot)
           p=plot( above, color='red' ,/overplot)
           
           ;stop, 'plotted order :  '+strt(ord_idx)
           
        endif

       

 endfor
 
 
;  debug = 1
if debug gt 0 then begin       
    p=plot(order_ys[0, *] )    
    for ord_idx = 1L, n_elements(templates.middle )-1 do begin
        p=plot( order_ys[ord_idx, *] , /overplot )        
    endfor
    stop, 'look at the plot '
endif

 


  return, order_ys ; Order Coefficients
end
