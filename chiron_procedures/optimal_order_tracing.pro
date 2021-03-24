

;+
; :Description:
;  Returns Y value to store in order_ys. This Y value is such that when add + 6 and -6 will give back the order perfectly
;
;  Input :
;       i_swath :
;       i_template : 
;       column_i :
;       
;  
;-

;  > Returns Y value to store in order_ys. This Y value is such that when add + 6 and -6 will give back the order perfectly

function cross_correlate, i_swath,i_template , column_i,  scipy_signal, prev_local_row=prev_local_row,  debug=debug 
  compile_opt idl2
      
    
      i_swath = reform(i_swath)
      min_val =  min(i_swath)  ; Just to bring everything down to 0 levl momentarely 
      i_swath = double(i_swath - min_val)      
      i_template=double( reform(i_template) - min_val ) 
      
      
      
      section = reform(i_swath[column_i,* ] )  
      
       
      
      corr= scipy_signal.correlate(section, i_template, mode='same' )
      
;      corr= smooth(corr, 3)
      ;corr= corr_order(i_template, section  )
      ;The peak of corr is apperently already giving me the middle point
      ; It takes the middle point of my template and corr has a peak wrt to the template(wich is the middle of the template)
      
      corr_peaks_idx =  find_peaks( corr )
      
     
;      corr_peaks_idx = round(n_elements(i_template)/2.0 ) + corr_peaks_idx 
      
      
      
      if debug gt 0 then begin
        print, 'Peaks found (indices) :'
        print, corr_peaks_idx

      endif
      
      ;Get the closest to prev_local_row
      closest_to_prev = abs(corr_peaks_idx - prev_local_row )
      closest_to_prev = min( closest_to_prev , min_idx) ; dummy we only care about the idx 
      closest_to_prev = corr_peaks_idx[min_idx]
      
      
      if debug gt 0 then begin        
        print, 'Previosuly Peak was : ' +strt(prev_local_row)
        print, ' Current Peak is  : ' +strt(closest_to_prev)
      endif
      
      
     ;  dummy =  min(corr, min_index)
     ;  dummy =  max(corr, max_index) ; instead find all the peaks and choose the one closes to prev_local_row
      
       
;      print, 'Corr : '
;      print, corr
;      print, 'Min_index :'+strt(min_index)
      
     ; min_index = round(n_elements(i_template)/2.0) + min_index ; i_template/2.0 will give me the start point (wrt to the middle of the template)
                                                    ; min_index wills shift it according to where the minimum is found
      
;      if column_i mod 31 eq 0 then debug=1 else  debug =0
      if debug gt 0 then begin
        t='column :' +strt(column_i)
        p =plot( section,  LAYOUT=[1,2,1] , title = t)
        p =plot( i_template,  color='blue', /overplot, LAYOUT=[1,2,1] )
        p=plot(corr,/CURRENT,  LAYOUT=[1,2,2] )
        min_val =min(corr)
        max_val =max(corr)
        p=plot([closest_to_prev, closest_to_prev] , [min_val, max_val],   color='red', /CURRENT,  LAYOUT=[1,2,2], /overplot )
        p=plot([prev_local_row, prev_local_row] , [min_val, max_val], LINESTYLE=2,  color='green', /CURRENT,  LAYOUT=[1,2,2], /overplot )
        
        stop, 'CHekc plots '
      endif
 
  
  return, closest_to_prev ; Recall THE ACTUAL Y VALUE will be  wrt to the general image not the the local peak that we find in here 
end



























;+
; :Description:
; 
; Input : 
;       method_str :  'from_scratch' : Finds the middle peaks of each order from scratch 
;                  :  'use_poly' : Used the previously stored polynomial 
;       
; Output :
;        y_peaks  
;
;-



function order_middle_peaks, flat, redpar, method_str= method_str
  compile_opt idl2

  debug=0
  ; Image is  4112X 1432 for the slicer mode
  img_size=size(flat)     ;size of image passed: e.g. Master Flat
  n_rows= img_size[1]     ;number of rows in image
  n_columns=img_size[2]   ;number of cols in image
  
  
  if method_str eq 'from_scratch' then begin
      
      
      above_noise_level=10.0 ; Defines how many times bgger(wrt to the Background) the intensity values should get picked
                            ; Found experimentally. Change if neccesary 
                            
      middle = flat[  round(n_rows/2.0),*]
      middle= reform(middle)
      if debug gt 0 then p=plot(middle)
      
      noise_level = background_level(middle, noise_width=redpar.ron, section_width=redpar.xwids[redpar.mode] ,  noise_str='band_fragments', debug=debug) ; Has the same with as readout noise
                    ; ron(input) : Read out noise of the frame                                    
                    ; section_width(input) : Used to define the width of the  24.0
                    ; now noise level(output) is a vector with the same length as the middle vector
      
      if debug gt 0 then begin        
          p=plot(noise_level, color='red', /overplot)   
          upper_noise_level= noise_level+ ( redpar.ron * above_noise_level )
          p=plot(upper_noise_level, color='red', /overplot)
      endif
     
     
      
      idx_peaks = find_peaks(middle, noise_level ,read_out_noise=redpar.ron, above_noise=above_noise_level) ; The value of 20.0 was experimentally found

      peak_intensities = middle[idx_peaks]
      ; plot the 1-value noise level
      if debug gt 0 then  p=plot( idx_peaks,peak_intensities,'+g'   , /overplot)

                
       
       all_indices = indgen(   n_elements(middle) )
       
       multiples_of_3 = where(all_indices mod 3  eq 0, n3)
       
       multiples_of_3= multiples_of_3 + 1L ; multiples_of_3 = 0,3,6,9...   +1 -> 1,4,5,... (The are the positions of the middle peaks )
       
      ; >> Group the peaks in groups of three. 
      n_orders= redpar.nords
      ; Check if number of peaks is a multiple of 12 othwerwise throw exception
      n_orders_found =n_elements(idx_peaks) /3.0
      IF  ( n_elements(peak_intensities) mod 3 ) eq  0 then begin 
          
          
          if debug  gt 0 then print, 'Number of Orders found : '+strt(n_orders_found)
        
          indices_middle_peaks=idx_peaks[multiples_of_3[0:-2]]     ; Dont take last    multiples_of_3 into account  
          
          if debug  gt 0 then p =plot(indices_middle_peaks, middle[indices_middle_peaks],  'Hr', /overplot  )
          return, indices_middle_peaks; This is what we are after 
          
      endif else stop, '>> Error << The number of peaks returned is not a multiple of three. Please check the plot and try again. '
      
      
      
                    
    
  endif else if method_str eq 'use_poly' then begin
    
    ;>> The polynomial "pkcoefs_slicer" has the middle points of each order in the cross-dispersion direction
    pkcoefs =redpar.pkcoefs_slicer  ; POlynomial is restored from ctio.par 
    y_peaks = poly(iord,pkcoefs)

  
    
    return, y_peaks 
  endif else stop,  '>> ERROR <<  Argument method_str does not match any avaialble option '
 
  
  
  
end











;+
; :Description: Defines the templates to be used to trace the orders
;
;-
function define_templates, flat, redpar
  
  
  debug = 0

  y_peaks=order_middle_peaks(flat, redpar, method_str='from_scratch')
  ; Image is 4112 x 1366 for the slicer mode
  img_size=size(flat)     ;size of image passed: e.g. Master Flat
  n_columns=img_size[2]   ;number of cols in image
  n_rows= img_size[1]     ;number of rows in image
  
  middle_column = round( n_rows/2.0 ) 
  
  if debug gt 0 then begin 
      print, 'Order width : ' +strt(redpar.xwids[redpar.mode] )
  endif 
 
  
  
  orders_to_extract = redpar.nords  ; There are really 76 Orders in the cdd However since the very first and 
                                    ; the very last go off the cdd then, we can really get a max of 74 orders 
                                    
                                    
  ;>> Initiate info to be returned
  order_templates= {middle:fltarr(orders_to_extract ), up:fltarr(orders_to_extract ), down:fltarr(orders_to_extract)}

  
  ;>> Extract +-3 of xwid  just to validate the peak already selected.
  for index = 1L, orders_to_extract do begin ; Iteration for each template
    
        ; I start from 1L since the frst order does not really count since it is incomplete 
        
        print, index
        ;Hack to compensate for the overlapping in the red ordesr. 
        if  index gt  28 then extra_width=3 else   extra_width =1
        
       
        low= round(y_peaks[index] - (  (redpar.xwids[redpar.mode]/2.0 )  +  extra_width )  )
        up= round(y_peaks[index]  + (  (redpar.xwids[redpar.mode]/2.0 )  +  extra_width ) )
        
        if debug  gt 0 then begin 
            print, " *************** "
            print, " *************** "
            print, 'Order : '+strt(index)
            print, 'Template to work with : [' +strt(low) +' , '+strt(up) +' ]'
            print, 'y-peak found : '+strt(round(y_peaks[index]) )

        endif 
        
        ; This is going to mess up the initial and  final orders so take care of these down  TO FIX !!!
        low =low > 1
        up =up < 1430  ; chagen !!!OT FIX 
        
        template  = flat[middle_column, low:up  ]
        template = reform(template)
        
        
     
       
        ;Smooth the template with a moving avarage 
        ;smooth_template= ts_smooth(template,3)
        
        
        
        
        background = background_level( template, noise_width=redpar.ron , noise_str = 'band_fragments' , section_width=float(n_elements(template)) , poly_order=1 )
        

        peak_indices = find_peaks( template, background, above_noise=2.0, read_out_noise=redpar.ron ) ; This applies only for 1 template 
        
        peak_values = template[peak_indices]
        
        
        
        ;*********************************
        ;* Define Initial upper-lower bounds 
        ;*********************************
        
        if n_elements(peak_values) lt 3 then stop, '>> Error << Numper of peak found is less than 3 for  spectra '
        idx_sorted_peaks =  sort(peak_values) ;  Ascending order : these indices are relatative to the list created within find_peaks 
        idx_sorted_peaks = reverse(idx_sorted_peaks) ; Now in descending order. From highest to lowest
        idx_sorted_peaks = idx_sorted_peaks[0:2] ; Just the 3 highest peaks . Which presumable have to me the ones that belong to the order. 
        peak_idx_of_template = peak_indices[ idx_sorted_peaks  ]  ; but out of these indices we only care about the indes of the side peaks (meaning not the middle peak)
        
        
        peak_idx_of_template =  peak_idx_of_template [ sort(peak_idx_of_template)] ;Ascending; 
        
        ; So far bounds in the cross dispersion direction for this order 
        low_bound = peak_idx_of_template[0]
        up_bound = peak_idx_of_template[2]
        
    ;    optimal_found=0
    ;    while (optimal_found eq 0) do begin
          
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
                if flat[middle_column,low + low_bound-1  ] gt flat[middle_column, low + up_bound+1  ] then begin
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
                sum_no_shift = total( reform( flat[middle_column,  low+low_bound: low+up_bound]   ) )
                sum_minus    = total( reform( flat[middle_column,  low+potential_minus_low_bound : low+potential_minus_up_bound] ) )
                sum_plus     = total( reform( flat[middle_column,  low+potential_plus_low_bound : low+potential_plus_up_bound]   ) )
    
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
              order_templates.middle[index-1] = low+ low_bound + round(redpar.xwids[redpar.mode] /2.0 ) ; NEED TO DUBLE CHECK BOXCAR TO FIND HOW IT DEFINES THE MIDDLE
              order_templates.down[index-1]   = low + low_bound   ; + low is to compensate for the fact that the pixel template are taken as local when they are part of somthing bigger
              order_templates.up[index-1]     = low+ up_bound
              
              if debug gt 0 then  begin
                   print, 'Order-template information : middle : ' +strt(order_templates.middle[index-1] ) + ' | down :'+strt(order_templates.down[index-1] ) + ' | up:'+strt(order_templates.up[index-1] )
                           
              
              endif
              
              
;              if debug  gt 0 then begin
;                
;               ; test_low_bound= y_peaks[index] -12 > 0
;                ;test_up_bound= y_peaks[index] +12  < 1365
;;                pxls = indgen( test_up_bound - test_low_bound ) + test_low_bound
;                
;;                if index gt 65 then begin
;                  min_ref_val = min( flat[middle_column, * ] )
;                  max_ref_val =  max(flat[middle_column, * ])
;
;
;                  p=plot(  flat[middle_column, * ] )
;                  p=plot( [order_templates.down[index-1], order_templates.down[index-1] ] , [min_ref_val,max_ref_val ] , color='red', /overplot)
;                  p=plot( [order_templates.up[index-1], order_templates.up[index-1] ] , [min_ref_val,max_ref_val ] , color='red', /overplot)
;
;
;
;                  ;                p=plot(background , color='red', /overplot)
;                  ;                p=plot(peak_indices, peak_values, 'Hr',  /overplot)
;;                    stop, 'prrr'
;;                endif
;                  
;               
;                
;              endif
;
;
;              stop, 'gotta stopped '
              
       
          
    
     
   
    

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
;Input:
;      img:  2-D image of the master flat
;      initial_order_peaks:
;
;Output:
;      array 
;      
;;Notes:
;      the polynomial "pkcoefs_slicer" was found from the 0th order to the nth order.
;      If extracting 74 order for example it would be from 0 to 74
; Written by Jorge Lozano 2021-14-03

function optimal_order_tracing, img, redpar

  ;------------------------------
  ; >> Constants
  ;------------------------------
  debug    = 0 ;redpar.debug
  flat     = img
  img_size = size(flat)      ;size of image passed: e.g. Master Flat  4112 x 1366 for the slicer mode
  n_columns   = img_size[1]     ;number of rows in image
  n_rows      = img_size[2]     ;number of cols in image
 
  nord     = redpar.nords    ;total number of orders
  iord     = findgen(nord)   ;Generated Orders in range [0,nord]  
  orcdeg = 4.          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome)
  middle_column = round( n_columns/2.0 )
  scipy_signal= Python.Import('scipy.signal')

  

  
  
  
;  ;BUT  eventually loss of  precision begins increasing the errors again.MAKE SURE RESIDUALS DECREASE.
;  mmfrac = 0.05       ;maximum fraction missing peaks allowed. Only Up to 5% of the spectrum can be missing.
;  maxome = 10.        ;max allowable mean pixel error in orcs. Previous = 2 i ---------------------NEEDS IMPLEMENTATION

  
  ;------------------------------
  ; >> define n  templates (for each order)
  ;------------------------------
   
  
  templates = define_templates(flat , redpar) ; n orders structure  with middle, up and down for every order (central column)
  
  
  if debug gt 0 then print, " ** Templates recognizition  Sucessfull *** "
  ;------------------------------
  ; >> Cross-correlate every order 
  ;------------------------------
  

  orc= dblarr(orcdeg+1,nord) ; Initializing array to store polynomia of each order. +1 cause the contant 
  ; > Iterate over each order
  ; > cross correlate along the order,  such that I end up with 4112 points per order
  ; > Fit polynomial to 4112 points
  
  order_ys = MAKE_ARRAY(n_elements(templates.middle ), n_columns, /FLOAT, VALUE = 0.0) ; (#of Orders, # X Pixels (alond dispersion) )
  debug_ys = MAKE_ARRAY( n_columns, /FLOAT, VALUE = 0.0)

  swath_width = 17
  for ord_idx = 0L, n_elements(templates.middle )-1 do begin
       
       debug =1 
       if debug  gt 0 then print, 'Now in order: ' +strt(ord_idx)
       
;       if ord_idx lt 2 then begin
;        swath_width = 10
;        
;       endif else if ord_idx ge 2 and ord_idx lt 72 then begin
;        swath_width = 10
;        
;       endif else swath_width =10
       
        ; must be greate than xwid/2 ; Thus., we assuming that the whole order must be places in within a band of +- swath_width
       
       i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                        ; Define Section 
       i_swath =     flat [ *,  templates.middle[ord_idx] - swath_width : templates.middle[ord_idx] + swath_width-1   ]  ; Define Swath 
        
        order_ys[ ord_idx,middle_column] = templates.middle[ord_idx] ; Insert value for  the middle
        debug_ys[middle_column] = templates.middle[ord_idx] 
        
        swath_sz=size(i_swath)
        prev_local_row =round(swath_sz[2]/2.0 )
        
        ; >> Left side of Order
        ;----------------------.
       debug=0
        FOR x = middle_column, 1,-1  DO BEGIN
          back_x = x-1          
          
          ;  > Returns Y value to store in order_ys. This Y value is such that when add + 6 and -6 will give back the order perfectly          
           ;IF back_x le 1942 and back_x gt 1920 then debug = 1 else debug = 0
          local_row= cross_correlate( i_swath,i_template , back_x, scipy_signal, prev_local_row = prev_local_row ,   debug =debug )   
          
 
          prev_local_row = local_row
          
;          print, 'Local_row :local_row '
;          sz_swath= size(i_swath)
          order_ys[ord_idx,back_X] = templates.middle[ord_idx] -  swath_width   + local_row
          
          
   ;       i_template =  flat [ back_x ,  order_ys[ord_idx,back_X] -6  : order_ys[ord_idx,back_X] + 5 ]  ;>> Key : update new template accordingly
;         i_swath = flat [ *,  order_ys[ord_idx,back_X] -10  : order_ys[ord_idx,back_X] +10  ]
          
          
          debug_ys[back_X] = local_row

        ENDFOR
        
        ; >> Right side of Order
        ;-----------------------
        
        ; redefine i template and i_swath since start from the middle again
        i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                        ; Define Section
        i_swath =     flat [ *,  templates.middle[ord_idx] - swath_width : templates.middle[ord_idx] + swath_width   ]  ; Define Swath
        
        prev_local_row =round(swath_sz[2]/2.0 )
        
        FOR x=middle_column, n_columns-2  DO BEGIN
          
          forward_x = x+1
          local_row= cross_correlate( i_swath,i_template , forward_x, scipy_signal, prev_local_row=prev_local_row, debug =0 )
          
          prev_local_row = local_row
          
          order_ys[ord_idx,forward_x] = templates.middle[ord_idx] - swath_width + local_row  
          
        ;  i_template =  flat [ forward_x ,  order_ys[ord_idx,forward_x]-6  : order_ys[ord_idx,forward_x]+ 5 ]  ;>> Key : update new template accordingly

          
          
          debug_ys[forward_x] = local_row
        ENDFOR

       
;       debug=1
;       if debug  gt 0 then begin 
;            p=plot( order_ys[ord_idx,*]  )
;            stop, 'check plot '
;       endif   
;       if debug gt 0 then begin
;         ;Once finish with swath plot 
;         
;          plotimage,i_swath, range=[ 0,255]
;         x_pixels= indgen(n_elements(debug_ys))
;         
;         p=plot( x_pixels,round(debug_ys) ,color='red', /overplot  )
;         
;         stop, 'Check Above '
;       endif

  endfor
 
 
  
;  if debug gt 0 then begin   
;    
    p=plot(order_ys[0, *] )    
    for ord_idx = 1L, n_elements(templates.middle )-1 do begin
        p=plot( order_ys[ord_idx, *] , /overplot )
        
    endfor
;  endif
;
; 
  
  stop, 'Got to the very  end '
  


  return, order_ys ; Order Coefficients
end
