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



function all_order_peaks, flat, order_num, redpar, method_str= method_str
  compile_opt idl2

        debug=0
        
        ;; USE THIS SECTION TO MANUALLY ADD OR REMOVE PEAKS. Refer to the plot by setting debug =1 
        ; DON'T FORGET TO COME BACK HERE AND LEAVE THEM EMPTY ONCE IS
        
        ; Image is  4112X 1432 for the slicer mode
        img_size=size(flat)     ;size of image passed: e.g. Master Flat
        n_rows= img_size[1]     ;number of rows in image
        n_columns=img_size[2]   ;number of cols in image
          
          
        ;if method_str eq 'from_scratch' then begin      
        above_noise_level=5.0 ; Defines how many times bgger(wrt to the Background) the intensity values should get picked
                               ; Found experimentally. Change if neccesary 
                              
        middle = flat[order_num, *]
        middle= reform(middle)
        
        noise_level = background_level(middle, noise_width=redpar.ron, section_width=redpar.xwids[redpar.mode] ,  noise_str='band_fragments', debug=debug) ; Has the same with as readout noise
                      ; ron(input) : Read out noise of the frame                                    
                      ; section_width(input) : Used to define the width of the  24.0
                      ; now noise level(output) is a vector with the same length as the middle vector
        
       
            
        idx_peaks = find_peaks(middle, noise_level ,read_out_noise=redpar.ron, above_noise=above_noise_level) 
        
      
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
                    
                    ; Try with X proximity to eliminate any non peak value
                    selected_idx = idx_peaks[indices_of_idx]
                    diff_indices= INTARR(n_elements(selected_idx)-1) ; empt yarray   
                    for index = 0L, n_elements(selected_idx)-2 do begin
                      diff_indices[index]  = selected_idx[index+1] - selected_idx[index]
                    endfor      
                    
                    ; need to know which indices to include based on how close they are
                    sorted_diff_indices =  sort( diff_indices)   ; ascending. I am only interested in 2 diff (the very first 2 once sorted )    
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
                    
                    ;Try with Y proximity if X did not work.
                    if n_elements(peaks_group) gt 3 then begin 
                      curr_peaks_gp =  peaks_group.toarray()
                      y_vals_curr_peaks_gp = middle[curr_peaks_gp]
                      idx_y_vals_curr_peaks_gp = sort( y_vals_curr_peaks_gp)
                      p_sel = [0,0,0]
                      curr_stdev = 9999e9
                      p_win = 0
                      while p_win+2 ne n_elements(peaks_group) do begin
                        temp_sel = y_vals_curr_peaks_gp[idx_y_vals_curr_peaks_gp[p_win:p_win+2] ] 
                        inner_stdev = stddev(temp_sel)
                        if inner_stdev lt curr_stdev then begin
                          p_sel = idx_y_vals_curr_peaks_gp[p_win:p_win+2]
                        endif
                        p_win+=1
                      endwhile
                      curr_peaks_gp = curr_peaks_gp[p_sel]
                      peaks_group = curr_peaks_gp.tolist()
                    endif 
                    
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
              peaks_group_array = peaks_group.toarray()
              fixed_peak_indices[every_3_ref-1:every_3_ref+1] = peaks_group_array
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
        endelse  
end