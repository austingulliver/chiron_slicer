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
function define_templates, flat, order_num, redpar

      print, 'OPTIMAL_ORDER_TRACING: Defining templates to be used for cross-correlation. Please Wait ....'
      debug = 2 ; Make it greater than 1 for detailed plotting
      
      y_peaks=all_order_peaks(flat, order_num, redpar, method_str='from_scratch')  ; Finds the all peaks of each order. Note it will return the 76 orders but
      ; we will get rid of some of them
      ; Need to iterate for every thee
    
      if debug  then p=plot(y_peaks, title='These are all the peaks found', xtitle='Indices of peaks', ytitle='indices of peaks wrt column')
      img_size=size(flat)     ;size of image passed: e.g. Master Flat ; Image is 4112 x 1366 for the slicer mode
      n_columns=img_size[1]   ;number of cols in image E.g. 4112
      n_rows= img_size[2]     ;number of rows in image E.g 1366
    
      ;middle_column = round( n_columns/2.0 )
    
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
        ;           Check on maximum sepoaration of peaks especially for last order 73
        if pixel_diff gt redpar.xwids[redpar.mode] then begin
          print, " ****************************************** "
          print, 'Beware: for order ',+strt(index), ' pixel diff is ', pixel_diff
          print, " ****************************************** "
          ;               print, +strt(index), pixel_diff
          ;               if pixel_diff gt redpar.xwids[redpar.mode] then stop, ' >> Error <<  The pixel diff cannot be greater than 12 pixel (For the slicer mode ). The peaks are supposed to be within the 12 pixels '
        endif
        missing_px_num = redpar.xwids[redpar.mode]  -  pixel_diff   ; plus+ to account for substraction
    
        if missing_px_num mod 2 eq 0 then begin ; if even
          low_bound = low_bound - round(missing_px_num/2)
          up_bound = up_bound + round(missing_px_num/2)
        endif else  begin ; is odd for some reason
          missing_px_num =missing_px_num-1 ; to make it even
          low_bound = low_bound - round(missing_px_num/2)
          up_bound = up_bound + round(missing_px_num/2)
    
          ; >> We add the +1 pixel to complete the 12 pixels based on which pixel either lower or upper is higher (this translates to if the pixel belongs to the order or  not )
          if flat[order_num, low_bound-1  ] gt flat[order_num,  up_bound+1  ] then begin
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
          sum_no_shift = total( reform( flat[order_num,  low_bound: up_bound]   ) )
          sum_minus    = total( reform( flat[order_num,  potential_minus_low_bound : potential_minus_up_bound] ) )
          sum_plus     = total( reform( flat[order_num,  potential_plus_low_bound : potential_plus_up_bound]   ) )
    
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
    
        p=plot(flat[order_num, *  ])
        ; Now plot the choosen bounds one by one
        min_flux = min(reform(flat[order_num, *  ]) )
        max_flux =max( reform(flat[order_num, *  ]) )
    
        for i= 0 , 72 do begin
    
          p=plot( [ order_templates.down[i] , order_templates.down[i] ], [min_flux, max_flux], color = 'red' , /overplot)
          p=plot( [ order_templates.up[i]   , order_templates.up[i]   ], [min_flux, max_flux], color = 'red' , /overplot)
    
        endfor
    
      endif
      
      return, order_templates
end