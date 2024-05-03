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

function cross_correlate_op, i_swath,i_template , column_i,  scipy_signal, prev_local_row=prev_local_row, idx_order=idx_order, debug=debug
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