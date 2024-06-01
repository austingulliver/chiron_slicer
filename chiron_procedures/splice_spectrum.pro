;+
; :Description:
; 
; Options for Splice :
;  1) Cut at 3 200
;  2) Cut at intersection 
;  3) Cut as stated by ..\chiron\tous\mir7\utils\ch_deftrims.dat
;  
;  
;  pixel_to_cut : Number of pixels to cut if splice_type=pixel_cut
;-

function splice_spectrum, data_cube, position_artifact=position_artifact, splice_type=splice_type, redpar=redpar, pixel_to_cut = pixel_to_cut, maskArtifact=maskArtifact
  compile_opt idl2

size_cube= size(data_cube)
n_orders= size_cube[3]
n_wavelengths= size_cube[2]
threshold = 1000

artifact_indices =[646,764];[ 680,730]

if keyword_set(maskArtifact) then begin
   ; Identify the order where the mask is needed
   ; All blue orders(indexed orders after 63). Orders at this point are reversed so
   ; we need to count backwards.
   n_order_to_mask= n_orders-59 ; 63 is the number of red orders (orders that don't have the artifact)
   
   for order_idx = 0L, n_order_to_mask-1 do begin
        copy_data_cube = data_cube
        sub_section = data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx]
        sub_section=reform(sub_section)
        ; Mask with NaN before interpolate 
        copy_data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =!VALUES.F_NAN
        
        order=reform(copy_data_cube[1,*, order_idx])
        
        ; Interpolate
        to_interpolate= artifact_indices[0] + indgen( artifact_indices[1]- (artifact_indices[0] -1) )
        x=indgen(n_elements(copy_data_cube[1,*, order_idx]))
        result =interpol( order,x, to_interpolate, /NaN )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assign interpolated values
        ;copy_data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =result
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        sub_res = abs(result - sub_section)
        ;p=plot(data_cube[1,*, order_idx], color ="black")
        
        if order_idx eq 0 then begin
          max_local = max(sub_res, idx_max)
        endif else begin 
          search_range_idx = idx_max-6 + indgen(12)
          search_range = sub_res[search_range_idx]
          max_local = max(search_range, idx_max)
          idx_max = search_range_idx[idx_max]
        endelse 
        if max_local gt threshold then begin
              idx_sr=7
              search = 1
              prev_stddev = 0.0
              left_array = []
              ;Search Left
              while search do begin
                curr_idx = idx_max-idx_sr
                ser_array= sub_section[curr_idx:idx_max]
                curr_stddev = stddev(ser_array)
                substraction =abs(prev_stddev - curr_stddev)
                temp =[prev_stddev,curr_stddev]
                maximum = max(temp)
                inner_thr = substraction/maximum
                poss_array = curr_idx + indgen( idx_max- (curr_idx -1) )
                ;print, "***********************"
                ;print, inner_thr
                ;print,  poss_array
                ;print, "***********************" 
                if inner_thr ge 0.6 then begin
                  prev_stddev = curr_stddev
                endif else begin
                  left_array = poss_array
                  search=0
                endelse
                 idx_sr+=7
              endwhile
              idx_sr=7
              search = 1
              prev_stddev = 0.0
              right_array=[]
              ;Search Right
              while search do begin
                curr_idx = idx_max+idx_sr
                ser_array= sub_section[idx_max:curr_idx]
                curr_stddev = stddev(ser_array)
                substraction =abs(prev_stddev - curr_stddev)
                temp =[prev_stddev,curr_stddev]
                maximum = max(temp)
                inner_thr = substraction/maximum
                poss_array = idx_max+1 + indgen( curr_idx- (idx_max -1) )
                ;print, "***********************"
                ;print, inner_thr
                ;print,  poss_array
                ;print, "***********************"
                if inner_thr ge 0.6 then begin
                  prev_stddev = curr_stddev
                endif else begin
                  right_array = poss_array
                  search=0
                endelse
                idx_sr+=7
              endwhile
              res_arr_raw = [left_array, right_array]
              res_arr = artifact_indices[0]+res_arr_raw
              position_artifact.add, [order_idx, min(res_arr), max(res_arr)]
              if redpar.set_artifact_vals eq 0 then begin
                data_cube[1,res_arr,order_idx] = 0L
              endif else begin
                subcopy_data_cube = data_cube
                subcopy_data_cube[1,res_arr, order_idx] =!VALUES.F_NAN
                subcopy_data_cube = reform(subcopy_data_cube[1,*, order_idx])
                order=reform(copy_data_cube[1,*, order_idx])
                data_cube[1,res_arr,order_idx] = interpol( subcopy_data_cube,x, res_arr, /NaN )
              endelse
            
              ;p=plot(data_cube[1,*, order_idx], color ="black")
              ;p=plot(res_arr, data_cube[1,res_arr,order_idx ], '+r', /overplot)              
        endif
   endfor
   
   
   if ~keyword_set(splice_type) then new_cube = data_cube

   
endif


; >> Separte according to splice_type by cases 

if keyword_set(splice_type)    then begin 
    print, '>> Spectrum gets spliced by  ' +string(splice_type)
    switch (splice_type) of

          'custom': begin 
            ;We maintain the cube shape but with lest points 
            IF ~KEYWORD_SET(pixel_to_cut) then pixel_to_cut=3200
            
            new_cube = MAKE_ARRAY(2, pixel_to_cut, n_orders, /double, VALUE = 0.0)
            
            ;Finding middle section of 3200 pixels
            diff= round((n_wavelengths  - pixel_to_cut ) /2.0 )
            
                    
            new_cube[0,*,*] = data_cube[0,diff:diff+pixel_to_cut-1, *]
            new_cube[1,*,*] = data_cube[1,diff:diff+pixel_to_cut-1, *]
                           
              break
          end
          'intersection_cut': begin
              stop, 'ERROR: not implemented yet'
              break
          end
          'deftrims_cut': begin     
             stop, 'ERROR: not implemented yet '
             break
          end
          else: begin
              stop, 'SPLICE_SPECTRUM: >> ERROR << The parameter passed for splice_type is not allowed .'
          end
          
    endswitch

endif
   return, new_cube


end
