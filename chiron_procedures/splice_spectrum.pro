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

function splice_spectrum, data_cube, splice_type=splice_type, redpar=redpar, pixel_to_cut = pixel_to_cut, maskArtifact=maskArtifact
  compile_opt idl2

size_cube= size(data_cube)
n_orders= size_cube[3]
n_wavelengths= size_cube[2]


artifact_indices =[646,764];[ 680,730]

if keyword_set(maskArtifact) then begin
   ; Identify the order where the mask is needed
   ; All blue orders(indexed orders after 63). Orders at this point are reversed so
   ; we need to count backwards.
   n_order_to_mask= n_orders-60 ; 63 is the number of red orders (orders that don't have the artifact)
   
   for order_idx = 0L, n_order_to_mask-1 do begin
      copy_data_cube = data_cube
       
       ;sub_dataset_y = data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx]
       ;sub_dataset_y= reform(sub_dataset_y)
       ;sub_dataset_x = data_cube[0,artifact_indices[0]:artifact_indices[1], order_idx]
       ;sub_dataset_x= reform(sub_dataset_x)
       ;to_interpolate= artifact_indices[0] + indgen( artifact_indices[1]- (artifact_indices[0] -1) )
       
       ;ncoeff_3 = POLY_FIT( to_interpolate, sub_dataset_y, 1, /double,  status=status)
       ;fit_y_3= poly(to_interpolate,ncoeff_3)
       ;wp=plot(to_interpolate, fit_y_3, color="blue")
       ;wp=plot(to_interpolate, sub_dataset_y, color="green", /overplot)
       
       
       
       
       
        
        ; Mask with NaN before interpolate 
        data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =!VALUES.F_NAN
        
        order=reform(data_cube[1,*, order_idx])
        
        ; Interpolate
        to_interpolate= artifact_indices[0] + indgen( artifact_indices[1]- (artifact_indices[0] -1) )
        x=indgen(n_elements(data_cube[1,*, order_idx]))
        result =interpol( order,x, to_interpolate, /NaN )
        
        wp=plot(copy_data_cube[0,artifact_indices[0]:artifact_indices[1], order_idx], copy_data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx], color="blue")
        wp=plot(copy_data_cube[0,artifact_indices[0]:artifact_indices[1], order_idx], result, color="green", /overplot)
        
        ; Assign interpolated values 
        data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =result 

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
