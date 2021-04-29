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

function splice_spectrum, data_cube, splice_type, redpar=redpar, pixel_to_cut = pixel_to_cut, maskArtifact=maskArtifact
  compile_opt idl2

size_cube= size(data_cube)
n_orders= size_cube[3]
n_wavelengths= size_cube[2]


artifact_indices =[ 680,730]

if keyword_set(maskArtifact) then begin
   ; Identify the order where the mask is needed
   ; All blue orders(indexed orders after 63). Orders at this point are reversed so
   ; we need to count backwards.
   n_order_to_mask= n_orders-63 ; 63 is the number of red orders (orders that don't have the artifact)
   
   for order_idx = 0L, n_order_to_mask-1 do begin
        
        ; Mask with NaN before interpolate 
        data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =!VALUES.F_NAN
        
        order=reform(data_cube[1,*, order_idx])
        
        ; Interpolate
        to_interpolate= artifact_indices[0] + indgen( artifact_indices[1]- (artifact_indices[0] -1) )
        x=indgen(n_elements(data_cube[1,*, order_idx]))
        result =interpol( order,x, to_interpolate, /NaN )
        
        ; Assign interpolated values 
        data_cube[1,artifact_indices[0]:artifact_indices[1], order_idx] =result 
    

   endfor

   
endif


; >> Separte according to splice_type by cases 
    
    print, '>> Spectrum gets spliced by  ' +string(splice_type)
    switch (splice_type) of

          'pixel_cut_of_3200px': begin 
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


   return, new_cube


end
