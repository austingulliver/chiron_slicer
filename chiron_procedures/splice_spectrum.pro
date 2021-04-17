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

function splice_spectrum, data_cube, splice_type, redpar=redpar, pixel_to_cut = pixel_to_cut
  compile_opt idl2

size_cube= size(data_cube)
n_orders= size_cube[3]
n_wavelengths= size_cube[2]




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
