;+
; :Description:
; 
; Options for Splice :
;  1) Cut at 3 200
;  2) Cut at intersection 
;  3) Cut as stated by ..\chiron\tous\mir7\utils\ch_deftrims.dat
;  
;
;-

pro splice_spectrum, night , obs_num, splice_type, redpar=redpar 
  compile_opt idl2

; >> Restore file




; >> Separte according to splice_type by cases 

print, '>> Spectrum gets spliced by  ' +string(splice_type)
switch (splice_type) of

    
      '3200_cut': begin
          
          break
      end
      'intersection_cut': begin
          
          break
      end
      'deftrims_cut': begin
        
        
        
        
         break
      end
      else: begin
          stop, 'SPLICE_SPECTRUM: >> ERROR << The parameter passed for splice_type is not allowed .'
      end
      
endswitch





end
