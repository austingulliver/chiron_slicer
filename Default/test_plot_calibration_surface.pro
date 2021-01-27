;PLot the surface produced by the mapping of a given order number + a pixel to 
;the wavelength 


; UNDERSTAND THE POLYNOMIAL 


thidfile = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\thid\thidfile\custom_solution\wvc_slicer_171218.sav'
RESTORE, thidfile

init = wvc 



mkwave2,wave_offset_0,init ,pixel_offset=0


;graphic = SURFACE(wave_offset_0)

mkwave2,wave_offset_10,init ,pixel_offset=2
;
;s1= SURFACE(wave_offset_10 , COLOR='blue',/overplot)



;new_wave= wave_offset_0 + 1500
;s2= SURFACE(new_wave , COLOR='red',/overplot)

p0 = plot(wave_offset_0[*,73], LINESTYLE='-:', title='BLUE ORDER' )
p0 = plot(wave_offset_10[*,73], color='blue', /overplot )



END

