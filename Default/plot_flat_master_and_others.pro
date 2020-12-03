;Plot Flat file + others 
;



flatdir= 'C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\fitspec\181103\achi181103.1164.fits'


flatCube=readfits(flatdir)

;For Flat :
;Expected size of cube => 
;flat[*,*,0] = tmp     ; Flat Spectrum /Smoothed
;flat[*,*,1] = sp      ; Flat Spectrum
;flat[*,*,2] = smflt   ; Smoothed Spectrum

cubeSize= size(flatCube)

print, 'Number of orders found: ' +string(cubeSize[1])
selectedFlat=flatCube[*,*,2]

order= 0
xPixel= findgen(4112) 


;title= 'Order '+string(order)
;p1=plot(xPixel,selectedFlat[*,order], color='Black',thick=1,TITLE=title ,XTITLE='Wavelength', YTITLE='Intensity' )



;All Order: Full Spectrum 
invertedSpec = reverse(selectedFlat[*,*],1)
invertedSpec = reverse(invertedSpec,2)

p1=plot(invertedSpec, color='Black',thick=1,TITLE=' SMOOTHED Master Flat Spectrum' ,XTITLE='Pixel ', YTITLE='Intensity' )

name= 'C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\files_sent\master_flat\master_flat_spec_smoothed\smooth_mstr_flat_spec.fits'
wdsk, invertedSpec, name, /new


;Plotting alternating color orders
;for ordIdx=0,  72 do begin

;  if ordIdx MOD 2 eq 0 then begin
;   color='Red'
;  thick= 1
;endif else begin

; color= 'Black'
; thick=3
;endelse

;p1=plot(xPixel,selectedFlat[*,order], color=color, thick=thick, /overplot )


;endfor


end