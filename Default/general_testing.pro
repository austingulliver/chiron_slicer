


; plot all disctibution for sigma clipping :


;nums=indgen(1170-1159+1) + 1159
;file_names='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\no_cr_remove\wchi210526.'+strt(nums)+'.fits'
;
;
;foreach file, file_names do begin
;  y=readfits(file)
;  p=plot(y[1,*,*] ,/overplot)
;
;endforeach


; befor e



with_cr = readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\no_cr_remove\wchi210526.1159.fits')
yales = readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\yales\wchi210526.1159.fits')
sigma= readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\sigma_clipping_3\wchi210526.1159.fits')
la_cosmic= readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\la_cosmic\wchi210526.1159.fits')

order= 25

;p=plot(with_cr[1,*,25])

; no cr VS legacy 
p=plot(with_cr[0,*,order], with_cr[1,*,order],name='original',xtitle='Wavelength ', ytitle='Intensity', title=' Legacy - 210526- 25th Blue order')

p1=plot(yales[0,*,order], yales[1,*,order],'r--2' ,name='legacy - sigma = 5' , /overplot)

!null = legend(target=[p,p1])

; no cr VS la cosmic
p=plot(with_cr[0,*,order], with_cr[1,*,order],'2',name='original',xtitle='Wavelength ', ytitle='Intensity', title=' La Cosmic - 210526 - 25th Blue order')

p1=plot(la_cosmic[0,*,order], la_cosmic[1,*,order],'b--2' ,name='La Cosmic - sigma = 21' , /overplot)

!null = legend(target=[p,p1])


; no cr VS sigma clipping 
p=plot(with_cr[0,*,order], with_cr[1,*,order],'2',name='original',xtitle='Wavelength ', ytitle='Intensity', title=' Sigma Clipping - 210526 - 25th Blue order')

p1=plot(sigma[0,*,order], sigma[1,*,order],'g--2' ,name='Sigma Clipping = 3' , /overplot)

!null = legend(target=[p,p1])





end 


