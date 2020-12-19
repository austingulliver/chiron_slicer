  

output = FILE_TEST("C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\c15e_160104_104849_dri.fits.fz")


PRINT, ' FILE EXIST? ' + STRING(OUTPUT)

;x= readfits("C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\c15e_160104_104849_dri.fits")
;print, size(x)

flat = MRDFITS("C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\c15e_160104_104849_dri.fits.fz", /FPACK )

;print, flat

print, size(flat)

end