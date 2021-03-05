


flat= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\flats\chi210208.slicer.sum.fits'

iflat= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\210208\chi210208.1040.fits'   ; 1040

file= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\210208\chi210208.1121.fits'

file17= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\171218\chi171218.1145.fits'




imFlat= readfits(iflat)
imSpec= readfits(file)

im = imSpec /  imFlat

;im=rotate(im,1)
print,size(im)  ; 1432 * 4112


y1=900
y2=950
deltay=y2-y1

X1=0.0;4112.0/4.0
X2=4111.0;4112.0/2.0


imRaw=im[   y1:y2, x1:x2]
;im=im[x1:x2, y1:y2]   ; Red Order    4112 x 1366
;im=im[450:490,*]   ; Middle Order
;im=im[1000:1431,0:689]  ; Plotting Blue Orders AFTER ARTIFACT
;im=im[1200:1250,727:*]  ; Plotting Blue Orders BEFORE ARTIFACT

sz=size(im)
print, 'this is what matters'
print, sz

d1=sz[1]   ;  4112 
d2=sz[2]   ;  1432

;superd1=indgen(d1)
;superd2=indgen(d2)


;x=lonarr(d1*d2)
;y=lonarr(d1*d2)
;z=dblarr(d1*d2)
;
;counter=0L
;
;for i=0, deltay do begin
;
;  for j=0,d2-1 do begin
;    x[counter] = i
;    y[counter] =j
;    z[counter] = im[i,j]
;    counter=counter+1L
;
;  endfor
;endfor
;


print,size(z)
print,size(x)
print,size(y)



s = SURFACE(imRaw, TEXTURE_IMAGE=image, YSTYLE=1)
;s = SURFACE(z,x,y, COLOR='burlywood', BOTTOM_COLOR='r',DEPTH_CUE=[0,1], THICK=10 ) ; burlywood dark orange , dark slate gray

;s1= SURFACE(z,x,y,STYLE=0, COLOR='blue',THICK=6,/overplot)
;s2= SURFACE(z,x,y,STYLE=0, COLOR='BLACK',THICK=3, /overplot)  ;; perfect for all pixels
;
;
;dist_size = SCATTERPLOT3D( x,y,  z , SYM_COLOR='red', /SYM_FILLED, overplot=1);SYM_OBJECT=ORB(), /SYM_FILLED )
;p = PLOT3D(x, y, z, symbol=3, LINESTYLE=6)
d=0

if d eq 1 then begin


  sz=size(im)
  d1=sz[1]   ; 4112
  d2=sz[2]

  im=im[0:11,*]

  print,size(im)

  print, 12*4112

  z=reform(im, 17184)

  d1=indgen(d1)
  super_d1=[d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1,d1]


  zeros= INTARR(4112)



  super_d2= [zeros, zeros+1, zeros+2, zeros+3, zeros+4, zeros+5, zeros+6,zeros+7, zeros+8, zeros+9, zeros+10,zeros+11 ]

  print,size(z)
  print,size(super_d1)
  print,size(super_d2)


  p = PLOT3D(super_d1,super_d2,z)

endif





END