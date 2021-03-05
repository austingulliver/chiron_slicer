


t='o'
if t eq 'm' then begin
  print, 'got t'
endif else if t eq 's' then begin
  print, 'got not t'
endif else print, ' else'

;Comparing ThAr files for 181103 and 171218


;getting WVC 
;wvc_path = "C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\thid\thidfile\sl181103_myWaveSol.sav"
;restore, wvc_path
;wvc_input=wvc

order=0 



; - - -- -  FILE 1 : extracted spectra  ThAr 171218  are 1003, 1144
;Directory 2017
dir_20 ='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003'



rdsk,sp,dir_20,1   

;rdsk2fits, filename=fitsname, data = flat
;rdsk, x, path1
;cube1=readfits(path1) ; 1432 x 4112
;restore, path1
;mkwave2,w_output1,wvc_input, pixel_offset=0; ,pixel_offset=-3


;p1=plot(w_output1,sp [*,order],  title ='ThAr-Comparison nights 171218.1003 - 201121.1180')
;p1=plot(sp [*,order],  title ='ThAr-Comparison nights 171218.1003 - 201121.1180')



; - - -- -  FILE 2   ThAr for 2018
dir_20_2= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\181103\achi181103.1003'
rdsk,sp2,dir_20_2,1  



;mkwave2,w_output2,wvc_input,pixel_offset=0

;p2=plot(w_output2, sp2[*,0],color='blue',linestyle=2,   /overplot)

;p2=plot( sp2[*,0],color='blue',linestyle=2,   /overplot)


;p1=plot(w_output2[*,order], sp2[*,order], color='blue', title ='ThAr-181103.1003', /overplot)









; Starts the smoothing and actual comparison 
;-------------------------------------------

spec_1 = sp[*,0]
spec_2 = sp2[*,0]

smooth_spec1= TS_SMOOTH(spec_1,10)
smooth_spec2= TS_SMOOTH(spec_2,10)



yvals = smooth_spec2
xvals = indgen( n_elements(yvals) )




;pdf = HISTOGRAM(yvals, BINSIZE=50 ,LOCATIONS=xbin)
;histo = PLOT(xbin, pdf, LAYOUT=[3,1,2] )


local_maxmin_index = local_max_finder(xvals, yvals) ; also the absolute pixel number 



; Just as another way for validating you want to make sure we found only one withi the range of 600 to 800. if more than 1 then get the highest 

ref_pixel=list()
for i=0, n_elements(local_maxmin_index) -1 do begin
  
  if (local_maxmin_index[i] gt 600) AND (local_maxmin_index[i] lt 800) then begin  ; 200 pixels confidence interval 
       ref_pixel.add, local_maxmin_index[i]   
  endif
  
 
endfor


print, 'Values after the filtering '
print, ref_pixel

num_ref_pixels= ref_pixel.Count()
if num_ref_pixels eq 0  then stop, "Somthing very bad happend look at plots"
if num_ref_pixels eq 1  then  out_pixel= ref_pixel[0] else begin
  ;Else pick the biggest 
  print, 'got else '
  pixels= ref_pixel.toarray()
  intensities = dblarr(n_elements(pixels))
  
  for p=0,  n_elements(pixels)-1 do intensities[p] = yvals[pixels[p]]
  
  print, pixels
  maxVal = max(intensities, maxIdx)
  out_pixel = pixels[maxIdx]  
endelse
  
; ref Pixel is the one getting compared 

x_extrema = xvals[out_pixel ]
y_extrema = yvals[out_pixel ]


p1= plot(xvals, yvals )
p2 = scatterplot(x_extrema, y_extrema, /current, /overplot, $
  symbol = 'o', sym_color = 'r', sym_thick = 2)
  
  
  
  
  
print, 'terminated'



p1=plot(smooth_spec1,  title ='Comparison')
p2=plot( smooth_spec2,color='blue',linestyle=2,   /overplot)






End