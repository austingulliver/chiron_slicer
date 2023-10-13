
function find_noise_level, spectrum 
    ; It finds the noise level assuming the noise is more-less constant at a given X value. Returns
    ; a range (two x values) of where to expect that noise.
    ; Spectrum given as intensity vs  pixel 
    
    len= n_elements(spectrum) -1
    noise_peaks = list()

    noise_vallies = list()
    for i=1, n_elements(spectrum)-2 do begin
       
      if (  (spectrum[i-1] le spectrum[i]) AND (spectrum[i] ge spectrum[i+1])  ) then   noise_peaks.add, spectrum[i]
      if (  (spectrum[i-1] ge spectrum[i]) AND (spectrum[i] le spectrum[i+1])  ) then   noise_vallies.add, spectrum[i]
     
    endfor
    
      noise_peaks= noise_peaks.toarray()
      noise_vallies = noise_vallies.toarray()
      
      y0 = MEDIAN(noise_vallies, /DOUBLE)
      y1 = MEDIAN(noise_peaks, /DOUBLE)
      
      
      ;Plotting (Testing purposes)
      
;      p0= plot(spectrum)
;      px0 = plot([0,len],[y0, y0], color='blue',thick =3, /overplot)
;      px0 = plot([0,len],[y1, y1], color='blue', thick=3, /overplot)
;    
    RETURN, [y0, y1]
END


function find_spec_peak, datax, datay,minima = minima
  ; adapted for emission spectra

  ;set compile options
  compile_opt idl2


  ;initialize list
  max_points = list()

  data_x = datax
  data_y = datay
  
 ; p1=(data_y)

  noise_level =find_noise_level(datay);median(data_y)
  above_noise =7.0 ; The number of times the line has to be above noise level 
  
  max_peak = max(data_y)
  delta_y = (1.0/100.0 ) *(max_peak - noise_level)



  ;check for keyword, flip the sign of the y values
  if keyword_set(minima) then data_y = -datay

  ;iterate through elements
  ;Check for 5 points

  for i=1, n_elements(data_y)-2 do begin
    ;previous point less than i-th point and next point less than i-th point
    if (  (data_y[i-1] le data_y[i]) AND (data_y[i] ge data_y[i+1])  ) then  begin
      
      
      IF data_y[i] gt noise_level[1] + (( noise_level[1]- noise_level[0] )*above_noise )  then   max_points.add, i
    
      
      
    endif
  endfor

  ;return an array of the indices where the extrema occur
  return, max_points.toarray()

end



FUNCTION validate_one_peak, pixel_list, spec

  num_ref_pixels= pixel_list.Count()

  if num_ref_pixels eq 0  then stop, " ERROR : No peak was found within a range. Look into find_ref_peak.pro "
  if num_ref_pixels eq 1  then  out_pixel= pixel_list[0] else begin
    ;If more than 1 then get the highest. (The algorithm probably picked some other peak(s) down the road )

    pixels= pixel_list.toarray()
    intensities = dblarr(n_elements(pixels))
    for p=0,  n_elements(pixels)-1 do intensities[p] = spec[pixels[p]]


    maxVal = max(intensities, maxIdx)
    out_pixel = pixels[maxIdx]
  endelse
  
  
  RETURN, out_pixel


END


FUNCTION find_ref_peak ,abs_path, redpar=redpar,  order_num=order_num
;+              
; :Input:
; 
; :Output:
; 
; :Summary:
;  Opens the achi files input (is not a fits a file, file contains spectrum in pixels),
;  ,applies and smoothing algorithim (moving avg of 3),  
;  
;  Ajusted to read the indexed order 70 (Blue Order)
;
;-

if not keyword_set(order_num) then order_num =  1

  ;Read file & smooth
  ;--------------------------------------------  
  rdsk,sp,abs_path,1                       ; File restored is the the one extracted where the orders still go 

  ;smooth_spec= TS_SMOOTH(spec,3)          ; Moving avg only to smooth seems to influece more 
  
  
  spec = REFORM(sp[*, order_num] )  ; If software crashes at this point. It is most likely because the order changed due to 
   
  p0 = plot(spec, LINESTYLE='-:', /OVERPLOT, title ='ThAr -Indexed Order  (Blue order) ' ); ,/overplot)  



  ;Find peaks
  ;--------------------------------------------
  ;yvals = smooth_spec 
  yvals =spec 
  xvals = indgen( n_elements(yvals) )
  local_maxmin_index = find_spec_peak(xvals, yvals) ; expect back an array with the pixel number where peaks in the spectrum where found 
                                                    ; The peaks were found under certain criteria 
  

  ;Filter returned peaks
  ;--------------------------------------------

  ; For consistency sake we only take the 3 greatest peaks wich we experimentally found and 
  ; should always be withint a pixeles range of : 
  ;                                               r1=[2125,2174]  
  ;                                               r2=[2240,2270]
  ;                                               r3=[2560,2590]
  ;                                               r4=[3160,3195]
  ;                                               r5=[3340,3380]
  
  r1=[2125,2174]         ; rough approximations
  r2=[2240,2270]
  r3=[2560,2590]
  r4=[3160,3195]
  r5=[3340,3380]
  
  ref_pixel_1=list()
  ref_pixel_2=list()
  ref_pixel_3=list()
  ref_pixel_4 =list()
  ref_pixel_5=list()
  for i=0, n_elements(local_maxmin_index) -1 do begin
    
    ;pl=scatterplot(local_maxmin_index[i] , spec [local_maxmin_index[i]] , /current, /over,   symbol = 'o', sym_color = 'b', sym_thick = 2)   
    
    IF (local_maxmin_index[i] gt r1[0]) AND (local_maxmin_index[i] lt r1[1]) THEN BEGIN      
        ref_pixel_1.add, local_maxmin_index[i]  
    ENDIF 
    
    IF  (local_maxmin_index[i] gt r2[0]) AND (local_maxmin_index[i] LT r2[1] ) THEN  BEGIN
        ref_pixel_2.add, local_maxmin_index[i]     
    ENDIF 
    
    IF (local_maxmin_index[i] gt r3[0]) AND (local_maxmin_index[i] LT r3[1]) THEN BEGIN
        ref_pixel_3.add, local_maxmin_index[i]     
    ENDIF
    
    IF (local_maxmin_index[i] gt r4[0]) AND (local_maxmin_index[i] LT r4[1]) THEN BEGIN
      ref_pixel_4.add, local_maxmin_index[i]
    ENDIF
    
    IF (local_maxmin_index[i] gt r5[0]) AND (local_maxmin_index[i] LT r5[1]) THEN BEGIN
      ref_pixel_5.add, local_maxmin_index[i]
    ENDIF
    
  ENDFOR

  
  ;Define 1 peak per range (Validation)
  ;--------------------------------------------  
  peak_1=validate_one_peak(ref_pixel_1,spec )
  peak_2=validate_one_peak(ref_pixel_2,spec )
  peak_3=validate_one_peak(ref_pixel_3,spec )
  peak_4=validate_one_peak(ref_pixel_4,spec )
  peak_5=validate_one_peak(ref_pixel_5,spec )
  
  
  ;Gaussian fitting 
  ;--------------------------------------------
  
  ;We fit a gaussian to obtain a better approximation of the peak shift
  
  
  y_intensities = spec[peak_1-20: peak_1+20]  ; Width of the window is defined emperically 
  relative_pixels= indgen(n_elements(y_intensities)) -20
  y_fit_1= gaussfit(relative_pixels, y_intensities, out_coefficients_1, NTERMS=4 ) ;, ESTIMATES=guess )
  
  y_intensities = spec[peak_2-15: peak_2+15]
  relative_pixels= indgen(n_elements(y_intensities)) -15 
  y_fit_2= gaussfit(relative_pixels, y_intensities, out_coefficients_2, NTERMS=4 )
  
  y_intensities = spec[peak_3-15: peak_3+15]
  relative_pixels= indgen(n_elements(y_intensities)) -15
  y_fit_3= gaussfit(relative_pixels, y_intensities, out_coefficients_3, NTERMS=4 )
  
  y_intensities = spec[peak_4-15: peak_4+15]
  relative_pixels= indgen(n_elements(y_intensities)) -15
  y_fit_4= gaussfit(relative_pixels, y_intensities, out_coefficients_4, NTERMS=4 )
  
  y_intensities = spec[peak_5-15: peak_5+15]
  relative_pixels= indgen(n_elements(y_intensities)) -15
  y_fit_5= gaussfit(relative_pixels, y_intensities, out_coefficients_5, NTERMS=4 )
  
 
 

 
; if ps eq 1  then  p0 = plot(spec, LINESTYLE='-:' , THICK=2, /overplot) else p0 = plot(spec, LINESTYLE='-:' ,/overplot)
; All of these plot are for testing purposes
; p0 = plot(spec, LINESTYLE='-:' )
; pixels= indgen(51) + (peak_1-20)
; pa = plot( pixels, y_fit_1 , /overplot, color = 'r', thick =1)  ;  symbol = 'o'
; 
; pixels= indgen(31) + (peak_2-15 )
; pb = plot( pixels, y_fit_2 , /overplot, color = 'r', thick = 1) ;  symbol = 'o',
; 
; pixels= indgen(31) + (peak_3-15 )
; pc = plot( pixels, y_fit_3 , /overplot , color = 'r', thick = 1) ; symbol = 'o',
; 
; pixels= indgen(31) + (peak_4-15 )
; pc = plot( pixels, y_fit_4 , /overplot , color = 'r', thick = 1) ; symbol = 'o',
; 
; pixels= indgen(31) + (peak_5-15 )
; pc = plot( pixels, y_fit_5 , /overplot , color = 'r', thick = 1) ; symbol = 'o',


 abs_pixel_a = peak_1 + out_coefficients_1[1]
 abs_pixel_b = peak_2+ out_coefficients_2[1]
 abs_pixel_c = peak_3 + out_coefficients_3[1]
 abs_pixel_d = peak_4 + out_coefficients_4[1]
 abs_pixel_e = peak_5 + out_coefficients_5[1]
 
 pa = SCATTERPLOT( abs_pixel_a, spec[abs_pixel_a] , /OVERPLOT,   symbol = 'o', sym_color = 'r', sym_thick = 1)
 pb = SCATTERPLOT( abs_pixel_b, spec[abs_pixel_b] , /OVERPLOT,   symbol = 'o', sym_color = 'r', sym_thick = 1) 
 pc = SCATTERPLOT( abs_pixel_c, spec[abs_pixel_c] , /OVERPLOT,   symbol = 'o', sym_color = 'r', sym_thick = 1)
 pd = SCATTERPLOT( abs_pixel_d, spec[abs_pixel_c] , /OVERPLOT,   symbol = 'o', sym_color = 'r', sym_thick = 1)
 pe = SCATTERPLOT( abs_pixel_e, spec[abs_pixel_c] , /OVERPLOT,   symbol = 'o', sym_color = 'r', sym_thick = 1)




RETURN, [abs_pixel_a,abs_pixel_b, abs_pixel_c, abs_pixel_d, abs_pixel_e ] ; Returns 3 pixels which are meant to be compared with another spectrum and 
                                                ; 
  
END



;Sample Code for testing
;;---------------------------------- 
;
;night= 171117
;;t= 'Comparison between 171218 and ' + strtrim(string(night))
;;p = plot([0], [0], title=t) 
;dir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003' ; Used as Reference 
;ref_pixel_2017= find_ref_peak(dir ) ; ,p=1) 
;print, ref_pixel_2017

;
;comparedDir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171117\achi171117.1003' ; Used as Reference
;current_ref_pixel= find_ref_peak(comparedDir, p=0)
;
;
;pixel_offset =  mean( [ref_pixel_2017[0]-current_ref_pixel [0] , ref_pixel_2017[1]-current_ref_pixel [1], ref_pixel_2017[2]-current_ref_pixel [2]  ] ) ; 
;
;print, ' The found peaks of 2017 are  : '
;print, ref_pixel_2017
;print, ' The found peaks of' + strtrim(string(night)) + 'are  : '
;print, current_ref_pixel
;print, 'The avg pixel shift is : ' + strtrim(string(pixel_offset))

;

; Sample Code for thing
;-----------------------------------
;abs_path = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003'
;rdsk,sp,abs_path,1
;spec = reform(sp[*,70])
;answer = find_noise_level(spec)
;spec = sp[*,60]
;smooth_spec= TS_SMOOTH(spec,100)
;p = PLOT(spec ,LINESTYLE='-:',  TITLE='Red Order 60')
;p = PLOT(smooth_spec ,color='blue', TITLE='Red Order 60', /overplot)


;END 
