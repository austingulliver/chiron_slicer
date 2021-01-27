function find_spec_peak, datax, datay, $
  ; adapted for emission spectra

  ;keyword for minimum /minima
  minima = minima
  ;set compile options
  compile_opt idl2


  ;initialize list
  max_points = list()

  data_x = datax
  data_y = datay
 ; p1=(data_y)

  noise_level =median(data_y)
  max_peak = max(data_y)
  delta_y = (1.0/100.0 ) *(max_peak - noise_level)



  ;check for keyword, flip the sign of the y values
  if keyword_set(minima) then data_y = -datay

  ;iterate through elements
  ;Check for 5 points

  for i=1, n_elements(data_y)-2 do begin
    ;previous point less than i-th point and next point less than i-th point
    if (  (data_y[i-1] lt data_y[i]) AND (data_y[i] gt data_y[i+1])  ) then  begin

      if data_y[i] ge noise_level + delta_y then   max_points.add, i
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


FUNCTION find_ref_peak ,abs_path
;+
; :Input:
; 
; :Output:
; 
; :Summary:
;  Opens the achi files input (is not a fits a file, file contains spectrum in pixels),
;  ,applies and smoothing algorithim (moving avg of 3),  
;
;-



  ;Read file & smooth
  ;--------------------------------------------  
  rdsk,sp,abs_path,1
  spec = REFORM(sp[*,0] )                 ; Get the first order only since we only need a reference 
  smooth_spec= TS_SMOOTH(spec,3)          ; Moving avg only to smooth seems to influece more 



  ;Find peaks
  ;--------------------------------------------
  yvals = smooth_spec 
  xvals = indgen( n_elements(yvals) )
  local_maxmin_index = find_spec_peak(xvals, yvals) ; expect back an array with the pixel number where peaks in the spectrum where found 
                                                    ; The peaks were found under certain criteria 
  

  ;Filter returned peaks
  ;--------------------------------------------

  ; Fo consistency sake we only take the 3 greatest peaks wich we experimentally found and 
  ; should always be withint a pixeles range of : 
  ;                                              r1=[ 600, 800 ]  
  ;                                              r2=[1975,2050]
  ;                                              r3=[3820,3920]
  ;p0 = (spec, /overplot )
  r1=[600,800]         ; rough approximations
  r2=[1975,2050]
  r3=[3820,3920]
  ref_pixel_1=list()
  ref_pixel_2=list()
  ref_pixel_3=list()
  for i=0, n_elements(local_maxmin_index) -1 do begin
    
    ;pl=scatter(local_maxmin_index[i] , spec [local_maxmin_index[i]] , /current, /over,   symbol = 'o', sym_color = 'b', sym_thick = 2)   
    
    IF (local_maxmin_index[i] gt 600) AND (local_maxmin_index[i] lt 800) THEN BEGIN  ; 200 pixels confidence interval
       
        ref_pixel_1.add, local_maxmin_index[i]  
    ENDIF 
    
    IF  (local_maxmin_index[i] gt r2[0]) AND (local_maxmin_index[i] LT r2[1] ) THEN  BEGIN
        ref_pixel_2.add, local_maxmin_index[i]     
    ENDIF 
    
    IF (local_maxmin_index[i] gt r3[0]) AND (local_maxmin_index[i] LT r3[1]) THEN BEGIN
        ref_pixel_3.add, local_maxmin_index[i]     
    ENDIF
    
  ENDFOR

  
  ;Define 1 peak per range (Validation)
  ;--------------------------------------------  
  peak_1=validate_one_peak(ref_pixel_1,spec )
  peak_2=validate_one_peak(ref_pixel_2,spec )
  peak_3=validate_one_peak(ref_pixel_3,spec )
  
  
  ;Gaussian fitting 
  ;--------------------------------------------
  
  ;We fit a gaussian to obtain a better approximation of the peak shift
  
  
  y_intensities = spec[peak_1-20: peak_1+20] 
  relative_pixels= indgen(n_elements(y_intensities)) -20  
  y_fit_1= gaussfit(relative_pixels, y_intensities, out_coefficients_1, NTERMS=3 ) ;, ESTIMATES=guess )
  
  y_intensities = spec[peak_2-20: peak_2+20]
  relative_pixels= indgen(n_elements(y_intensities)) -20   
  y_fit_2= gaussfit(relative_pixels, y_intensities, out_coefficients_2, NTERMS=3 )
  
  y_intensities = spec[peak_3-20: peak_3+20]
  relative_pixels= indgen(n_elements(y_intensities)) -20
  y_fit_3= gaussfit(relative_pixels, y_intensities, out_coefficients_3, NTERMS=3 )
  
 
 

 
; if ps eq 1  then  p0 = plot(spec, LINESTYLE='-:' , THICK=2, /overplot) else p0 = plot(spec, LINESTYLE='-:' ,/overplot)
; pixels= indgen(41) + (peak_1-20 )
; pa = plot( pixels, y_fit_1 , /overplot, color = 'r', thick =1)  ;  symbol = 'o'
; 
; pixels= indgen(41) + (peak_2-20 )
; pb = plot( pixels, y_fit_2 , /overplot, color = 'r', thick = 1) ;  symbol = 'o',
; 
; pixels= indgen(41) + (peak_3-20 )
; pc = plot( pixels, y_fit_3 , /overplot , color = 'r', thick = 1) ; symbol = 'o',


 abs_pixel_a = peak_1 + out_coefficients_1[1]
 abs_pixel_b = peak_2+ out_coefficients_2[1]
 abs_pixel_c = peak_3 + out_coefficients_3[1]
 
; pa = scatter( abs_pixel_a, spec[abs_pixel_a] , /current, /over,   symbol = 'o', sym_color = 'r', sym_thick = 1)
; pb = scatter( abs_pixel_b, spec[abs_pixel_b] , /current, /over,   symbol = 'o', sym_color = 'r', sym_thick = 1) 
; pc = scatter( abs_pixel_c, spec[abs_pixel_c]  , /current, /over,   symbol = 'o', sym_color = 'r', sym_thick = 1)
; 
; 




RETURN, [abs_pixel_a,abs_pixel_b, abs_pixel_c ] ; Returns 3 pixels which are meant to be compared with another spectrum and 
                                                ; 
  
END



;Sample Code for testing
;;---------------------------------- 
;
;night= 171117
;t= 'Comparison between 171218 and ' + strtrim(string(night))
;p = plot([0], [0], title=t) 
;dir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003' ; Used as Reference 
;ref_pixel_2017= find_ref_peak(dir, p=1) 

;
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

; Sample Code for ting
;-----------------------------------
abs_path = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003'
rdsk,sp,abs_path,1
spec = sp[*,*]
p = PLOT(spec , TITLE='BLUE ORDER')


END 
