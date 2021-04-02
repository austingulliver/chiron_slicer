
;+
; :Description:
;              Given the y values (the spectra) it returns the positions of the peaks within the array passed.
;              The peaks are found under two premises 1) A pixel is define as peak if the pixel before and after have a lower value that the
;              pixel itself. and 2) The intensity of the potential peak (given than 1) is true ) has to be greate than 'above_noise' times from 
;              the background_lvl
; 
; 
; Input:
;     y_values( 1-d array)    : with the intensity values intepreted as the signal 
;     noise_level (1-d array ):  with 2 elements where [0] is the lower bound of the noise and [1] is the upper bound of the noise. 
;-

function find_peaks , y_values, background_lvl, above_noise=above_noise, read_out_noise=read_out_noise
  compile_opt idl2
   
 
  data_y = y_values
  max_points=list()

  if not keyword_set(above_noise) then above_noise =2.0 ;Default The number of times the line has to be above noise level

  for i=1, n_elements(data_y)-2 do begin     
      ;previous point less than i-th point and next point less than i-th point
      if (  (data_y[i-1] lt data_y[i]) AND (data_y[i] ge data_y[i+1])  ) then  begin  
          if keyword_Set( background_lvl) then begin
               
            
                
;               prev_px    = Float(Round(data_y[i-1]*1000)/1000.)
;               present_px = Float(Round(data_y[i]*1000)/1000.)
;               
;               
;               if prev_px eq present_px then add_flag=0 else add_flag=1; Dont Add 
;               if i  gt 46  then begin
;                 print,  'prev pixel : ' +strt(prev_px)
;                 print,  'present pixel : ' +strt(present_px)
;                 print, 'was added ? ' +strt(add_flag)
;                 print, '    . .. . '
;
;               endif
;               
               
               IF ( data_y[i] gt background_lvl[i] + (  read_out_noise *above_noise ) )    then   max_points.add, i  ;  Test for 2nd parameter defined in description 
            endif else begin 
               max_points.add, i 
            endelse     
      endif
      
  endfor
  
  max_points=max_points.toarray()
  return, max_points  ;return an array of the indices where the extrema occured

end



