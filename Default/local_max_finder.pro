function local_max_finder, datax, datay, $
  ; adapted for emission spectra 
  
  ;keyword for minimum /minima
  minima = minima
  ;set compile options
  compile_opt idl2


  ;initialize list
  max_points = list()

  data_x = datax
  data_y = datay
  
  noise_level =median(data_y)
  max_peak = max(data_y)
  delta_y = (1.0/200.0 ) *(max_peak - noise_level)
  

  
  ;check for keyword, flip the sign of the y values
  if keyword_set(minima) then data_y = -datay

  ;iterate through elements
  ;Check for 5 points
  
  for i=2, n_elements(data_y)-3 do begin
    ;previous point less than i-th point and next point less than i-th point
    if ( (data_y[i-2] lt data_y[i-1]) AND (data_y[i-1] lt data_y[i]) AND (data_y[i] gt data_y[i+1]) AND (data_y[i+1] gt data_y[i+2]) ) then  begin
      
      if data_y[i] ge noise_level + delta_y then   max_points.add, i
    endif
  endfor

  ;return an array of the indices where the extrema occur
  return, max_points.toarray()

end












;main level program
compile_opt idl2
ireset, /no_prompt

;make some test data
xvals = findgen(2000)
yvals = 2*sin( xvals*(2*!PI/500) ) + sin( xvals*(2*!PI/1000) )
;plot the test data
p = plot(xvals, yvals,$
  xtitle = 'X position', ytitle = 'Amplitude',$
  yminor = 3, xminor = 3, title = 'Neat Local Max/Min example')


;find the local max index
local_maxmin_index = local_max_finder(xvals, yvals)

;extract the x/y pairings of the local max/min
x_extrema = xvals[local_maxmin_index]
y_extrema = yvals[local_maxmin_index]

;overplot the max/min on the existing plot
p2 = scatterplot(x_extrema, y_extrema, /current, /overplot, $
  symbol = 'o', sym_color = 'r', sym_thick = 2)


;find the local min index with /minima keyword
local_maxmin_index = local_max_finder(xvals, yvals, /minima)

;extract the x/y pairings of the local max/min
x_extrema = xvals[local_maxmin_index]
y_extrema = yvals[local_maxmin_index]

;overplot the min on the existing plot
p3 = scatterplot(x_extrema, y_extrema, /current, /overplot, $
  symbol = 'o', sym_color = 'b', sym_thick = 2)

end

