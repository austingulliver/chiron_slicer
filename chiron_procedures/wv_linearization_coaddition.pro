function wv_linearization_coaddition, star_list
  
  ;Get reference spectrum
  ref_fits =  readfits(star_list[0], hd)
  sz = size(ref_fits)
  n_wav = sz[2]
  n_orders = sz[3]
  nele=star_list.count()
  spectra = make_array(n_wav, n_orders, nele, /double)
  wavelengths = make_array(n_wav, n_orders, /double)
  result = make_array(2, n_wav, n_orders, /double)

  ; Create x intervals
  for order = 0L, n_orders-1 do begin
    max_init_wv=-1
    min_final_wv=1.0e30
    for exposure = 0L, nele-1 do begin
      samp =  readfits(star_list[exposure])
      upper_limit  = samp[0,n_wav-1,order]
      lower_limit= samp[0,0,order]
      if lower_limit gt max_init_wv then max_init_wv = lower_limit
      if upper_limit lt min_final_wv then min_final_wv = upper_limit
    endfor
    inc_int = (min_final_wv - max_init_wv)/n_wav
    xinterp = DINDGEN(n_wav, START= max_init_wv, INCREMENT=inc_int)
    wavelengths[*, order]= xinterp
  endfor

  ; Get interpolated intensities values
  for num = 0L, nele-1 do begin
    samp =  readfits(star_list[num])
    for order_inn = 0L, n_orders-1 do begin
      interp = interpol(samp[1,*,order_inn], samp[0,*,order_inn], wavelengths[*, order_inn],  /SPLINE)
      spectra [*,order_inn,num]= interp
    endfor
  endfor

  ; Calculate combine master
  spectra_res = mean(spectra, /double, dimension=3) ;Note the method always uses mean
  result[0,*,*]= wavelengths
  result[1,*,*]= spectra_res
  
  
;  temp =  readfits(star_list[0])
;  p=plot(temp[0,*,0],temp[1,*,0], color='black', title=strt(fxpar(hd, 'OBJECT')))
;  for order_t = 1L, n_orders-1 do begin
;    p=plot(temp[0,*,order_t],temp[1,*,order_t], color='black', /overplot)
;  endfor
  
;  for num = 1L, nele-1 do begin
;    temp =  readfits(star_list[num])
;    for order_t = 0L, n_orders-1 do begin
;       p=plot(temp[0,*,order_t],temp[1,*,order_t], color='black', /overplot  ) ;, title=t)
;    endfor
;  endfor
  
;  for order_t = 0L, n_orders-1 do begin
;    p=plot(result[0,*,order_t],result[1,*,order_t], color='blue', /overplot  ) ;, title=t)
;  endfor
  
  return, result   
end