function wv_linearization_coaddition, star_list
  
  ;Get reference spectrum
  ref_fits =  readfits(star_list[0], hd)
  sz = size(ref_fits)
  n_wav = sz[2]
  n_int = sz[2]
  n_orders = sz[3]
  nele=star_list.count()
  spectra = make_array(n_int, n_orders, nele, /double)
  wavelengths = make_array(n_wav, n_orders, /double)
  result = make_array(2, n_int, n_orders, /double)

  ; Create x intervals
  for order = 0L, n_orders-1 do begin
    inc_int = (ref_fits[0,3200-1,order] - ref_fits[0,0,order])/3200
    xinterp = DINDGEN(3200, START= ref_fits[0,0,order], INCREMENT=inc_int)
    wavelengths[*, order]= xinterp
  endfor

  ; Get interpolated intensities values
  for num = 0L, nele-1 do begin
    samp =  readfits(star_list[num])
    for order_inn = 0L, n_orders-1 do begin
      interp = INTERPOL(samp[1,*,order_inn], samp[0,*,order_inn], wavelengths[*, order_inn],  /SPLINE)
      spectra [*,order_inn,num]= interp
    endfor
  endfor

  ; Calculate combine master
  spectra_res = mean(spectra, /double, dimension=3) ;Note the method always uses mean
  result[0,*,*]= wavelengths
  result[1,*,*]= spectra_res
  return, result
       
end