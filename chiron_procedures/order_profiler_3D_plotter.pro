path_to_orc_file= "C:\Users\aleja\Desktop\Desktop\Job\Gulliver\Reduction-Pipeline-Software\chiron_reduc_pipeline\chiron\tous\mir7\orders\chi190517.slicer.orc" ; 73 x 4112
path_to_master_flat= "C:\Users\aleja\Desktop\Desktop\Job\Gulliver\Reduction-Pipeline-Software\chiron_reduc_pipeline\chiron\tous\mir7\flats\chi190517.slicer.sum" ; 4112 x 1366


rdsk, orc, path_to_orc_file, 1
rdsk, master_flat, path_to_master_flat, 1
sz_orc = size(orc)
sz_master_flat = size(master_flat)
nc_master_flat=sz_master_flat[1]  &  nr_master_flat=sz_master_flat[2]
n_orders= sz_orc[1]
order_width=12
strips = fltarr(n_orders, nc_master_flat, order_width)

; Extract strips
for k=0,n_orders-1 do begin ;Foreach order
  middle_points = reform(orc[k, *])
  for j=0, n_elements(middle_points)-1 do begin
    yti = middle_points[j] + round((order_width/2.0) - 1) ; Top edge of the order
    ybi = middle_points[j] - round(order_width/2.0)        ; Bottom edge of the order

    ; Ensure yti and ybi are within valid bounds
    yti = min([yti, nr_master_flat - 1])
    ybi = max([ybi, 0])

    ; Extract the 12-value strip
    strip = master_flat[j, ybi:yti]

    ; Store the 12-value strip into the 3D array
    if n_elements(strip) eq order_width then begin
      strips[k, j, *] = strip
    endif

  endfor

endfor

order_number = 0
strips_order_number = reform(strips[order_number, *, *])
; Plotting along the dispersion for order_number = #

x_vals = findgen(nc_master_flat, start=0) ; [0,1,2,3,....4111]

for i=0, order_width-1 do begin
  y_vals = fltarr(nc_master_flat)
  z_vals = replicate(i, nc_master_flat) ; [0,0,0]

  for j=0, nc_master_flat-1 do begin
    individual_strip = strips_order_number[j, *]
    y_vals[j] = individual_strip[i]
  endfor

  p=PLOT3D(x_vals, y_vals, z_vals , title='3D Plot for Order ' + string(order_number), XTITLE='x', YTITLE='y', ZTITLE='z', /overplot)
endfor
temp = 0


; Plotting perpendicular to the dispersion for order_number = #

;for i=0, nc_master_flat-1 do begin
;  z_vals = replicate(i, order_width); [0,0,0,0... x oder_width]
;  x_vals = findgen(order_width, start=0)
;  y_vals = strips_order_number[i, *]
;  print, x_vals
;  print, y_vals
;  print, z_vals
;  if i eq 0 then begin
;    p=PLOT3D(x_vals, y_vals, z_vals , title='3D Plot for Order ' + string(order_number), XTITLE='x', YTITLE='y', ZTITLE='z')
;  endif else begin
;    p=PLOT3D(x_vals, y_vals, z_vals , title='3D Plot for Order ' + string(order_number), XTITLE='x', YTITLE='y', ZTITLE='z', /overplot)
;  endelse
;endfor

;temp = 0

end
