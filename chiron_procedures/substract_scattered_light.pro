function substract_scattered_light, im, orc, outfname, redpar=redpar
  ;Define constants
  order_width_t =  redpar.xwids[redpar.mode ] - round(redpar.pixel_not_extracted)
  scatter_light = list()
  scatter_light_vals = list()
  copy_im = im
  n_im = size(im)
  n_im_o = n_im[1]
  n_im_t = n_im[2]
  n_orc = size(orc)
  n_orc_o = n_orc[1]
  n_orc_col = n_orc[2]
  
  for enu =0, n_orc_col-1 do begin
    spec_vals = reform(im[enu, *])
    cal_vals = reform(orc[*, enu])
    scatter_min = list()
    scatter_min_vals= list()
    scatter_low = list() 
    scatter_high= list()
    for n_order=0, n_orc_o-1 do begin
        lower_val_l=-1
        higher_val_l=-1
        if n_order eq 0 then begin
          lower_val_l = cal_vals[n_order]- (round(order_width_t/2.0)+2)
          higher_val_l = lower_val_l+2
        endif else begin
          lower_val_l = cal_vals[n_order]- (round(order_width_t/2.0)+2)
          higher_val_l = cal_vals[n_order-1]+ ( round((order_width_t/2.0))+2)
        endelse
        lower_val=min([higher_val_l, lower_val_l])
        higher_val=max([higher_val_l, lower_val_l])
        interval_loc = indgen(higher_val - lower_val + 1, start = lower_val)
        interval_loc_min = min(spec_vals[interval_loc], ind)
        
        ;idx_min = interval_loc[ind]
        ;search_min = indgen(12, start = idx_min-6)
        ;interval_loc_min_n= min(spec_vals[search_min], ind_n)
        
        if n_order lt 19 then begin 
          max_size=1
        endif else if n_order le 33 then begin
          max_size=3
        endif else if n_order le 44 then begin
          max_size=4
        endif else if n_order le 66 then begin
          max_size=7
        endif else begin
          max_size=11
        endelse
        n_sep_p_l=2
        n_sep_p_r=2
        curr_idx_s= interval_loc[ind]
        scatter_l_interval=list(curr_idx_s)
        ctr=1
        
        while ((n_sep_p_l gt 0 or n_sep_p_r gt 0) and max_size gt 0) do begin
            avg_local = mean(spec_vals[scatter_l_interval.toarray()])
            upper_b =avg_local + (avg_local*0.5)
            lower_b = avg_local - (avg_local*0.5)
            curr_idx_l= curr_idx_s - ctr
            ;Search_left
            if (n_sep_p_l gt 0) and (spec_vals[curr_idx_l] lt upper_b and spec_vals[curr_idx_l] gt lower_b) then begin
              scatter_l_interval.add, curr_idx_l ,0
              max_size-=1
            endif else begin
              n_sep_p_l-=1
            endelse
            ;Search right 
            curr_idx_r= curr_idx_s + ctr
            if (n_sep_p_r gt 0) and  (spec_vals[curr_idx_r] lt upper_b and spec_vals[curr_idx_r] gt lower_b) then begin
              scatter_l_interval.add, curr_idx_r ,0
              max_size-=1
            endif else begin
              n_sep_p_r-=1
            endelse
            ctr+=1
        endwhile
        
        scatter_l_interval= scatter_l_interval.toarray()
        
        ;p=plot([median(scatter_l_interval)], [mean(spec_vals[scatter_l_interval])], '+g' , /overplot)
        ;p=plot([cal_vals[n_order]],[spec_vals[cal_vals[n_order]]],'+b',  /overplot)
        ;p=plot(search_min, spec_vals[search_min], color="red", /overplot)
        ;p=plot([search_min[ind_n]], [interval_loc_min_n],  '+g',  /overplot)
        ;p=plot([lower_val, higher_val], [spec_vals[lower_val], spec_vals[higher_val]], color="yellow", /overplot)
        
        ;scatter_min.add,  interval_loc[ind]
        scatter_min.add,  median(scatter_l_interval)
        scatter_min_vals.add, mean(spec_vals[scatter_l_interval])
        ;scatter_low.add, lower_val
        ;scatter_high.add, higher_val
    endfor
    scatter_min = scatter_min.toarray()
    scatter_min_vals=scatter_min_vals.toarray()
    ;scatter_low = scatter_low.toarray()
    ;scatter_high = scatter_high.toarray() 
    scatter_light.add, scatter_min
    scatter_light_vals.add, scatter_min_vals
    ;p=plot(scatter_low, spec_vals[scatter_low],  '+b',  /overplot)
    ;p=plot(scatter_high, spec_vals[scatter_high],  '+g',  /overplot)
    ;p=plot(scatter_min, scatter_min_vals,  '+r',  /overplot)
     ;rint,"ram"
  endfor
  scatter_light = scatter_light.toarray()
  scatter_light_vals=scatter_light_vals.toarray()
   
  ;Remove outliers from scatter light
  intervals_i=19
  intervals_w =  n_orc_col/20
  intervals_w = intervals_w.Floor()
  fitted_pol =  make_array(n_orc_o+1, n_orc_col, /FLOAT, VALUE = 0.0)

  for i=0, n_orc_o-1 do begin
    ro_data = reform(scatter_light_vals[*,i])
    for j=0, intervals_i do begin
      low_idx = intervals_w*j
      if low_idx gt 10 then low_idx-=10
      high_idx = low_idx + intervals_w
      if j eq intervals_i then w_data = ro_data[low_idx: *] else w_data = ro_data[low_idx: high_idx]
      bpd =CREATEBOXPLOTDATA(w_data, MEAN_VALUES=means, OUTLIER_VALUES=outliers)
      iqr=1.5 * (bpd[3]-bpd[1])
      low_f = bpd[1] - iqr
      upper_f = bpd[3] + iqr
      h =  make_array(intervals_w, /FLOAT, VALUE = upper_f)
      l = make_array(intervals_w, /FLOAT, VALUE = low_f)
      x_vals  = low_idx + indgen(intervals_w)
      for k= 0, n_elements(w_data)-1 do begin
        if (w_data[k] gt upper_f or w_data[k] lt low_f) and k ne 0 then begin
          w_data[k]=w_data[k-1]
        endif
      endfor
      if j eq intervals_i then ro_data[low_idx: *] = w_data else ro_data[low_idx: high_idx] = w_data
    endfor
    t='order for scatter light :' +strt(i)
    ;wp=plot(ro_data, color="green", title= t)
    x_vals= indgen(n_orc_col)
    ncoeff_4 = POLY_FIT( x_vals, ro_data, 4, /double,  status=status)
    fit_y_4= poly(x_vals,ncoeff_4)
    ;wp=plot(fit_y_4, color="blue", /overplot)
    fitted_pol[i,*] = fit_y_4
    scatter_light_vals[*,i] = ro_data
  endfor
  
  ;Remove scatter light
  for i= 0, n_orc_col-1 do begin
   ; wp=plot(im[i, *] , color="black")
    for j = 0, n_orc_o-2 do begin
      low_index = scatter_light[i, j]
      high_index =scatter_light[i, j+1]
      width = high_index-low_index
      x_range = low_index + indgen(width)
      fitted_pol_l =  fitted_pol[j,i]
      fitted_pol_h = fitted_pol[j+1, i]
      coff_line = POLY_FIT( [low_index, high_index], [fitted_pol_l, fitted_pol_h], 1, /double,  status=status)
      fit_linear= poly(x_range, coff_line)
      ;wp=plot([low_index, high_index], [fitted_pol_l, fitted_pol_h],  '+b',  /overplot)
      ;wp=plot(x_range, fit_linear,  'yellow',  /overplot)
      copy_im[i, x_range]-= fit_linear
      temp_selec = reform(copy_im[i, x_range])
      bvals = where(temp_selec lt 0, ltzidx)
      if ltzidx gt 0 then begin 
        copy_im[i, x_range[bvals]] = 0.0
      endif
      earse_temp_selec = reform(copy_im[i, x_range])
      ;wp=plot(x_range,  copy_im[i, x_range] , color="green", /overplot)
    endfor
    ;wp=plot(copy_im[i, *] , color="green", /overplot)
  endfor
  
  ;Save scattered light 
  base_name = file_basename(outfname)
  dir_name = file_dirname(outfname)
  sca_light_dir_nm = dir_name + "\scattered_light"
  sca_light_file_nm = dir_name + "\scattered_light" + "\" + base_name + ".fits" 
  spawn, 'mkdir '+sca_light_dir_nm
  MKHDR, scattered_light_hd, scatter_light_vals
  history_str1 = 'Scattered light values'
  sxaddpar, scattered_light_hd, 'HISTORY', history_str1
  writefits, sca_light_file_nm , scatter_light_vals, scattered_light_hd
  
  
  return, copy_im
end

;for i=0, n_orc_o-1 do begin
;if i gt 65 then begin
;   index_scatter = scatter_light[*,i]

;  t='order for spectrum :' +strt(i)
;  wp=plot(scatter_light_vals[*,i], color="green", title=t)
;   max_val = max(poss_middle_val[i,*])
;   for j=0, 1 do begin
;        temp = ans_two[i+j,*] + ((max_val)*j)
;        wp=plot(temp, color="red", /overplot)
;   endfor
;   endif
;endfors