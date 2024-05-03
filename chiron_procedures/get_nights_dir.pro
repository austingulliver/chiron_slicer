; Description: Get nights directories from path for given years. 
function get_nights_dir, path, years
  nights =list()
  poss_nights = Findfile(path, Count=count)
  poss_nights = file_basename(poss_nights)
  reg_exp = ''
  if keyword_set(years) then begin
    years_pro = strt(years)
    years_pro = years_pro.substring(2,4)
    counter=0
    foreach yy, years_pro do begin
        if counter eq 0 then begin
          reg_exp+="^("+ yy
        endif else begin
          reg_exp+= "|"+ yy
        endelse
        if counter eq n_elements(years_pro) -1 then begin
          reg_exp+= ")"
        endif
        counter+=1
    endforeach
    reg_exp+="[0-9]{4}$"
  endif else begin
    reg_exp+="^[0-9]{6}$"
  endelse
  remove_d = poss_nights.Matches(reg_exp)
  
  for i=0, Count-1 do begin
    if remove_d[i] eq 1 then nights.add, poss_nights[i]
  endfor
  print, "Nights found in " + path
  print, nights
  return, nights.toarray()
end