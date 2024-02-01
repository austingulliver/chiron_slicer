; Convert UT midpoint time to float hours
; input; string array in 'y-m-dTh:m:s' format
; output: ut, >24h after midnight
function get_nights_dir, path
  nights =list()
  poss_nights = Findfile(path, Count=count)
  poss_nights = file_basename(poss_nights)
  remove_d = poss_nights.Matches("[0-9]{6}")
  for i=0, Count-1 do begin
    if remove_d[i] eq 1 then nights.add, poss_nights[i]
  endfor
  print, "Nights found in " + path
  print, nights
  return, nights.toarray()
end