; Convert UT midpoint time to float hours
; input; string array in 'y-m-dTh:m:s' format
; output: ut, >24h after midnight
function get_nights_dir, path
  nights = list()
  dirs = file_search(path+"*",count=nFiles)
  poss_nights = file_basename(find_all_dir(path))
  poss_nights  = poss_nights[where(stregex(poss_nights,'^[0-9]{1,6}', /boolean))]
  foreach po_night, poss_nights do begin
    night = long(po_night)
     IF night NE 0 THEN nights.add, night
  endforeach 
  print, "Nights found in " + path
  print, nights
  return, nights.toarray()
end