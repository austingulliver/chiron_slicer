; Convert UT midpoint time to float hours
; input; string array in 'y-m-dTh:m:s' format
; output: ut, >24h after midnight
function get_nights_dir, path
  nights = list()
  dirs = file_search(path+"*",count=nFiles)
  poss_nights = file_basename(find_all_dir(path))
  foreach po_night, poss_nights do begin
    night = long(po_night)
    catch, Error_status
     IF night NE 0 THEN print, nights.add, po_night
  endforeach 
  return, nights.toarray()
end