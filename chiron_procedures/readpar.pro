
; Notes:
;       Reads parameters of ctio.par in a data structure
; Input:
;      String with directory of ctio.par file. 
function readpar, file
 line = ''
 tmps = ''

 res = file_search(file, count=c)

  if (c eq 0) then begin
    print, 'File ',file,' is not found, exiting'
  return, 0
  endif
  openr, unit, file, /get_lun
  while not eof(unit) do begin
    readf, unit, line
    
    if strpos(line,';') eq -1 then tmps = tmps + ' ' + line  else tmps = tmps + ' ' + strmid(line,0,strpos(line,';'))
    
    if strpos(line,'}') ne -1 then tmps = strcompress(strtrim(tmps,2))	
  endwhile      
  
  res = execute( 'par = ' + tmps)  ; execute command, create structure
  close, unit 
  free_lun, unit  
  print, 'READPAR: Variables are assigned default values from :', file

  return, par
end