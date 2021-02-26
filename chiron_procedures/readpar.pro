
; Notes:
;       Reads parameters of ctio.par in a data structure
; Input:
;      String with directory of ctio.par file. 
function readpar, file

 debug =1

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
    if debug eq 1 then print, 'Line :' + line
    if strpos(line,';') eq -1 then tmps = tmps + ' ' + line  else tmps = tmps + ' ' + strmid(line,0,strpos(line,';'))
    if debug eq 1 then print, 'Line :' + tmps
    if strpos(line,'}') ne -1 then tmps = strcompress(strtrim(tmps,2))	
   
    
  endwhile      
  
  if debug eq 1 then print, "Everything from ctio.par is :"
  if debug eq 1 then print, tmps
  
  print, ''
  res = execute('par = '+tmps)  ; execute command, create structure
  print, res
  close, unit 
  free_lun, unit  
  print, 'READPAR: Variables are assigned default values from :', file

  return, par
    
end

