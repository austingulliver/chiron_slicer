; Create log structure
; input; list of paths to log files
; output: log structure 
function get_log_structure, path

  log_struc = CREATE_STRUCT('name', '', 'ra', 0.0, 'dec', 0.0, 'inten', FLTARR(12))
  
  
  ;foreach path, paths do begin
   ; readcol,logsheet, skip=9, obnm, objnm, bin, slit, ra, dec,  mdpt,  exptm , ccdTem, airMass,juDate,baryCorrec, intensity,f='(a10,     a15,       a8,    a10 ,   a14,   a14,     a28,      a12,     a12,      a10,     a17,    a14 , a17  )'
  ;endforeach
    
   return, log_struc
  
end