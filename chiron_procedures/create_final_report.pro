; NOTES:
;        Extracts star information from each log sheet and constructs a final report about them.
;INPUT :
;         logsheets_paths   LIST with paths to logsheets 
;
;OUTPUT :
;         does not return any value. It creates a file which contains the final log information
;
;  Create by Edison Lozano - 2024/01/11


PRO create_final_report, redpar=redpar,  logsheets_paths=logsheets_paths

  ; Create directory 
  dir_name = redpar.rootdir+redpar.fitsdir+"final_report"
  file_mkdir, dir_name
  
  ; Construct name final report 
  current_date = SYSTIME()
  current_date = current_date.replace(' ', '_')
  current_date = current_date.replace(':', '-')
  name_file = dir_name + "\report_" + current_date + ".log"
  
  extended_form = '(a11,           a15,        a15,             a30)'
  
  ;Create header log file 
  openw,1,name_file
  printf,1,'                CTIO Spectrograph Automation Report '
  printf,1,'  '
  printf,1,'-------------------------------------------------------------------------------------'
  printf,1,'Date Execution: ' + SYSTIME()
  printf,1,'-------------------------------------------------------------------------------------'
  printf,1,'     Night           Object        Object             Date-midUT            '
  printf,1,'     number          number        name               (y-m-dTh:m:s)         '
  
  foreach p, logsheets_paths do begin
    ; Get night number
    night_number =  file_basename(p, '.log')
    ;Read logsheet
    readcol,p, skip=9, obnm, objnm, bin, slit, ra, dec,  mdpt,  exptm , ccdTem, airMass,juDate,baryCorrec, intensity, f='(a10,     a15,       a8,    a10 ,   a14,   a14,     a28,      a12,     a12,      a10,     a17,    a14 , a17  )'
    ;Find stars
    starindx=where(objnm ne 'iodine' and objnm ne 'thar' $
      and objnm ne 'focus' and objnm ne 'junk' and objnm ne 'dark' $
      and objnm ne 'bias' and objnm ne 'quartz' and objnm ne 'master_stellar', num_star)
    if num_star ne 0 then begin
      foreach index, starindx do begin
        printf,1, strcompress(night_number), obnm[index], objnm[index], mdpt[index], format=extended_form     
      endforeach
    endif
  endforeach
  close,1
   print,'Final report can be found at:  ' + name_file
END 