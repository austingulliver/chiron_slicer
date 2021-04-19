

;+
; :Description:
;             Calls the barycentric correction for the stellar observations 
;
;
; Output : czi (barycentric correction)
;          jdUTC (julian data)
;-
pro call_qbary,star_name=simbadStarName, jd=jdUTC, barydir=barydir,czi=czi 
  
  debug=0
  
  
  ;----------
  ;>> 1_ JdUT
  ;----------
  
  ; Calculated before entering this procedure.
  ;mjd = jdUTC-2440000.d0  ; This modification was originally made by JD in qbarylog.pro. It was not used but might lead to a hint
  ; in the case of malfunction
  

  ;---------
  ;>>2_ coords
  ;--------
  print, 'call_qbary:  Requesting RA and DEC information from SIMBAD ..... '
  
  ; Validate simbadStarName
  
  
  QuerySimbad, simbadStarName , ra, dec, found=coord_found
  if  coord_found ne 0 then begin
      ra= ra* (24.0/360.0)      ; Transformation from degrees to hours
      coords = [ra,dec]
  endif else print, 'call_qbary: Request failed '
    
  ;---------
  ;>>3_ epoch
  ;--------
  epoch =2000.d0
  
  ;---------
  ;>>4_ pm
  ;--------  
  pm = [0.d0,0.d0]  ; pass dummy values for  proper motion
  

 
    ; ***********************************************
    ;  Run Correction
    ; ***********************************************
    if debug gt 0 then begin
        ; >>Input needed for qbary.pro
        print,  ' ------------------------------------   '
        print, 'coords : '  ; RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]
        print, string( coords)
        print, 'pm     : ' + string(pm)       ; proper motion [ra,dec] in ARCsec/year [optional]
        print, 'epoch  : ' + string(epoch)
        print, 'barydir: ' + string(barydir)   ; Barycentric directory
        print, 'jdUTC  : ' + string(jdUTC)    ; julian date (double precision) eg 2448489.3462d0
    endif
    
    if coord_found eq 0 then begin
         print, ' ** The coordinates (RA and DEC) for the star '+ strtrim(simbadStarName,2) + ' were not found. Check for the name **'
         stop, ' >> Error <<  Please pass the argument star_name when calling logmaker_v2 OR get rid of /barycorr . RUN AGAIN'
         czi= 0.0
    endif else begin
         qbary,jdUTC,coords,epoch,czi,obs='ctio', pm = pm,barydir=barydir, ha=ha
    endelse
    
;   >>Output of qbary.log
;   print, 'ha : hour angle of observation '+string(ha)
;   print, 'czi :relativistic redshift' +string(czi)
  
  

end







; convert month numbers to 3 character string names
;
function strMonth, mon
  nMonths = ['01','02','03','04','05','06','07','08','09','10','11','12']
  sMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

  n = where ( nMonths eq mon )
  if ( n lt 0 ) then m = '???' else m = sMonths[n]
  return, m
end


;+
;
; CALLING SEQUENCE: logmaker,'110309'
;
; MODIFICATION HISTORY:
;
; 12-Mar-2011  DF (from original logmaker.pro with previous contributions from JS, JB, MG
; -fixed a problem with addcomment 20110321 ~MJG
; -readded the override keyword for use with automated calibration scripts. 20110326 ~MJG
; -replaced midtime calculation from "UT of TCS" to "UT of Shutter Open". 20110517 ~MJG
; -added focus information 201106 ~MJG
; -added archiving of old logs and noarchive keyword 201106 ~MJG
; -increased space for objnm and propid to accommodate queue observers ~MJG
; -made it search for the 'chiyymmdd' prefix before using the 'qa##' prefix 20110920 ~MJG
; 12-12-2020 J.Lozano Make procedure Windows Compatible + Addded compatabilty with enviroment variable CHIRON_PATH of paths.
; 09-04-2021 J.Lozano Added complete UT to each observation + Barycentric Correction .


; JL:  E.g. -- IDL> logmaker_v2, '210118', /nofoc, prefix='chi', star_name='HR5049',/barycorr

pro logmaker_v2, rawdir, $
  override = override, $
  prefix = prefix, $
  noarchive=noarchive, $
  nofoc =nofoc, $
  date=date, $
  barycorr=barycorr, $ ; When set includes barycentric correction and Jdate in the .log file
  star_name =star_name,$  ; Name of the stellar object. This name HAS TO BE compatible with SIMBAD 
  stellar_bary_correc=stellar_bary_correc



  ;*****************************************
  ;Check for paths 
  ;*****************************************

  ;spawn, 'date "+%Y"', yyyy
  if keyword_set(date) then begin
    yyyy='20'+strmid(strt(date),0,2)
  endif else begin
    yyyy = '20'+strmid(strt(rawdir),0,2)
  endelse


  chi_path=getenv('CHIRON_PATH')  ;This must the absolute path where the directory chiron was placed
  ; E.g.  SETENV, 'CHIRON_PATH=.......\chiron'


  spawn, 'hostname', host
  spawn, 'cd', pwddir   ;Updated to a Windows command

  ;print, "This is the actual directory : " +string(host)


  if strlen(chi_path)  ge 1 then begin
    rawpath = chi_path.trim() + '\raw\mir7\'
    logpath = chi_path.trim() + '\tous\mir7\logsheets\'+yyyy+'\'
  endif else begin

    if host eq 'ctimac1.ctio.noao.edu' then begin
      rawpath = '/mir7/raw/'
      logpath = '/mir7/logsheets/'+yyyy+'/'
    endif else if  host eq 'Drius22' then begin
      rawpath = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\'
      logpath = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\logsheets\'+yyyy+'\'
      barypath = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\bary\'
    endif else if pwddir eq 'C:\Users\gulliver' then begin
      rawpath = 'C:\F disk\chiron_reduc_pipeline\chiron\raw\mir7\'
      logpath = 'C:\F disk\chiron_reduc_pipeline\chiron\tous\mir7\logsheets\'+yyyy+'\'
      barypath = 'C:\F disk\chiron_reduc_pipeline\chiron\tous\mir7\bary\'
    endif else begin
      rawpath = '/nfs/morgan/chiron/raw/mir7/'
      logpath = '/nfs/morgan/chiron/tous/mir7/logsheets/'+yyyy+'/'
    endelse

  endelse







  ;*****************************************
  ;Constants + validation
  ;*****************************************
  logname = logpath+rawdir+'.log'
  _QUARTZ = 'quartz'
  _THAR = 'thar'
  _IODINE = 'iodine'
  
  ;Check existence of directory
  if ~file_test(logpath) then spawn, 'mkdir '+ string(34B) + logpath + string(34B)
  ; Otherwise just create the year directory manually. E.g. ..\chiron\tous\mir7\logsheets\2021\



  ;*****************************************
  ;Check existing log sheet
  ;*****************************************
  lfile=File_Search(logname,count=nlogs)
  if nlogs gt 0 and ~keyword_set(override) then begin
    answer=''
    print,'Hey! this log file exists already - are you sure you want'
    read,' to overwrite? (y/n)', answer
    if answer eq 'n' then stop
  endif
  if (~keyword_set(noarchive) and nlogs) then $
    spawn, 'cp '+logname+' '+nextname(logpath+'archive/'+rawdir+'.log'+'_old', '')

  ;>> Retrieve all  raw files
  allFitsFiles=File_Search(rawpath+rawdir+'/chi*.fits',count=nFiles)
  print, 'LOGMAKER: Number of files found is: ', nFiles
  if nFiles eq 0 then begin     
    stop,'LOGMAKER: >>Error << Make sure the raw files follow this format: ',rawpath+rawdir+'\*.fits'
  endif




  ;*****************************************
  ;Check prefix for labeling logsheet header and checking for missing files (end of program)
  ;*****************************************

  if ( ~keyword_set(prefix) || strt(prefix) eq '' ) then begin
    
        ; >> Finding prefix by ourselves
        lens = strlen(allfitsfiles)
        nfts = n_elements(allfitsfiles)
        prefixarr1 = strarr(nfts)
        for i=0, nfts-1 do prefixarr1[i] = strmid(allfitsfiles[i],  $
                           strlen(rawpath)+strlen(rawdir)+1, $
                           lens[i] - 9 - strlen(rawpath)-strlen(rawdir)-2)
        prefixarr = prefixarr1[uniq(prefixarr1, sort(prefixarr1))]
        for i=0, n_elements(prefixarr)-1 do begin
            if strmid(prefixarr[i], 0, 3) eq 'chi' then prefix = prefixarr[i]
        endfor
        if size(prefix, /type) eq 0 then begin
            for i=0, n_elements(prefixarr)-1 do begin
                if strmid(prefixarr[i], 0, 2) eq 'qa' then prefix = prefixarr[i]
            endfor
        endif
        
  endif else begin
        print,"LOGMAKER: Using '" + prefix + "' as the image prefix."
  endelse







  ;*****************************************
  ; Validation for formatted files: qa04.nnnn.fits or qa04_nnnn.fits
  ;*****************************************
  ;obFiles = where(stregex(allFitsFiles,'\'+prefix+'\.([0-9]+)\.fits$',/BOOLEAN))
  obFiles = where(stregex(allFitsFiles,'\'+prefix+'([0-9]+).([0-9]+).fits$',/BOOLEAN)) ; Updated for Windows
  if ( n_elements(obFiles) eq 0 ) then  stop,  'No files found with name format "/' + prefix + '\.([0-9]+)\.fits$"'

  obs_file = allFitsFiles[obFiles] ; Only observation files that match our format. 
  nobs = n_elements(obs_file)
  print, 'Number of Observations files found, nobs, is: ', nobs

  ; FIND THE STARTING NUMBER FOR TONIGHT'S OBSERVATIONS
  ;  Not being used  so the following two lines were commented.
  ;  first_num = stregex(obs_file[0],"([0-9]+)\.fits$",/EXTRACT,/SUBEXPR)
  ;  first_num = first_num[1] ; just the number captured in the parens






  ;*****************************************
  ; Extract DATE information
  ;*****************************************

  ; COLLECT SOME LOGSHEET INFORMATION FROM THE FITS HEADERS
  ; Is expected that the files come sorted  straight from WINDOWS 
  hd = headfits(obs_file[0])       ;Header from first observation
  hd2= headfits(obs_file[nobs-1])  ;Header from final observation

  ; OBSERVER
  observer=sxpar(hd, 'OBSERVER', count=m1)
  if m1 eq 0 then begin
    observer = '               '
    print,'***************************************************************'
    print, 'Please edit the logsheet to add the observer name'
    print,'***************************************************************'
  endif

  ; DATE AND LOGFILE NAME  ;file creation date:
  obsDate = sxpar(hd,'DATE',count=foundDate)
  obsDateEnd = sxpar(hd2,'DATE',count=foundDateEnd)

  ; extract yyyy-mm-dd
  dateParts = stregex(obsDate,"^([0-9]{4}).([0-9]{2}).([0-9]{2})",/EXTRACT,/SUBEXPR)
  datePartsEnd = stregex(obsDateEnd,"^([0-9]{4}).([0-9]{2}).([0-9]{2})",/EXTRACT,/SUBEXPR)

  yr = dateParts[1]
  mon = dateParts[2]
  day = dateParts[3]

  endYr = datePartsEnd[1]
  endMon = datePartsEnd[2]
  endDay = datePartsEnd[3]

  mon = strMonth(mon)
  endMon = strMonth(endMon)

  date=yr+', '+mon+' '+strcompress(string(fix(day)),/rem)
  if ( yr ne endYr ) then begin
    date = date + '/' + endYr + ', ' + endMon + ' ' + strcompress(string(fix(endDay)),/rem)
  endif else if ( mon ne endMon ) then begin
    date = date + '/' + endMon + ' ' + strcompress(string(fix(endDay)),/rem)
  endif else begin
    date = date + '/' + strcompress(string(fix(endDay)),/rem)
  endelse




  ;*****************************************
  ;extract CHIP and CONTROLLER information
  ;*****************************************

  runInfo = chip_geometry(obs_file[0],hdr=hd)

  if ( runInfo.status eq 'error' || runInfo.controller eq 'unknown' ) then begin
    print,'***************************************************************'
    print, 'Unable to determine chip characteristics '
    print,'***************************************************************'
  endif

  ; some of this info is written to the logsheet header (pre-2011)
  ccd_info = runInfo.ccd
  if runInfo.controller eq 'old' or runInfo.controller eq 'new' then begin
    colStart = runInfo.image_trim.upleft[0]
    cols =  (runInfo.image_trim.upleft[1]-runInfo.image_trim.upleft[0] + 1) + $
      (runInfo.image_trim.upright[1]-runInfo.image_trim.upright[0] + 1)
    rowStart = runInfo.image_trim.upleft[2]
    rows = (runInfo.image_trim.upleft[3]-runInfo.image_trim.upleft[2] + 1)
    str_ech_old='Ech: 1114  X-Dis: 226 l/mm     Slit: 150 micron fiber, 60 micron slit'
    str_xdisp_old='Cross disperser #3        Blaze: 6300A --> FWHM= (used Th-Ar#2, 20sec)'
  endif

  if runInfo.controller eq 'mnsn' or runInfo.controller eq 'torrent' then begin
    str_ech='Ech: CHIRON'
    str_xdisp='Fixed Cross-disperser position'
  endif



  ;form1='(a5, a13, a4, a14, a8, a12, a6, a15, a-60)'
  ;form2='(a10, a8, a4, a14, a8, a12, a6, a15, a-60)'
  ;form1 is for normal observations
  ;      (#, space, name, i2, mdtm, exptm, bin, slit, spc, propid, comment)
  form1='(a5, a1, a18 ,a4,  a14,    a8,  a6,  a-8,  a1,    a15,    a-40)'
  ;form2 is for quartz exposures
  form2='(a10, a1, a13, a4, a14,    a8,  a6,  a-8,  a1,    a15,    a-40)'
   
  ;                (#,   space,   objname,   i2,   mdtm,   jd,    barycorrec    exptm,  bin,   slit,  space,   propid,  comment)
  extended_form = '(a10, a1,      a13,       a4,   a14,    a20,   a15,           a8,     a6,    a-8,   a1,      a15,     a-40)'




  ;*****************************************
  ;Extract Focus Information
  ;*****************************************
  ;this while loop will go through the files until it finds a
  ;narrow slit thar observation for the focus:
  if ~keyword_set(nofoc) then begin
    foc = 0
    i=0L
    while ~foc do begin
      im = mrdfits(allfitsfiles[i], 0, hd, /silent)
      deck = strt(sxpar(hd, 'DECKER'))
      objnm = strt(sxpar(hd, 'OBJECT'))
      print, allfitsfiles[i], '   ', deck, '   ',objnm
      if (deck EQ 'narrow_slit') and (strlowcase(objnm) EQ 'thar') then foc = 1 else i++
    endwhile
    foc, inpfile=allfitsfiles[i], slicevals=slicevals;, /plt, /mark
    focpos = strt(sxpar(hd, 'FOCUS'), f='(F10.4)')
    focfwhm = strt(slicevals.avgfwhm, f='(F7.3)')
  endif else begin
    focpos = ''
    focfwhm = ''
  endelse







  ;*****************************************
  ; .LOG's header
  ;*****************************************

  openw,1,logname
  printf,1,'                CTIO Spectrograph Observing Log '
  printf,1,'  '
  printf,1,'-------------------------------------------------------------------------------------'
  printf,1,'Observer: '+observer+'        Telescope: CTIO 1.5-m              Prefix: '+strt(prefix)
  printf,1,'UT Date: '+date+'             Chip: '+ ccd_info +  '  Foc: '+focpos+' mm'
  ; pre-2011 logsheets
  if runInfo.controller eq 'old' or runInfo.controller eq 'new' then begin
      printf,1,'Windowing: Cols: '+strtrim(string(cols),2)  $
        +', Xstart='+strtrim(string(colStart),2)    $
        +'    Rows: '+strtrim(string(rows),2)   $
        +' Ystart='+strtrim(string(rowStart),2)
      printf,1,'Binning: '+strtrim(string(runInfo.bin.row),2)+'r x ' $
        +strtrim(string(runInfo.bin.col),2)+'c        prefix: '+prefix
      printf,1,'prefix: '+prefix
      printf,1,'--------------------------------------------------------------------------'
      printf,1,' Obs       Object    I2     Mid-Time     Exp      Comments'
      ;post 2010 logsheets (e2v 4K detector)
  endif else begin
      printf,1, str_ech, '     ', str_xdisp, '                  Foc FWHM: '+focfwhm
      printf,1,'-------------------------------------------------------------------------------------'
      if keyword_Set(barycorr) then begin
        printf,1,' Obs            Object    I2     Mid-Time              JDate       BaryCorrec     Exp  Bin   Slit             PropID   Hdr-Comments'
        print,   ' Obs            Object    I2     Mid-Time              JDate       BaryCorrec     Exp  Bin   Slit             PropID   Hdr-Comments'
        printf,1,'number           Name    (y/n)     (UT)                               [m/s]   '
      endif else begin
        printf,1,' Obs            Object    I2     Mid-Time     Exp  Bin   Slit             PropID   Hdr-Comments'
        print,   ' Obs            Object    I2     Mid-Time     Exp  Bin   Slit             PropID   Hdr-Comments'
      endelse

  endelse
                  










  ; setup arrays for grouping quartz images
  counter_num = intarr(nobs)
  counter     = intarr(nobs+1)
  decknm      = strarr(nobs)
  exptm     = fltarr(nobs)
  i2        = strarr(nobs)
  st_exptime = strarr(nobs)
  propID     = strarr(nobs)
  binarr      = strarr(nobs)
  slitarr     = strarr(nobs)
  comment1    = strarr(nobs)
  bary_correc = strarr(nobs) ; Barycentric Correction 
  mjdate      = strarr(nobs) ;Modified Julian Date
  
  
  ;Used in a different script
  ;Store the observation number (or file names) along with barycentric correction found
  stellar_bary_correc=list()  ; a list containing structure in the form {file_name:    ,  correction: }
  
  
  
  ;*****************************************
  ; .LOG's body : 1 line per observation
  ;*****************************************

  for i=0, nobs - 1 do begin
    
 
        file_num = stregex(obs_file[i],"([0-9]+)\.fits$",/EXTRACT,/SUBEXPR)
        file_num = file_num[1] ; just the number captured in the parens
        fitsname = obs_file[i]
        hd=headfits(fitsname,ERRMSG=badFits) ; Extract header only from file.
        
        if ( strlen(badFits) gt 0 ) then begin ; Default bad header
            printf,1, file_num, ' ', 'JUNK', 'n', '00:00:00.0', '0', 'BAD FITS HEADER', format=form1
            continue
        endif
        
    
    
    
        ; >> [3] Mid-Time (UT): midtime
        ;---------------------------
        Exptime = sxpar(hd,'EXPTIME')   ; float
        half_exp = 0.5 * (Exptime / 3600.)
        Starttime = sxpar(hd,'UTSHUT')      ; string
        starttime = strmid(starttime, 11, strlen(starttime) - 11)
        hti = ten(float(strsplit(Starttime,':',/ext))) + half_exp
        mt = sixty(hti)
        mth = strcompress(string(fix(mt[0])),/remove_all)
        if strlen(mth) eq 1 then mth = '0'+mth
        mtm = strcompress(string(fix(mt[1])),/remove_all)
        if strlen(mtm) eq 1 then mtm = '0'+mtm
        mts = strcompress(string(fix(mt[2])),/remove_all)
        if strlen(mts) eq 1 then mts = '0'+mts
        midtime=mth+':'+mtm+':'+mts    ; geometric midpoint time - to be replaced !!
    
        ; if the EM midpoint time is available, use it instead
        diff_from_geom=0.0
        em_time=sxpar(hd,'EMMNWOB', count=mid_time_match) ;/ mean time without bckgrd subtraction    
        if mid_time_match gt 0 then begin
          em_time=strmid(em_time, 11, 8)
          if em_time ne '00:00:00' then begin
              em_h = strmid(em_time, 0, 2)
              em_m = strmid(em_time, 3, 2)
              em_s = strmid(em_time, 6, 2)
              dh_sec = (em_h*3600.) - (mt[0]*3600.)
              dmin_sec = (em_m*60.) - (mt[1]*60.)
              dsec = em_s - mt[2]
              diff_from_geom = dh_sec + dmin_sec + dsec
              midtime=em_time
          endif
        endif
        





        ; >> [1] Object name: objName 
        ;---------------------------------------------------------------
        ; Standardize the object names for quartz, thar, and iodine
        object = sxpar(hd, 'OBJECT')   ; string
        objName = strcompress(object,/remove_all)
        if ( stregex(objName,'^(qtz|quartz|flat|normalslit|normal slit|wideflats|wideflat|quartz\/calibra)$',/BOOLEAN,/FOLD_CASE) ) then begin
          objName = _QUARTZ
          ;if strcompress(sxpar(hd,'COMPLAMP'),/rem) eq 'TH-AR' then objName = _THAR
        endif else if ( stregex(objName,'^(th|thar|thar\/calibra)$',/BOOLEAN,/FOLD_CASE) ) then begin
          objName = _THAR
          ;if strcompress(sxpar(hd,'COMPLAMP'),/rem) eq 'QUARTZ' then objName = _QUARTZ
        endif else if ( stregex(objName,'^(i2|iodine|qtziodine|quartz\/iodine)$',/BOOLEAN,/FOLD_CASE) ) then begin
          objName = _IODINE
        endif
    
        ;kludges for alpha Cen typo's...
        if strlowcase(objName) eq 'hd128621' or strlowcase(objName) eq 'hd128261' then objName = '128621'
        if strlowcase(objName) eq 'hd128620' or strlowcase(objName) eq 'hd128260' then objName = '128620'
        
        
        
        
        
        
        ; >> [0] Obs  number :st_num
        ; -------------------------- 
        st_num = strt(file_num, f='(I04)')
        
        ;st_object = strcompress(object,/remove_all) ; comment cause not used
        ;st_exptime = strcompress(string(fix(Exptime)),/remove_all)
        
        
        
        
        
        ; >> [4] Exp time : exptime
        ;-----------------------------
        st_exptime[i] = strt(exptime, f='(f5.2)')
        if exptime ge 10 then st_exptime[i]=strt(exptime, f='(i5)')
        
        ;st_starttime = strcompress(Starttime,/remove_all) ; Commented cause not used
        ;st_starttime = strmid(st_starttime,0,strlen(Starttime)-4)
        
        
        
        
        ; >> [3] in LOG : Is (y/n) :i2
        ; --------------------------
        iod = sxpar(hd,'IODCELL', count=iod_tag)
        if iod_tag gt 0 and strcompress(iod,/rem) eq 'IN' then i2[i] = 'y'
        if iod_tag gt 0 and strcompress(iod,/rem) eq 'OUT' then i2[i] = 'n'       
        if iod_tag eq 0 then begin ;old school, old controller, no IODCELL keyword - do the best you can.
            i2[i]='y'
            if ( objName eq _QUARTZ or objName eq _THAR or strupcase(strmid(objName,0,2)) eq 'HD' ) then i2[i] = 'n'
        endif
        if i2[i] eq 'y' and objName eq _QUARTZ then objName = _IODINE
    
    
    
    
    
       
        ; >> [5] and [6] Bin and Slit : binarr and slitarr
        ; -----------------------------
    
        ccd_ck = chip_geometry(obs_file[i],hdr=hd)   
        if (ccd_ck.status eq 'OK' and strpos(ccd_ck.ccd,'e2v') lt 0) then begin
            ;>> Pre-2011: DETECT ANY CHANGES IN THE SETTINGS
            ;--------------------------------------------
            if ( ccd_ck.controller ne runInfo.controller ) then comment1 = comment1 + ' ctl: ' + ccd_ck.controller
            for j=0, n_elements(runInfo.image_trim.upleft)-1 do begin
              if ( ccd_ck.image_trim.upleft[j] ne runInfo.image_trim.upleft[j] ) then begin
                comment1[i] = comment1[i] + ' geoL: ' + strTrim(string(ccd_ck.image_trim.upleft[0]),2) + $
                  ':' + strTrim(string(ccd_ck.image_trim.upleft[1]),2) + ',' + $
                  strTrim(string(ccd_ck.image_trim.upleft[2]),2) + ':' + $
                  strTrim(string(ccd_ck.image_trim.upleft[3]),2)
                break
              endif
            endfor
            for j=0, n_elements(runInfo.image_trim.upright)-1 do begin
              if ( ccd_ck.image_trim.upright[j] ne runInfo.image_trim.upright[j] ) then begin
                comment1[i] = comment1[i] + ' geoR: ' + strTrim(string(ccd_ck.image_trim.upright[0]),2) + $
                  ':' + strTrim(string(ccd_ck.image_trim.upright[1]),2) + ',' + $
                  strTrim(string(ccd_ck.image_trim.upright[2]),2) + ':' + $
                  strTrim(string(ccd_ck.image_trim.upright[3]),2)
                break
              endif
            endfor
            if ( ccd_ck.bin.row ne runInfo.bin.row || ccd_ck.bin.col ne runInfo.bin.col) then begin
              comment1[i] = comment1[i] + strtrim(string(ccd_ck.bin.row),2) + 'r' $
                + strtrim(string(ccd_ck.bin.col),2) + 'c'
            endif
            
        endif else begin
          
            ;>> Post-2011: GET HEADER INFO
            ; --------------------------------
            if (ccd_ck.status eq 'OK' and strpos(ccd_ck.ccd,'e2v') ge 0) then begin
                im=readfits(obs_file[i],header,/silent)
                bintag=sxpar(header,'CCDSUM')
                bintag=strcompress(bintag,/rem)
                bin=strarr(2)
                bin[0]=strmid(bintag,0,1)
                bin[1]=strmid(bintag,1,1)
                decknm[i]=strt(sxpar(header,'DECKER'))
                exptm[i]=sxpar(header,'EXPTIME')
                if decknm[i] eq 'narrow_slit' then decknm[i] = 'narrow'
                binarr[i] = bin[0]+'x'+bin[1] ;<< [5] Bin
                slitarr[i] = '  '+strt(decknm[i]) ; [6] Slit
                comment1[i]=' '
            endif
        endelse
        
        
        
        
        
        
        ; >> [8] Hdr Comments : comment1
        ; -----------------------------     
        ;grab comments from the FITS header
        comments = sxpar(hd, 'COMMENT')
        if n_elements(comments) ge 3 then begin
          if comments[2] ne 'none' then addcomment=comments[2] else addcomment = ''
        endif else addcomment = ''    
        ; don't let manual entries override FITS headers
        if addcomment eq 'slicer' or addcomment eq 'normal slit' or $
          addcomment eq 'fiber' or addcomment eq 'narrow slit' then addcomment=''    
        comment1[i] = '      '+comment1[i]+' '+addcomment
        if diff_from_geom gt 0.0 then comment1[i] = comment1[i]+' '+strcompress(string(fix(diff_from_geom)),/rem)+' sec'
        
        
        
        ; >> [7] PropID :propID[i]
        ; -----------------------------
        propspec = sxpar(hd, 'PROPID')
        if strt(propspec) eq '' then propspec = 'chi'
        propID[i] = strt(propspec)
        
        
        
    
        ;FOR FLATS, ABBREVIATE THE NUMBER SYSTEM
        endflag=0  ;changes from 0 if it enters a grouping loop
        if (objName eq _QUARTZ) then begin
          counter[i] = 1            ;start counting them
          counter_num[i] = st_num
          holder = midtime
        endif
        
        
        
        
        
        ; >> [] JD, czi : Julian Date and relativistic redshift
        ; -----------------------------
        
        if keyword_Set(barycorr) then begin 
              if objName ne 'iodine' and objName ne 'thar' $ ; Only for stellar file
              and objName ne 'focus' and objName ne 'junk' and objName ne 'dark' $
              and objName ne 'bias' and objName ne 'quartz' and objName ne 'master_stellar' then begin
  
              em_date_time=sxpar(hd,'EMMNWOB', count=mid_time_match)
              ; >> Find Julian Date and calculate barycentric correction only for stellar objects.
              if mid_time_match gt 0 then begin
  
                ;date
                em_date=strmid(em_date_time, 0, 10)
                em_date = strsplit(em_date, '-',/extract)
                year    =em_date[0]
                month   =em_date[1]
                dd      =em_date[2]
  
                ;time
                em_time=strmid(em_date_time, 11, strlen(em_time))
                em_time =strsplit(em_time, ':', /extract)
                hour    =em_time[0]
                minutes =em_time[1] + (em_time[1]/60.0) ; *Including decimales
  
  
              endif else begin
                ; Create option for when  keyword EMMNWOB does not exist
              endelse
  
              jdUTC = jdate([year,month,dd,hour,minutes])
              JD    = cgNumber_Formatter(jdUTC, DECIMALS=6)
  
              if keyword_Set(star_name) then objName = star_name
  
              call_qbary, star_name=objName, jd=jdUTC, barydir=barypath, czi=czi
              bary_correc = cgNumber_Formatter(czi,DECIMALS=3) ; Barycentric Correction
  
  
              ; For future reference only stellar files
              file_nm_to_save=  stregex(obs_file[i], 'chi([0-9]+).([0-9]+).fits', /extract)
              stellar_bary_correc.add, {file_name:file_nm_to_save , correction:czi } ; Meant to be output
  
            endif else begin
  
              ;>>Default for non-stellar files
              bary_correc = '0.0' ; Barycentric Correction
              JD          = '0.0'
  
            endelse
        endif ; end bary correction
       
        
       
        
      
        
        
        
        
        ;-------------------------------------------------------
        ; >>Priting  all collected information from observation
        ;--------------------------------------------------------
        
        
        if i ne 0 then begin
           
            ; Printing for FLATS groups
            ;------------------------------------
    
            ;last observation was a qtz, this one is not, end grouping
            if counter[i] eq 0 and counter[i-1] eq 1 then begin
                endflag=0
                wfirst = where(counter eq 1)
                numfirst= n_elements(wfirst)
                first = counter_num[wfirst[0]]  &  first = strt(first, f='(I04)')
                last  = counter_num[wfirst[numfirst-1]]  & last = strt(last, f='(I04)')
                if last ne first then st_numW = first+'-'+last else st_numW = first
                ;       if last ne first then st_numW =  ' '+strtrim(first,2)+'-'+strtrim(last,2) else st_numW = first
               
                if keyword_Set(barycorr) then begin
                       printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder, JD, bary_correc , st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=extended_form
                endif else begin
                        printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder , st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=form1
                endelse
                                
                counter_num = intarr(nobs)   ;reset counter array to zeros after printing.
                counter     = intarr(nobs+1)
            endif
      
            ;last observation was a qtz, this is qtz, but different decknm
            if counter[i] eq 1 and counter[i-1] eq 1 then begin
                if (decknm[i] ne decknm[i-1]) then begin
                    wfirst = where(counter eq 1)
                    numfirst= n_elements(wfirst)
                    first = counter_num[wfirst[0]]  &  first = strt(first, f='(I04)')
                    last  = counter_num[wfirst[numfirst-2]]  & last = strt(last, f='(I04)')
                    if last ne first then st_numW = first+'-'+last else st_numW = first
                    ;         if last ne first then st_numW =  ' '+strtrim(first,2)+'-'+strtrim(last,2) else st_numW = first

                    
                    if keyword_Set(barycorr) then begin
                          printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder,JD, bary_correc ,  st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=extended_form
                    endif else begin
                          printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder,  st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=form1
                    endelse
                                        
                    counter_num = intarr(nobs)   ;reset counter to zeros after printing.
                    counter     = intarr(nobs+1)
                    endflag = 0 ; want 0 if it's alone and 1 if the next one is a qtz....
                endif          
            endif
      
            ;last observation was a qtz, this is a qtz, but different exptm
            if counter[i] eq 1 and counter[i-1] eq 1 then begin
                if (exptm[i] ne exptm[i-1]) then begin
                    wfirst = where(counter eq 1)
                    numfirst= n_elements(wfirst)
                    first = counter_num[wfirst[0]]   &  first = strt(first, f='(I04)')
                    last  = counter_num[wfirst[numfirst-2]]  & last = strt(last, f='(I04)')
                    if last ne first then st_numW = first+'-'+last else st_numW = first
                    ;         if last ne first then st_numW =  ' '+strtrim(first,2)+'-'+strtrim(last,2) else st_numW = first
                    
                    if keyword_Set(barycorr) then begin
                       printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder, JD, bary_correc , st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=extended_form
                    endif else begin
                       printf,1, strcompress(st_numW,/rem)+'    ', ' ', _QUARTZ, i2[i-1], holder , st_exptime[i-1], binarr[i-1], slitarr[i-1],  ' ', propID[i-1], comment1[i-1], format=form1
                    endelse
                    
                    counter_num = intarr(nobs)   ;reset counter to zeros after printing.
                    counter     = intarr(nobs+1)
                    endflag = 0 ; want 0 if it's alone and 1 if the next one is a qtz....
                endif
            endif
      
            ;last observation in the directory
            if i eq nobs-1 and counter[i] eq 1 then begin
                endflag=4
                wfirst = where(counter eq 1)
                numfirst= n_elements(wfirst)
                first = counter_num[wfirst[0]]    &  first = strt(first, f='(I04)')
                last  = counter_num[wfirst[numfirst-1]]  & last = strt(last, f='(I04)')
                if last ne first then st_numW = first+'-'+last else st_numW = first
                ;       if last ne first then st_numW =  ' '+strtrim(first,2)+'-'+strtrim(last,2) else st_numW = first
                
                if keyword_Set(barycorr) then begin
                    printf,1, strcompress(st_numW,/rem)+'     ', ' ', _QUARTZ, i2[i], holder, JD, bary_correc,  st_exptime[i], binarr[i], slitarr[i],  ' ', propID[i], comment1[i], format=extended_form

                endif else begin
                    printf,1, strcompress(st_numW,/rem)+'     ', ' ', _QUARTZ, i2[i], holder,  st_exptime[i], binarr[i], slitarr[i],  ' ', propID[i], comment1[i], format=form1

                endelse
            
            endif
        endif ;i ne 0
        
        
        
        ;>> Printing for every file other than Flat

        if counter[i] eq 0 and endflag eq 0 then begin
          
            if keyword_Set(barycorr) then begin 
                printf,1, st_num, ' ', objName, i2[i], midtime,JD, bary_correc ,st_exptime[i], binarr[i], slitarr[i],  ' ', propID[i], comment1[i], format=extended_form
                print, st_num, ' ', objName, i2[i], midtime,JD, bary_correc,  st_exptime[i], binarr[i], slitarr[i], ' ', propID[i], comment1[i], format=extended_form
            endif else begin
                printf,1, st_num, ' ', objName, i2[i], midtime ,st_exptime[i], binarr[i], slitarr[i],  ' ', propID[i], comment1[i], format=form2
                print, st_num, ' ', objName, i2[i], midtime,  st_exptime[i], binarr[i], slitarr[i], ' ', propID[i], comment1[i], format=form2
            endelse
            
        endif

  endfor   ;for each observation 




  close,1 ; Close file

  file_count,dir=rawpath+rawdir+'/', prefix, missing
  print,'LOGMAKER: Your logsheet can be found at : '+logname
end
