pro qbarylog,logfile, test=test, baryDir=baryDir, prefix=prefix , justtest=justtest, simbadStartName = simbadStartName
; PURPOSE: Calculate Barycentric Correction for Stellar spectra.
; 	    
; INPUT : 
;        logfile -path of .log file created by logmaker.pro.  Absolute Path 
;        baryDir: Your barycentric directory  E.g. .....\tous\mir7\bary\ (include slash at the end.) or setup CHIRON_PATH
;        
;        simbadStartName  (string) : If set makes a request to SIMBAD   http://simbad.u-strasbg.fr/simbad/ 
;                                    instead of looking for star information using LOOKUP.PRO;                       
;                                :  String is the name of the star  
;                                :  IF is not successful it might have been 'cause name of the start does not match Simbad's notation 
;                                :  Do the search in  http://simbad.u-strasbg.fr/simbad/ and pass the name of the star in this variable 
;        
;          
; 	    
; OPTIONAL KEYWORDS:
;		JUSTTEST: Set this keyword if you want to just check to make sure
;		things are working, but NOT print to qbcvel.ascii
;
;   METHOD: 1.) Obtain Object Name, FLUX-WIEGHTED mean epoch, etc. from
;              online logsheets.
;           2.) Star  positions are stored in: /mir7/bary/ctio_st.dat,  hip.dat,
;              kother.ascii (ktranslation.dat rarely used to find HIP numbers)
;           3.) Drive qbary.pro, which makes calculation.
;           4.) Append qbcvel.ascii file


; HISTORY:
    ;Create: NOV-7-93	ECW
    ;Modified: JUL-95	CMc.   Modified to Drive New Barycentric Routines 
    ;Modified; JAN-96       ECW    Modified to get input info from online 
    ;			       logsheets.  Currently setup to run on 
    ;                              the machines: hodge,quark,coude.
    ;Modified; Jan-97       GWM    cleaned up, made identical to Keck version
    ;Modified; Nov-97       GWM    Auto-read log lines; Invoke hipparcos catalog
    ;Modified; Nov-01   CMc    To accomodate 1000+ image seismology runs
    ;                               & general cleanup
    ;Modified; May-02   CMc    To include remove acc. calc. + other improvements
    ;Modified; Feb-05   CMc:
    ; --    To execute bcvel.ascii backups into bcvel_backup/ directory
    ; --    Convert From kbarylog to barylog for CTIO.
    ; --    "HR" designated stars do not have BC's computed.
    ;
    ;Modified; Mar-09	JMB		
    ;	log files should be passed with complete paths, this script will determine
    ;	the directory the file lives in and the run prefix
    ;
    ;Modified; Jan-10	JMB
    ;	updated to examine multiple log-file date formats and notice wether the UT
    ;	date changed during the night.  If it did, later on we increment the day
    ;	for any obs which have times < 14 hours.
    ;
    ;Modified; Feb-10	JMB
    ;	updated strndgen to format integers between 0-9999 as 4 digit strings
    ;	with leading zeros.
    ;
    ;Modfied; May-10	JMB
    ;	updated log-file date format code to notice when date  didn't change but
    ;	log file date format _appeared_ to have two dates (ie 2010 May 11/11)
    ;
    ;Modified: 20111012 ~MJG
    ;	the formating of what is printed is cleaner and more robust. Prior to this 
    ;	modification the routine was failing to include the full filename now that
    ;	the image prefixes are longer.
    ;
    ;Modified: 20111111 ~MJG
    ;	added a few items to the ThAR and list of items to exclude.
    ;
    ;2021-Jan -31 - Jorge Lozano
    ; Added comments - Understanding logic behind + updated outadated methods. 
;     Adjusted varibles to ommit secular acceleration correctio 
;NOTES:
    ;At least 1 log file is required
;   E.g. qbarylog, 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\logsheets\2017\171218.log', prefix= 'chi' ,simbadStartName='Sirius'


    ; Validation + Setting paths + default variables
    ; ---------------------------------------------------
    if  n_elements(logfile) eq 0 then stop,  "qBaryLog-ERROR:  No logfiles found to process." 
    
    if  ~ keyword_set(baryDir)  then begin ; If path for barycentric direcotry is not pass and find path.
          chi_path=getenv('CHIRON_PATH')   ; This must the absolute path where the directory chiron was placed
                                           ; E.g.  SETENV, 'CHIRON_PATH=.......\chiron'
          if strlen(chi_path)  ge 1 then begin
            baryDir = chi_path.trim() + '\tous\mir7\bary\'
            if ~file_test(baryDir) then spawn, 'mkdir ' + baryDir
          endif else begin
            spawn, 'hostname', host
            if  host eq 'Drius22' then begin
              baryDir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\bary\'
            endif else stop, ' Set up the environment variable CHIRON_PATH or add your path to the script list to continue.'
          endelse      
    endif ; Else baryDir was defined by user 
    
    
    
    ; >> Extract Date from .log file name
    if STRPOS(logfile, '.log') ne -1 then begin
      nightDate=strmid(logfile, STRPOS(logfile, '.log')-6,6 ) 
    endif else STOP, 'The .log path must contain the extension .log   '
    
       
    
    
    if ( keyword_set(test) ) then baryFile = nightDate+'.qbcvel.test.ascii' else baryFile = nightDate+'.qbcvel.ascii'    
    bcFile = baryDir + baryFile    
    structFile = baryDir+'ctio_st.sav' ;'This is a IDL variable containning info about observation ; 
                                        ; Has two variables  CTIO and CAT 
    
    dummy=File_Search(bcFile,count=nFiles) ; Only checking for bcFile
    if nFiles gt 0 then begin
       answer=''
       print,'Hey! this file already exists - are you sure you want'
       read,' to overwrite? (y/n)', answer
       if answer eq 'n' then stop else SPAWN, 'copy NUL '+bcFile
    endif else SPAWN, 'copy NUL '+bcFile
    
    ; >> Verify that the neccessary files exist    
    if (n_elements(file_search(structFile)) eq 0) then stop, structFile+ ' is missing.'
    ;if (n_elements(file_search(bcFile)) eq 0) then message,bcFile+ ' is missing.'
    
    ;>> Variable declarations:
    noerror=1	& chk=''		& du=''		& dum=''	& dummy=''	&  req=''  
    log=''		& logline=''	& obtype='' & mid=''
    year=0		& month=0		& day=0		& hr=0		& sc=0		& epoch=0
    
    skiplist= ['WIDEFLAT','WIDE','WF','W','WIDE-FLAT','WIDE/FLAT','W/F', $
               'WIDEFLATS','WIDES','WFS','WIDE-FLATS','WIDE/FLATS', $
               'WIDE_FLAT','FLAT','JUNK', 'FOCUS','quartz','QUARTZ','Quartz',$
               'QTZ', 'qtz', 'Qtz','FLATFIELD','SATURATED','SKIP', 'BIAS', 'DARK']
    iodlist = ['IODINE','I','I2','IOD','QTZIODINE', 'iodine']
    thorlist = ['TH-AR','TH_AR','THAR','TH/AR','THORIUM','THORIUM-ARGON','hear', 'th-ar']
    daylist = ['DAY','DS','DAYSKY','DAY-SKY','DAY_SKY']
    moonlist = ['moon', 'Moon','MOON']
    skylist = ['SKY', 'DARK','BIAS','narrowflat','narrflat','SUN','Sun','sun','SKYTEST']
    months = { Jan: '01', Feb: '02', Mar: '03', Apr: '04', May: '05', Jun: '06', Jul: '07', Aug: '08', Sep: '09', Oct: '10', Nov: '11', Dec: '12' }
    monthTags = tag_names(months)
    
    ;>> Define structures
    maxsize = 5000                  ; 
    log = {log, object: '', hour: '', min: '', sec: '', type: ''}
    temp = {bcvel, filename:'', object:'', cz:0.d0, mjd:0.d0, ha:0.d0, type:''} ;Temporary structure for results
    temp = replicate(temp[0],maxsize)
    
    tempbcfile = strarr(maxsize)    ;temporary storage of ascii results: 200 lines
    if ~keyword_set(simbadStartName) then simbadStartName= ''
    alreadySimbad= 0                ; Use to check if simbad request was already made 
    
    print,'     *************************************************************'
    print,'     **      THE CTIO BARYCENTRIC CORRECTIONS PROGRAM           **'
    print,'     *************************************************************'                                         
    print,'     Input Logsheet : ' + logFile 
    print,'     Outout         : '  +  bcfile 
    print,'     *************************************************************'
    
    
    
    
    
    
    
    ; Reading .log file 
    ; ---------------------------------------------------
    
    
    logFileParts = stregex(logFile,'^(.*\\)([0-9]+\\.+log)$',/EXTRACT,/SUBEXPR) ; This has been modified to be Windows compatible
                                                                                ; For linux use : '^(.*/)([0-9]+/.+log)$'
  
    
    if ( n_elements(logFileParts) ne 3 or logFileParts[2] eq '' ) then message, "New logfile format or Unable to locate a valid log file: "+logFile
    
    logDir = logFileParts[1]
    logFile = logFileParts[2]    
    logfileorig = logfile    
    
    
    restore,structFile & cat = dum   ;Restore coord. structure
    
    ; spit out the file and grab the UT Date line
    ;tlogdir=STRMID(logdir, 1) 
    
   
    spawn, "type "+ string(34B) + logDir + logfile + string(34B) , output  ;updated for Windows   : output is the content of the .log sheet  

    output = cull(output)                        ; remove any blank lines  
    

    logline = (output(where(getwrds(output,0) eq 'UT')))[0] ; 1 element
    
    
    
   
    
    ; Extrat date information from .log file header 
    ; ---------------------------------------------------
    
    
    ; >> OLD VERSION  (To be confirmed)
    

    if strmid(logfile,0,1) ne '1' then begin   ; DF revision Mar2012
        if ( strlen(logline) eq 0 ) then stop,'no proper format for logline'   ; it was ne before WHY ?
        
        ; We have several possible log file date formats, below we have regexes for all of those
        ;	which describe log-files with multiple dates
        ;
       flip = 0                     ; check for a changing UT
       dateRegexes = [ $
    		{	regex: 'UT Date: ([0-9]{4})[^a-zA-Z]+([a-zA-Z]{3})[^0-9]+([0-9]+)\/([0-9]{4})[^a-zA-Z]+([a-zA-Z]{3})[^0-9]+([0-9]+)', $ ; changing year
    			flip: 1, namedMonth: 1, checkDay: 0 }, $
    		{	regex: 'UT Date: ([0-9]{4})[^a-zA-Z]+([a-zA-Z]{3})[^0-9]+([0-9]+)\/([a-zA-Z]{3})[^0-9]+([0-9]+)', $ ; changing month
    			flip: 1, namedMonth: 1, checkDay: 0 }, $
    		{	regex: 'UT Date: ([0-9]{4})[^a-zA-Z]+([a-zA-Z]{3})[^0-9]+([0-9]+)\/([0-9]+)', $ ; changing day, same month
    			flip: 1, namedMonth: 1, checkDay: 1 }, $
    		{	regex: 'UT Date: ([0-9]{4})[^a-zA-Z]+([a-zA-Z]{3})[^0-9]+([0-9]+)[^\/]', $ ; single ut date
    			flip: 0, namedMonth: 1, checkDay: 0 }, $
    		{	regex: 'UT Date: ([0-9]{4})[ \t]+([0-9]{2})\/([0-9]{2})-([0-9]{2})\/([0-9]{2})', $ ; numeric instead of string dates
    			flip: 1, namedMonth: 0, checkDay: 0 } $
    	]
       i = 0                        ;
       dateMatch = 0                ;
       while ( i lt n_elements(dateRegexes) and dateMatch eq 0 ) do begin
          	if ( stregex(logline,dateRegexes[i].regex,/BOOLEAN) ) then begin
          		dateReg = dateRegexes[i].regex
          		dateMatch = 1
          		flip = dateRegexes[i].flip
          		namedMonth = dateRegexes[i].namedMonth
          		checkDay = dateRegexes[i].checkDay
          	endif
          	i = i + 1
       end
    
       if ( ~dateMatch ) then stop,'UT date was in an unknown format'
    
    ; Extract the YYYY MMM DD from the date
       logFileDate = stregex(logline,dateReg,/EXTRACT,/SUBEXPR)
       year = logFileDate[1]
       m = where(monthTags eq strupcase(logFileDate[2]))
       if ( namedMonth ) then month = months.(m) else month = m
       day = logFileDate[3]
       if ( checkDay && logFileDate[4] eq logFileDate[3] ) then flip = 0 ; somehow the format was two day even though there was only 1 day (ie May 11/11)
       
    endif ;reading old format logfile for dates
    
    
    
    
    ; >> NEW VERSION  (To be confirmed)
    if ( strmid(logfile,0,1) eq '1' ) then begin ; DF revision Mar2012  : New version ? ()
       flip = 1  ; add a day to get correct UT
       year = '20'+strmid(logfile,0,2)
       m = strmid(logfile,2,2) 
       month = m
       day = strmid(logfile,4,2) 
    endif
    
    
    
    
    
    
    
    
    
    
    ans='y'
    ;Save Backup copy of bcvel.ascii
    if ( ~ keyword_set(test) ) then begin
        strdate =strtrim(strcompress(day),2)+'_'+strtrim(strcompress(month),2)+$
          '_'+strtrim(strcompress(year),2)
        command = 'cp "'+bcFile+'" "'+barydir+'bcvel_backup/'+baryFile+'_'+strdate+'"'      ;TO FIX  This command has to change for Windows  
    ;    spawn,command
    endif  else print,'NOT Backing up!  Since this version of bary is in test mode!'
    
    ; Open the log file and get the prefix from the header area,
    ; continue through the whole file getting all of the entries so that we can
    ; make sure this log sheet wasn't already run.
    ;openr, LOGHEAD,logDir+logFile,/GET_LUN                                  ;UNCOMMENT ONLY THIS LINE
    ;prefix = ''
    ;while ( eof(LOGHEAD) eq 0 and prefix eq '' ) do begin
    ;	readf,LOGHEAD,logLine
    ;	if ( strpos(logLine,'prefix:') gt -1 ) then begin
    ;		prefix = stregex(logLine,'prefix:.([a-zA-Z0-9]+)',/EXTRACT,/SUBEXPR)
    ;		prefix = prefix[1]
    ;		break
    ;	endif
    ;endwhile
    ;close,LOGHEAD
    ;if ( prefix eq '' ) then message, "Unable to find the run prefix (ie qa04)"
    ;print,day
    
   
    
    num = 0
    skip = 0                        ;Reset to NOT skip
    strindgen = strtrim(string(indgen(10000),FORMAT='(I4.4)'),2)
    
    if ( keyword_set(test) ) then begin
    	print,' '
    	print,'  Filename      Object     I2 in?   Time   Barycentric Corr. (m/s)'
    	
    	print,'---------------------------------------------------------'
    endif
    
    openr,logune,logdir+logfile,/get_lun ; Open the logsheet
    
    
    
    
    
    
    
    ;Loop through each line of the logsheet
    ;--------------------------------------
    
    WHILE eof(logune) eq 0 do begin ;check for end of file (eof = 1)
            readf,logune,logline        ;read one line in the logsheet
            
;            print, ''
;            print, ''
;            print, '>> The present line is : '
;            print, logline
            
        	 ;>> Read the first four entries on the line.
            recnum = strtrim(getwrd(logline[0],0),2)              ;Observation Number number        		
            log.object = strtrim(strupcase(getwrd(logline,1)),2)  ;Object name
;            print,'Object name :' +string(log.object)
            if log.object eq 'LHS2627' then log.object = 'LHS2726'
            first2 = strupcase(strmid(log.object,0,2))            ;First 2 letters of the object name
            celltest = strtrim(strupcase(getwrd(logline,2)),2)    ;Iodine Cell Test (y/n)
            strtime = strtrim(getwrd(logline,3),2)                ;Time from logsheet (Mid Time UT)
            linelen = (strlen(logline))[0]                        ;Length of the line, used for line validation 
            temptest = strpos(strupcase(logline),'TEMP')          ;test for word "Template". If exists otherwise -1
        
        	 ;>> Construct reduced filename
            filename = prefix + '.' + recnum
           
          
           
        	 ;>> Guarantee that this is really an observation of something useful. E.g. Header gets thrown away in here.
            IF ((celltest eq 'Y' or celltest eq 'N') and $   ; Was cell specified?
               (select(skiplist,log.object) ne 1)    and $   ; Not wide flat nor skiplist?
                select(strindgen,recnum))            and $
                (linelen gt 1)                  THEN BEGIN 
            
                    if (celltest eq 'Y')   then  log.type='o' else log.type='t'           
                    if select(iodlist, log.object)  then begin 
                       log.type='i' & log.object='iodine'  ;                       
                    endif 
                    if select(thorlist,log.object) then begin
                      log.type='u' & log.object='th-ar'
                    endif
                    if select(daylist, log.object)   then  begin
                      log.type='s' & log.object='day_sky'
                    endif
                    if select(moonlist,log.object) then begin
                        log.type='o' & log.object='moon'   
                    endif                 
                    if select(skylist, log.object) then begin 
                        log.type='u'
                    endif        
            
                    if temptest[0] ne -1 and log.type ne 't' then begin ; Error Trap
                        print,'****WARNING:  Possible Template Detected: '
                        print,logline
                        print,'But observation type is not "t".  Was I2 cell in?'
                        help,log.type
                    endif
            
            
            		; >> Definning UT-time extracted from each line
                    colon1 = strpos(strtime,':')                         ;position of first colon
                    colon2 = rstrpos(strtime,':')                        ;postiion of last colon
            
                    strhour = strmid(strtime,0,colon1)                   ;UT hour    of observation
                    strminute = strmid(strtime,colon1+1,colon2-colon1-1) ;UT minute
                    strsecond = strmid(strtime,colon2+1,10)              ;UT second
            
                    hour = float(strhour)
                    minutes = float(strminute) + float(strsecond)/60.d0
            
                ;fischer added next 2 lines to try to adjust for two UT's 
                    if flip eq 1 then begin 
                       dd=day
                       if hour lt 14. then dd=dd+1
                    endif
                    if flip eq 0 then dd=day
                    jdUTC = jdate([year,month,dd,hour,minutes])
                    mjd = jdUTC-2440000.d0  ; modified JD
            
            		; Fix lengths of output strings
                    len = (strlen(filename))[0]
                    if len lt 9 then for jj=0,9-len-1 do filename = filename + ' '
                    obj = log.object
                    len = (strlen(obj))[0]
                    if len lt 8 then for jj=0,8-len-1 do obj = ' '+obj 
                    if strlen(strminute) eq 1 then strminute = '0'+strminute 
                    if strlen(strsecond) eq 1 then strsecond = '0'+strsecond 
                    strtime = strhour+':'+strminute+':'+strsecond
                    len = (strlen(strtime))[0]
                    if len lt 9 then for jj=0,9-len-1 do strtime = strtime + ' '
                                            
                    czi=0.0;cz = 0.0d0 
                    ha = 0.d0
                    filename = filename
                        
                   
                    ; >> Run BARYCENTRIC correction if type matches
                    IF select(['o','t'],log.type) then begin 
            
                          ;LOOKUP COORDINATES: lookup.pro takes starname (log.object) and finds
                          ;                     coords, equinox, proper motion, parallax 
                          ;                      coords=[ra,dec], pm=[ra_motion, dec_motion]
                         
                         ;print,filename, ' ', log.object
                         if first2 ne 'MO' then begin
                            ;if first2 ne 'HR' then begin ; SKIP Bright STARS, MOON (no B.C. for B*s). We have commented this since we are interested in Brigth stars 
                             
                             
                              ; ***********************************************
                              ;  Search information needed for star
                              ; ***********************************************
                              
                              if strlen(simbadStartName)  ge 1 then begin 
                                 
                                 if alreadySimbad eq 1 then  begin 
                                    ; Since there is only 1 star per .log sheet this only needs to be called once after already run 
                                    ; The same vairables ra, coords, pm, epoch will be stored and used
                                 endif else begin
                                   print, 'QBARYLOG:  Requesting start information to simbad '
                                   QuerySimbad, simbadStartName , ra, dec, found=found
                                   
                                   if  found eq 0  then stop, 'QBARYLOG:  >>  The used star name '+strtrim(simbadStartName,2)+' was not found. Try looking up it here first: http://simbad.u-strasbg.fr/simbad/' 

                                   ra= ra* (24.0/360.0)      ; Transformation from degrees to hours
                                   coords = [ra,dec]
                                   pm = [0.d0,0.d0]          ; dummy proper motion
                                   epoch =2000.d0
                                   alreadySimbad= 1
                                   print, ''
                                   print, ' Start Information used for Correction : '
                                   print, '*****************************************************************'
                                   print, 'coords : ' + string( coords) ; RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]
                                   print, 'pm     : ' + string(pm)       ; proper motion [ra,dec] in ARCsec/year [optional]
                                   print, 'epoch  : ' + string(epoch)
                                   print, '*****************************************************************'
                                   print,  '' 
                                   
                                 endelse
                               
                                 
                                 
                               
                              endif else begin ; Prior code Legacy          
                                  
                                
                                   lookup,log.object,coords,epoch,pm,parlax,radvel,hip=hip,$
                                          barydir=barydir,cat=cat,tyc=tyc
                                   if abs( coords(0)) eq 99.d0 then begin ;Logsheet star not found
                                      coords = [0.d0,0.d0]                ;force ra and dec = 0. :no object found
                                      pm     = [0.d0,0.d0]                ;dummy proper motion
                                      epoch = 2000.d0                     ;dummy epoch
                                   endif 
                                
                                
                              endelse

                                 
;                                >>Input for qbary.pro
;                                 print,  ' ------------------------------------   ' 
;                                 print, 'coords : ' + string( coords) ; RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]
;                                 print, 'pm     : ' + string(pm)       ; proper motion [ra,dec] in ARCsec/year [optional]
;                                 print, 'epoch  : ' + string(epoch)                                 
;                                 print, 'barydir: ' + string(barydir)   ; Barycentric directory
;                                 print, 'jdUTC  : ' + string(jdUTC)    ; julian date (double precision) eg 2448489.3462d0 
;                                 
                                  ; ***********************************************
                                  ;  Run Correction 
                                  ; ***********************************************
                                  qbary,jdUTC,coords,epoch,czi,obs='ctio',pm = pm,barydir=barydir, ha=ha
                                        
;                                 >>Output of qbary.log
;                                 print, 'ha : hour angle of observation '+string(ha)
;                                 print, 'czi :relativistic redshift' +string(czi)
                                 
                            
                                 
                                  
                                        
                                        
                                  ; ***********************************************
                                  ;  Secular Acceleration 
                                  ; ***********************************************      
                                  ;cz = rm_secacc(czi,pm,parlax,mjd)    ; We are not interested in the seculaar acceleration UNCOMMENT otherwise + change reference from czi to cz (thorugh out whole script)
                                  
                               ;endelse
                            ;endif             ; else print,'Skipping Brigth Stars'
                         endif  ;skipping Moon
                    ENDIF   
            
            
                
                ;>> Print log to be created IF test
            
            		if ( keyword_set(test) ) then begin
            			;stcz = strtrim(string(cz),2)
            			stcz = strtrim(string(czi),2)
            			len = (strlen(stcz))[0]
            			if len lt 7 then for jj=0,7-len-1 do stcz = ' '+stcz
            			infoline = '|  '+filename+' |  '+obj+' |  '
            			infoline = infoline + celltest+'  | '+strtime+' | '+stcz+' |'
            			k = (strlen(infoline))[0]-1
            			dashln = '-'
            			for p = 1,k do dashln = dashln+'-'            			
            			print,infoline &  print,dashln
            		endif
            
            
            		;>> Store results to Structure
            		
                temp[num].filename = filename
                temp[num].object = log.object
                temp[num].cz = czi ;cz
                temp[num].mjd = mjd
                temp[num].ha = ha
                temp[num].type = log.type
                num=num+1
                    
                    
                    
            ENDIF                        ;Guarantee that this is really an observation
            
    endwhile                             ;Iteration over every line of logsheet
    
    
    
    
    
    
    
    
    
    
    
    
    
    ;User interaction for BCVEL.ASCII validation 
    ;--------------------------------------
   
    temp = temp[0:num-1]            ;trim temp structure array
    if ( keyword_set(test) ) then begin
    	print,' '
    	ans = ' '
    	read,'Did all the printed results (above) look OK? (y or n)?',ans
    endif else ans = 'Y'
    
    if strupcase(ans) eq 'Y' then begin
        get_lun,une                 ;get Logical Unit Number
        ;openu,une,bcfile,/append    ;open bcvel file for writing
        openu,une,bcfile ;/append    ;open bcvel file for writing
    ;    form = '(A9,3X,A10,1X,D11.3,1X,D12.6,1X,F7.3,1X,A1)'; modified for
    ;            fn, object, cts, mjd, ha, type
        form = '(2X, A-16,A12, D12.3,D13.6,F8.3,A2)' ; long names
        
        print,'Printing results to '+bcfile+' ...'
        printf,une,'-------------------------------------------------------------------------------------'
        printf,une, ' NIGHT : '+nightDate
        printf,une, ' Correction: c * z in [m/s].  (z = relativistic redshift)'
        printf,une,' FileName              Object    Correction   ModJD         HA   fileType  
        printf,une,'-------------------------------------------------------------------------------------'
        for j=0,num-1 do begin
            fn = temp[j].filename
            ob = temp[j].object
            cz = temp[j].cz
            mjd = temp[j].mjd
            ha = temp[j].ha
            type = temp[j].type
            if ~keyword_set(justtest) then begin
    		  printf,une,format=form,fn,ob,cz,mjd,ha,type
            endif
            print,format=form,fn,ob,cz,mjd,ha,type
            ;stop
            
        end
        free_lun,une
        print,'Done with '+logfileorig
        print,' '
        comm=' '
        if num gt 55 then comm = 'You can have your pancakes now.'
        if num lt 30 then comm = 'Warm up the maple syrup.'
        print,' You observed ',strcompress(num),' stars. '+comm
        print,' '
        print,'It''s been a long night. Go to sleep!  Sweet Dreams.'
    endif else begin
        print,' '
        print,'Make necessary changes (to logsheet?) and start again.'
        print,'The file ',bcfile,' was not affected. Exiting ...'
    endelse
    close,/all
    free_lun,une 

END


