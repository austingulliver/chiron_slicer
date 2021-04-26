;+
; :NOTES:
; 
;
;-
pro clean_cosmic_rays, log, fileStructuresIdx, refStr
  compile_opt idl2
  
  
  ;####################################
  ;#1) Constants to be used
  ;####################################
  
  print, 'CLEAN_CR_PY: Cleanning all Cosmic Rays from ' +refStr+ ' ....'
  print, ' '
  silent = 1
  np     = Python.Import('numpy')
  astropy  = Python.Import('astropy.stats')
  limit_mad= 3 ; a pixel value needs to be 'limit_mad' times bigger than the rest of the pixel values to be identified as an outlier
               ; Previously -10
  
  
  
  files_num = n_elements(log.filename[fileStructuresIdx]) ;  Field of structure was arbitrarily choosen
  IF files_num le 3 THEN stop, 'CLEAN_CR_PY: The number of ' +strt(refStr) + ' is not enough. Deactivate this option in CTIO.PAR'
  
  n_columns =  fix(strt((log.naxis1[fileStructuresIdx])[0]   )) ; Arbitrarily choosen strucutre
  n_rows    =  fix(strt((log.naxis2[fileStructuresIdx])[0]   ))

  ;#################################################################
  ;# 2) Create Median Master bias + Master Median Absolute Deviation
  ;#################################################################

   
     
   paths= log.filename[fileStructuresIdx] 
  
   data_cube= np.zeros(Python.tuple( [files_num, n_rows , n_columns] ) ) ;CAREFUL this gets changed 
   counter =0
   foreach path, paths do begin
      dataImg = readfits(path, /silent)  
      data_cube[*,*,counter] = dataImg
      counter= counter +1
   endforeach



  master_median = np.median(data_cube, axis=0 )
  master_abs_mad = astropy.funcs.mad_std(data_cube, axis=0 )

;  #################################################################
;  # 2) Cleaning CR from every image + overwriting on disk
;  #################################################################

  todayDate = SYSTIME()
  todayDate = strmid(todayDate,20,4) + strmid(todayDate,3,7) 
  
  foreach path, paths do begin
    img_data = readfits(path,img_header, /silent) 
    img_data =np.array(img_data)
    
  
    
    exclude = ((img_data - master_median) / master_abs_mad) gt limit_mad
    
    n_crs = np.count_nonzero(exclude)
    
    img_data[exclude] = 0
    mask = np.multiply( exclude , master_median)
    img_data = np.add(img_data,  mask) 
    
    writefits,  path, img_data, img_header  
    
    print,"CLEAN_CR_PY: File : "+path+  "had "+strtrim(string(np.count_nonzero(exclude)),2)+" Cosmic Rays. It has been cleaned and overwritten successfully."
    
    
    history_str = 'CR-CLEANED : '+todayDate+' : ' + strt(n_crs)+ ' CRs removed using MAD = ' +STRT(limit_mad) + ' from : '  +strt(files_num) + '' + refStr +'files'
    ; The statement "CR-CLEANED" serves as reference to identify if the file has been cleaned previouly. Do not remove.

    sxaddpar, img_header, 'HISTORY', history_str
    
    

  endforeach

end





;+
; :NOTES:
;        This procedure has a dependency (clean_cosmic_rays) which  makes use of the Python Bridge.
;        Please make sure you have python running in your
;        system + the following libraries isntalled  :  astropy, numpy
;
;        
;        
;        To change the threshold to pick Cosmic Rays,change the value of limit_mad in clean_cosmic_rays procedure.
;
; :INPUT:
;        log    : Data structure built with information about each rawa file
;        redpar : Data structure contaning initial variables set to run the program
;         
;
; :OUTPUT:
;        *All files are overwritten with their clean version (no cosmic rays )  
;        remove_cr : In case  the files  were already cleaned or there are not enough files to clean cosmic rays 
;                   this variables is passed for future reference.
;        
; :HISTORY :
;        2021-15-02 : Written by J. Andres Lozano 
;
;
;-
FUNCTION clean_cr_py, redpar, log
  compile_opt idl2

 
  ; >> Parse binning string as needed.
  binningRaw = redpar.binnings[redpar.mode]
  bin1       = binningRaw.CharAt(0)
  bin2       = binningRaw.CharAt(binningRaw.StrLen()-1)
  binning    = bin1 +' '+bin2
  ;print, log.filename + '< >'+log.object + '< >' +log.ccdsum +  '< >' +log.decker +'< >' +log.imgtype +'|            '
  
  ;####################################
  ;#1) Create 3 lists: flats, biases and stellar
  ;####################################
  
  flatsLogIdx   = where( (STRLOWCASE(strt(log.object)) eq 'quartz' ) and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  and  (log.crcleaned eq 0 )  , ctF)
  biasLogIdx    = where( (STRLOWCASE(strt(log.object)) eq 'bias'   ) and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  and  (log.crcleaned eq 0 )  , ctB)
  objectsLogIdx = where( (STRLOWCASE(strt(log.imgtype)) eq 'object') and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  and  (log.crcleaned eq 0 )  ,  ctO )


  
  
  ;####################################
  ;#1) Find cosmics Rays + Overwrite Files
  ;####################################
  
  totalCount = ctF + ctB  + ctO
  if totalCount  eq 0 then RETURN, 0                        ; There is nothing to clea. Also,do not remove CR later on with LaCosmic.  
  if (ctF lt 3)  or  (ctB lt 3) or  (ctO lt 3) then RETURN, 1 ; This algorithim won't work. We take care of cosmic rays using LaCosmic
   
 
  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' FLAT files. Please wait....'
  clean_cosmic_rays, log, flatsLogIdx,   'flats' 
  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' BIAS files. Please wait....'
  clean_cosmic_rays, log, biasLogIdx,    'bias'
  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' STELLAR files. Please wait....'
  clean_cosmic_rays, log, objectsLogIdx, 'stellar'
    
 
  
  RETURN, 0
  
 

  
  
  
  
END



