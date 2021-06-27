
  
  
  ;+
  ; :NOTES:
  ; helper method for  removeCRs_sigma_clip
  ;
  ;
  ;-
pro clean_cosmic_rays, log, fileStructuresIdx, refStr
  compile_opt idl2


  ;####################################
  ;#1) Constants to be used
  ;####################################
  print, ' '
  print, 'CR_REMOVE: Cleaning all CRs from ' +refStr+ ' ....'
  print, ' '
  silent = 1
  limit_mad=9; pixel within a pixel set will be considered as a CR if the difference between 
               ; the median and the pixel value is more than (limit_mad) times its median deviation
  files_num = n_elements(log.filename[fileStructuresIdx]) ;  Field of structure was arbitrarily choosen
  IF files_num le 3 THEN stop, 'CLEAN_CR_PY: The number of ' +strt(refStr) + ' is not enough. Please, change your choice in the CTIO.PAR'

  n_columns =  fix(strt((log.naxis1[fileStructuresIdx])[0]   ), TYPE=3) ; Arbitrarily choosen strucutre
  n_rows    =  fix(strt((log.naxis2[fileStructuresIdx])[0]   ), TYPE=3)
   




; --------------------------------
;  1 : Calculate master median
; --------------------------------
  data_cube= make_array(n_rows, n_columns, files_num,/double )
  paths= log.filename[fileStructuresIdx]
  fileIdxs= 0
  foreach path, paths do begin
    data_cube[*,*,fileIdxs] = readfits(path, /silent)
    fileIdxs= fileIdxs+1
  endforeach
  master_median= median(data_cube, /double, dimension=3)


; --------------------------------
;  2 : Calculate the absolute deviations cube  abs(img_i -median )
; --------------------------------

  mad = data_cube ; median absolute deviation  as cube

  for index = 0L, fileIdxs-1 do begin
    mad[*,*,index]  = abs( data_cube[*,*,index] - master_median)
  endfor
  mad =median(mad, /double, dimension=3)
  zeroIndices = where(mad eq 0) ; Correcting in case any is zero 
  mad[zeroIndices] =1





; --------------------------------
;  3 : Iterate over each file and purge them with the median
; --------------------------------
  todayDate = SYSTIME()
  todayDate = strmid(todayDate,20,4) + strmid(todayDate,3,7)
    
  foreach path, paths do begin

    img_data = readfits(path,img_header, /silent)
    excludeIdxs = where( (abs(img_data - master_median) / mad) gt limit_mad , n_crs)
    img_data[excludeIdxs] = master_median[excludeIdxs]
    
    pos=stregex(path, 'chi([0-9]+)\.([0-9]+)\.fits$', length=len)

    print,"CR_ROMOVE:  Found  "  + strt(n_crs)+" CRs  in file : "+strmid(path, pos, len)+ '  ( '+strt(float(n_crs)/float(n_columns*n_rows))+' % of img)'
    history_str = 'CR-CLEANED : '+todayDate+' : ' + strt(n_crs)+ ' CRs removed using MAD = ' +STRT(limit_mad) + ' from : '  +strt(files_num) + '' + refStr +'files'
    ; The statement "CR-CLEANED" serves as reference to identify if the file has been cleaned previouly. Do not remove.

    sxaddpar, img_header, 'HISTORY', history_str
    writefits,  path, img_data, img_header


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


;+
; :Description:
;
;-
PRO removeCRs_sigma_clip, redpar, log
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
  ;  biasLogIdx    = where( (STRLOWCASE(strt(log.object)) eq 'bias'   ) and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  and  (log.crcleaned eq 0 )  , ctB)
  objectsLogIdx = where( (STRLOWCASE(strt(log.imgtype)) eq 'object') and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  and  (log.crcleaned eq 0 )  ,  ctO )




  ;####################################
  ;#1) Find cosmics Rays + Overwrite Files
  ;####################################

  totalCount = ctF  + ctO
                        
  if (totalCount  eq 0) OR (ctF lt 3)  OR  (ctO lt 3) then begin  
   ; There is nothing to clea. Also,do not remove CR later on with LaCosmic.
   ; This algorithim won't work. We take care of cosmic rays using LaCosmic
        PRINT,  ' '
        PRINT, '                                         || WARNING || '
        print, '  There were not enough valid files found to remove CRs using the Sigma Clipping strategy  '
        print, ' Please, change the variable | remove_crs | in the CTIO.PAR accordingly OR type  .cont  to keep running without CR removal.'
        print, ' Notice that files previously considered have been marked with the word CR-CLEANED in their fits header. These files will not be considered'
        print, ' You could remove this word from the headers of paste fresh copies in the raw directory. (Just be aware these files have been already cleaned ) '
        stop, ''
        return 
   endif 


  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' FLAT files. Please wait....'
  clean_cosmic_rays, log, flatsLogIdx,   'flats'
  ;  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' BIAS files. Please wait....'
  ;  clean_cosmic_rays, log, biasLogIdx,    'bias'
  PRINT, 'CLEAN_CR_PY: Cleaning Cosmic Rays from ' +strtrim(string(ctF),2 )+ ' STELLAR files. Please wait....'
  clean_cosmic_rays, log, objectsLogIdx, 'stellar'



  











end
