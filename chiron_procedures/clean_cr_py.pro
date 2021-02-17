





;+
; :Description:
;
;-
pro clean_cosmic_rays, log, fileStructuresIdx, refStr
  compile_opt idl2
  
  
  ;####################################
  ;#1) Constants to be used
  ;####################################
  
  print, 'CLEAN_CR_PY: Cleanning all Cosmic Rays from ' +refStr+ ' ....'
  print, ' '
  silent = 0
  np     = Python.Import('numpy')
  astropy  = Python.Import('astropy.stats')
  limit_mad= 10 ; a value need to be 'limit_mad' times bigger than the rest of the values to be identified as outlier
  
  
  

files_num = n_elements(log.filename[fileStructuresIdx]) ;  Field of structure was arbitrarily choosen
IF files_num le 3 THEN stop, 'CLEAN_CR_PY: The number of ' +strt(refStr) + ' is not enough. Deactivate this option in CTIO.PAR'

n_columns =  fix(strt((log.naxis1[fileStructuresIdx])[0]   )) ; Arbitrarily choosen strucutre
n_rows    =  fix(strt((log.naxis2[fileStructuresIdx])[0]   ))

;  #################################################################
;  # 2) Create Median Master bias + Master Median Absolute Deviation
;  #################################################################

   
   
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
;  # 2) Cleaning CR from every image + re writing on disk
;  #################################################################

  todayDate = SYSTIME()
  
  foreach path, paths do begin
    img_data = readfits(path,img_header, /silent) 
    img_data =np.array(img_data)
    
    history_str = 'CR-CLEANED : Cosmic Rays removed from this file on '+todayDate+' by finding the MAD of each pixel in '+string(files_num)+ ' ' +refStr +' files.'
    sxaddpar, img_header, 'HISTORY', history_str
    
    exclude = ((img_data - master_median) / master_abs_mad) gt limit_mad
    img_data[exclude] = 0
    mask = np.multiply( exclude , master_median)
    img_data = np.add(img_data,  mask) 
    
    writefits,  path, img_data, img_header  ; DOES THIS OVERWRITE?
    if silent eq 0 then print,"CLEAN_CR_PY: File : "+path+  "had "+string(np.count_nonzero(exclude))+" Cosmic Rays. It has been cleaned and overwritten successfully."
    

  endforeach

  


  









end


;+
; :NOTES:
;        This procedure makes use of the Python Bridge. Please make sure you hav python running in your
;        system + thefollowing libraries :  glob, astropy, numpy, datetime
;
;        E.g.   >> clean_cr_py ( 'sirius','slicer' , '3 1')
;        
;        If you want to change the threshold to pick CR then  change the value of limit_mad in clean_cosmic_rays
;
; :INPUT:
;        log : is a data sctructure built with information about each file
;        redpar : Data structure contaning initial variables set to run the program
;         
;
; :OUTPUT:
;        *All files are overwritten with their clean version (no cosmic rays )  
;        remove_cr :
;        
; :HISTORY :
;        2021-15-02 : Written by J. Andres Lozano 
;
;
;-
PRO clean_cr_py, redpar, log=log, remove_cr =remove_cr
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
  
  flatsLogIdx   = where( (STRLOWCASE(strt(log.object)) eq 'quartz' ) and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')    , ctF)
  biasLogIdx    = where( (STRLOWCASE(strt(log.object)) eq 'bias'   ) and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')   ,ctB)
  objectsLogIdx = where( (STRLOWCASE(strt(log.imgtype)) eq 'object') and (strt(log.ccdsum) eq binning) and  (STRLOWCASE(strt(log.decker)) eq 'slicer')  , ctO )


  
  ;####################################
  ;#1) Find cosmics Rays + Overwrite Files
  ;####################################

  clean_cosmic_rays, log, flatsLogIdx,   'flats'
  clean_cosmic_rays, log, biasLogIdx,    'bias'
  clean_cosmic_rays, log, objectsLogIdx, 'stellar'
  
  remove_cr=remove_cr

END



