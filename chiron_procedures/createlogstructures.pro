
FUNCTION isCleaned, history
  ;+
  ; :Description:
  ;  Is used to find out if file was already cleaned from CR.
  ;  Returns 0 if file was not already cleaned and 1 if it was.
  ;
  ;-
  compile_opt idl2
  
 
  sz= size(history)
  
  if sz[0] eq 0 then return, 0 else begin    
    cleanedArray = history.Contains('CR-CLEANED')    
    dummy = where( cleanedArray eq 1, count)    
    if count ge 1 then return, 1 else return, 0    
  endelse
END






; NOTES:
;         This procedure was created as a work-around since some files were missing.
;         Creates the missing procedure that creates a list of log structures based on the log sheet and the fits header of a given file
;         ideally this has to be an isoloted procedure since waiting time is long
;INPUT :
;         night   int(6)  data of the night
;
;OUTPUT :
;         does not return any value. It creates a file which contains the log structure
;
;  Create by Jorge Lozano - 2021/02/14


PRO createLogStructures, redpar,obnm,objnm, doFromScratch = doFromScratch

checkPath    = redpar.rootdir+redpar.logstdir+'20'+strmid(redpar.date, 0, 2)+'\'
fileSavePath = checkPath+redpar.date+'log.dat'
rawDirFiles  = redpar.rootdir + redpar.rawdir +redpar.imdir +redpar.prefix



if ~file_test(fileSavePath) or keyword_set(doFromScratch) then begin
  
      log= {object:strarr(n_elements(objnm) ), ccdsum:strarr(n_elements(objnm) ),speedmod:strarr(n_elements(objnm) ),naxis1: FLTARR(n_elements(objnm) ) ,naxis2 : FLTARR(n_elements(objnm) ) ,filename:strarr(n_elements(objnm) ), decker:strarr(n_elements(objnm) ), imgtype:strarr(n_elements(objnm) ), crcleaned:intarr(n_elements(objnm))}


      print, 'CREATELOGSTRUCTURES: Extracting information from the raw data. Please wait....'
      if n_elements(objnm) ne 0 then begin
        for idxFile=0, n_elements(objnm) -1 do begin
          ;pathFile= rawDirFiles+obnm[biasFileIndex[idxFile]]+'.fits'
          pathFile= rawDirFiles+obnm[idxFile]+'.fits'
          dummy = readfits(pathFile, hdr)
          log.object[idxFile] = strtrim(sxpar(hdr,'OBJECT' ) )
          log.ccdsum[idxFile] =strtrim(  sxpar(hdr,'CCDSUM' ) )
          log.speedmod[idxFile] =strtrim( sxpar(hdr,'SPEEDMOD' ) )
          log.naxis1[idxFile]= strtrim( sxpar(hdr,'NAXIS1' ) )
          log.naxis2[idxFile]= strtrim( sxpar(hdr,'NAXIS2' ) )
          log.filename[idxFile] = pathFile
          log.decker[idxFile] = strtrim(sxpar(hdr,'DECKER'))
          log.imgtype[idxFile] = strtrim(sxpar(hdr,'IMAGETYP'))
          log.crcleaned[idxFile]=  isCleaned( sxpar(hdr,'HISTORY') )
        endfor
      endif
    
      ;print, log.filename + '< >'+log.object + '< >' +log.ccdsum +  '< >' +log.decker +'< >' +log.imgtype +'|              '
    
    
    
      ;Check if Directory exist to save the .log.dat file  & Store data stucture
      ;---------------------------------------------------
      command = 'IF exist '+checkPath +' (echo alreadyThere ) ELSE (mkdir '+ checkPath +' && echo createdDir)'  
      if ~file_test(checkPath) then spawn, 'mkdir '+checkPath
      save, log, filename=fileSavePath
  
  
endif else print, 'CREATELOGSTRUCTURE: .LOG Structure already in disk. '





 
END 