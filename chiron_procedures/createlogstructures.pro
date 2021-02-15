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

pro createLogStructures, redpar,obnm,objnm

checkPath    = redpar.rootdir+redpar.logstdir+'20'+strmid(redpar.date, 0, 2)+'\'
fileSavePath = checkPath+redpar.date+'log.dat'
rawDirFiles  = redpar.rootdir + redpar.rawdir +redpar.imdir +redpar.prefix
;biasFileIndex = where(objnm eq 'bias', numBiasFiles)
log= {object:strarr(n_elements(objnm) ), ccdsum:strarr(n_elements(objnm) ),speedmod:strarr(n_elements(objnm) ),naxis1: FLTARR(n_elements(objnm) ) ,naxis2 : FLTARR(n_elements(objnm) ) ,filename:strarr(n_elements(objnm) ), decker:strarr(n_elements(objnm) ), imgtype:strarr(n_elements(objnm) )}


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
  endfor
endif

;print, log.filename + '< >'+log.object + '< >' +log.ccdsum +  '< >' +log.decker +'< >' +log.imgtype +'|              '



;Check if Direcotry exist to save the .log.dat file  & Store data stucture
;---------------------------------------------------
command = 'IF exist '+checkPath +' (echo alreadyThere ) ELSE (mkdir '+ checkPath +' && echo createdDir)'
SPAWN, command, windowsReply    ; This has to subtituted for the equivalent depending on the OS.
; Otherwise just create the year directory manually. E.g. ..\chiron\tous\mir7\logstructs\2021\
if  windowsReply eq 'createdDir' then print, 'CREATELOGSTRUCTURES: Directory '+checkPath+' created in your behalf. '



save, log, filename=fileSavePath



;#To make it faster we only creating log structures for files that have bias as object 
;
;
;needed variables     object eg. 'bias'     ccdsum eg. '3 1'    speedmod e.g.  'normal'    naxis1     naxis2


  


end 