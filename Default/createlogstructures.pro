; Creates the missing procedure that creates a list of log structures based on the log sheet and the fits header of a given file
; input :   night   int(6)  data of the night
; output:  does not return any value. It creates a file which contains the log structure 
; ideally this has to be an isoloted procedure since waiting time is long 

pro createLogStructures, redpar,obnm,objnm

fileSavePath= redpar.rootdir+redpar.logstdir+'20'+strmid(redpar.date, 0, 2)+'/'+redpar.date+'log.dat'  


rawDirFiles = redpar.rootdir + redpar.rawdir +redpar.imdir +redpar.prefix




biasFileIndex = where(objnm eq 'bias', numBiasFiles)


log= {object:strarr(numBiasFiles), ccdsum:strarr(numBiasFiles),speedmod:strarr(numBiasFiles),naxis1: FLTARR(numBiasFiles) ,naxis2 : FLTARR(numBiasFiles) ,filename:strarr(numBiasFiles)}



if numBiasFiles ne 0 then begin

  for idxFile=0, numBiasFiles-1 do begin

      pathFile= rawDirFiles+obnm[biasFileIndex[idxFile]]+'.fits'
      
      dummy = readfits(pathFile, hdr)
      
      
      log.object[idxFile] = strtrim(sxpar(hdr,'OBJECT' ) )
      log.ccdsum[idxFile] =strtrim(  sxpar(hdr,'CCDSUM' ) )
      log.speedmod[idxFile] =strtrim( sxpar(hdr,'SPEEDMOD' ) )
      log.naxis1[idxFile]= strtrim( sxpar(hdr,'NAXIS1' ) )
      log.naxis2[idxFile]= strtrim( sxpar(hdr,'NAXIS2' ) )
      log.filename[idxFile] = pathFile
    

  endfor


endif


;help, /STRUCTURE, log


save, log, filename=fileSavePath



;#To make it faster we only creating log structures for files that have bias as object 
;
;
;needed variables     object eg. 'bias'     ccdsum eg. '3 1'    speedmod e.g.  'normal'    naxis1     naxis2


  


end 