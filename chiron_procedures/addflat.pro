; median-co-adds flat-field files, result in SUM
; Find the master flat based on set of flat files numbers passed as input
; 
;do_mean: If 1 Calculates the Mean. 0 Calculates the mean

;Output:  sum(size of img)         :has either the MEAND or MEDIAN of all flats
;         im_arr(*,*, #of flats)   :all flats in a CUBE form   IF Normalized option actived  -->
;                                                              If not normalized then flats are just Copy-Paste

pro addflat, flatfiles, sum, redpar, im_arr, do_mean=do_mean

compile_opt idl2

numwf=n_elements(flatfiles) & owidefiles = flatfiles
print, "ADDFLAT: Master file combines  "+string(numwf) + ' files.'

if redpar.flatnorm eq 0 then print, "ADDFLAT: Flat files are NOT being normalized  " else print,  "ADDFLAT: Flat files ARE being normalized  "

sum = 0
im = getimage(flatfiles[0], redpar, header=header) ; read the first image


if (size(im))[0] lt 2 then return ; file not found

geom = chip_geometry(flatfiles[0], hdr=header, redpar=redpar) ; returns CCD geometry in a structure of the same first img
sz = size(im)
nc=sz[1]  &  nr=sz[2]

im_arr1=dblarr(nc,nr,numwf) ; Array that is meant to contain all flats 
im_arr1[*,*,0]=im
swidth = 50L                ;half width of central swath
gdfltidx = dblarr(numwf)    ; a marker for every flat file (marker of what ?)
normvalarr = 0d

ctwf=0   
fspot = 0 ;index the im array data cube


print, 'ADDFLAT: reading all flats wait ........'
for j = 0, numwf-1 do begin ; For each flat file
  	im = getimage(flatfiles[j], redpar, header=header, geom=geom) ; read  image, correct for bias and non-linearity      
  	
  	;print, im
    
  	
  	if redpar.flatnorm eq 1 then begin ; DEFAULT(our) case
    		imswath = im[(sz[2]/2d - swidth):(sz[2]/2d + swidth),*]    		
    		imswmed = median(imswath, dimen=1, /double)    	
    		normval = max(imswmed)    		    		
    		if normval ge redpar.minflatval then begin
    		    ctwf++
    		    gdfltidx[j] = 1  ; Used to flag the GOOD FILES where the value used for normalization is greater the constant set as MINflatval
    		    normvalarr = [normvalarr, normval]
    		endif 
    		   		
  	endif 
  	
  	if redpar.flatnorm eq 0 then normval = 1d  	  	
  	if redpar.flatnorm le 1 then im_arr1[*,*,fspot] = im/normval   ; For either case flatnorm 1 and 0
    if (normval ge redpar.minflatval) or (redpar.flatnorm eq 0 ) then begin      
        fspot++                      ; this will overwrite a file if such normval was less than MINFLATVAL for FLATNORM = 1 only
    endif
endfor





if  redpar.flatnorm eq 1 then begin
    if ctwf lt numwf then begin
      
          print, 'ADDFLAT: WARNING! Normal values found for some flat are less the Minmum Flat Value set ! Now excluding them!'
          print, 'ADDFLAT: To solver it you would adjust minflatval which is currently set at ' +string(redpar.minflatval) 
          print, 'ADDFLAT: '+strt(ctwf)+' out of '+strt(numwf)+' are being used.'
          print, 'ADDFLAT: Flat files being used: '
          ;printt, flatfiles[where(gdfltidx eq 1)]
          print, flatfiles[where(gdfltidx eq 1)]
          print, 'ADDFLAT: Flat file not NOT being used are: '
          ;printt, flatfiles[where(gdfltidx ne 1)]
          print, flatfiles[where(gdfltidx ne 1)]
          stop, ' Type .cont    .Be aware not all flats have been used '
          im_arr = im_arr1[*,*,0:(ctwf-1)]      ;It works cause above he is overwritting the 'bad' files 
          
    endif else im_arr = im_arr1
    
    mean_normal =  mean(normvalarr[1:*])     
    im_arr =  im_arr *mean_normal
  
endif else im_arr = im_arr1      ;When no normalization is applied









if do_mean eq 1 then flat_name='MEAN' else flat_name = 'MEDIAN'
print, 'ADDFLAT: calculating '+flat_name+' flat........'

sum = dblarr(nc,nr)
for ncol=0,nc-1 do begin
  for nrow=0,nr-1 do begin
       if do_mean eq 1 then begin
           sum[ncol,nrow]=mean(im_arr[ncol,nrow,*])
       endif else begin
           sum[ncol,nrow]=median(im_arr[ncol,nrow,*])
       endelse	 
  endfor
endfor



;Bad Pixels
badp = where(sum le 0, nbadp)   
if nbadp gt 0 then     sum[badp] = 1.0           ;Flaggin all bad pixels within the img itself




;About the master median flat
;print, 'ADDFLAT: '+flat_name+' Master Flat successfully found. Not stored in memory:'
;Stored for personal discussion
;writefits, 'C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\files_sent_dr_gulliver\181103_master_flat.fits', sum
;print, size(sum)
print, 'ADDFLAT: Now leaving routine.'





end
