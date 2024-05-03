; median-co-adds flat-field files, result in SUM
; Find the master flat based on set of flat files numbers passed as input


;Output:  sum(size of img)         :has either the MEAND or MEDIAN of all flats
;         im_arr(*,*, #of flats)   :all flats in a CUBE form   IF Normalized option actived  -->
;                                                              If not normalized then flats are just Copy-Paste

pro addflat, flatfiles, sum, redpar, im_arr, masterFlatName=name

compile_opt idl2


debug = 1

if strlen(name) le 1 then stop, 'ERROR: Please specify the name of the master flat ' 
numwf=n_elements(flatfiles) & owidefiles = flatfiles
print, "ADDFLAT: Number of frames used to create master flat :  "+string(numwf) + ' files.'

if redpar.flatnorm le 2 then print, "ADDFLAT: Flat files are NOT being normalized combining flats " else print,  "ADDFLAT: Flat files ARE normalized before combining flats"
sum = 0
im = get_image(flatfiles[0], redpar, header=header)            ; read the first image
if (size(im))[0] lt 2 then return                             ; file not found
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

avgs =list() ; average of each image

print, 'ADDFLAT: Reading all flats. Please wait ........'
for j = 0, numwf-1 do begin ; For each flat file
  	im = get_image(flatfiles[j], redpar, header=header, geom=geom) ; read  image, correct for bias and non-linearity      
  	
    
    if (redpar.flatnorm le 2) then begin 
           normval = 1d    
    endif else if (redpar.flatnorm eq 3) or (redpar.flatnorm eq 4) then begin  ;if 3 or  4 then we normalized  the flat before combining them 
      		imswath = im[(sz[2]/2d - swidth):(sz[2]/2d + swidth),*]    		
      		imswmed = median(imswath, dimen=1, /double)    	
      		normval = max(imswmed)    		    		
      		if normval ge redpar.minflatval then begin
      		    ctwf++
      		    gdfltidx[j] = 1  ; Used to flag the GOOD FILES where the value used for normalization is greater the constant set as MINflatval
      		    normvalarr = [normvalarr, normval]
      		endif     		   		
  	endif 
  	
    im_arr1[*,*,fspot] = im/normval   ;Storing in normalized cube format . In case of no-normalized then div by 1 makes no difference  
    
    avgs.add,  mean( im_arr1[*,*,fspot], /double)
    
    if (normval ge redpar.minflatval) or (redpar.flatnorm le 2 ) then begin      
        fspot++                      ; this will overwrite a file if such normval was less than MINFLATVAL 
    endif
endfor





if  (redpar.flatnorm eq 3)  or (redpar.flatnorm eq 4 ) then begin
    if ctwf lt numwf then begin      
          print, 'ADDFLAT: WARNING! Normal values found for some flat are less the Minmum Flat Value set ! Now excluding them!'
          print, 'ADDFLAT: To solver it you would adjust minflatval which is currently set at ' +string(redpar.minflatval) 
          print, 'ADDFLAT: '+strt(ctwf)+' out of '+strt(numwf)+' are being used.'
          print, 'ADDFLAT: Flat files being used: '         
          print, flatfiles[where(gdfltidx eq 1)]
          print, 'ADDFLAT: Flat file not NOT being used are: '    
          print, flatfiles[where(gdfltidx ne 1)]
          stop, ' Type .cont    .Be aware not all flats have been used '
          im_arr = im_arr1[*,*,0:(ctwf-1)]      ;It works cause above he is overwritting the 'bad' files           
    endif else im_arr = im_arr1
    
    mean_normal =  mean(normvalarr[1:*])  ; max values used previously to normalize image 
       
    im_arr =  im_arr *mean_normal
  
endif else im_arr = im_arr1      ;When no normalization is applied






; ##########################################
; Extra step to scale iamges before operation
; ##########################################
; At this point I have ther master flat as a cube 
avgs = avgs.toarray()
common_avg  = mean(avgs, /double) ; used as a master average. This is the common level for all iamges to scale to



;>>Scaling image per image
for index = 0L, numwf-1 do begin  
    img           = im_arr[*,*,index]
    goldenFactor  =   mean(img, /double) /common_avg
    img= img*goldenFactor
    im_arr[*,*,index]   = img
endfor






; ##########################################
; ### Combining Flats by Avg or Mean #######
; ##########################################  




print, 'ADDFLAT: Combining flats using the ' +redpar.master_flat+' of all flats . Please wait ........'

sum = dblarr(nc,nr)
;for ncol=0,nc-1 do begin
;  for nrow=0,nr-1 do begin
;    
;    
;       if redpar.master_flat eq 'mean' then begin
;           sum[ncol,nrow]=mean(im_arr[ncol,nrow,*])
;       endif else  if redpar.master_flat eq 'median' then begin
;           sum[ncol,nrow]=median(im_arr[ncol,nrow,*])
;       endif else stop, '>> ERROR << : >> ERROR << The variable master_flat can only be median or mean. Please change its value in the ctio.par file'
; 
;  endfor
;endfor

sum = weighted_master_frame( im_arr, redpar.master_flat )



;Bad Pixels
badp = where(sum le 0, nbadp)   
if nbadp gt 0 then     sum[badp] = 1.0           ;Flaggin all bad pixels within the img itself


; >>  Save Master Flat in Memory  file 
wdsk, sum, name, /new ; Writting Master Flat.
print, 'REDUCE_CTIO: Master Flat frame  created & stored as :  '+name    

if debug ge 1 then begin
    MKHDR, flatHeader, sum
    history_str1 = 'Master '+redpar.master_flat+' Flat frame made of ' + strtrim(string(numwf),2) + ' quartz frames' 
    sxaddpar, flatHeader, 'HISTORY', history_str1    
    writefits, name+'.fits' , sum, flatHeader
endif



;About the master median flat
;print, 'ADDFLAT: '+flat_name+' Master Flat successfully found. Not stored in memory:'
;Stored for personal discussion
;dir = redpar.rootdir + redpar.debugging  + redpar.date
;writefits, dir + '_master_flat.fits', sum
;print, size(sum)
;print, 'ADDFLAT: Now leaving routine.'





end
