; Extract flat field, divide by median-smoothed
; Inputs: summed flat field image, orders, extraction width
; Output: extracted and normalized flat field [pixels,orders]
; Oct 13, 2011 AT
function getflat_old, im, orc, xwid
 
      medwidth = 5 ; width of median smoothing
      getsky,im,orc,sky = sky   ; subtract scattered light  
      getspec, im, orc, xwid, flat   ; extract im to flat [npix,nord]

     smflt = flat*0.                  ;intialize smoothed flat
	for j = 0, n_elements(flat[0,*])-1 do begin      ;row by row median
	  s = flat[*, j]                         
	  ss = median(s, medwidth)       ;median smooth the orders
	  zeroes = where (ss eq 0., nz)  ;make sure we don-t divide by 0
	  if nz ne 0 then ss[zeroes] = 1.       
	  smflt[*, j] = ss              ; build smoothed flat              
	end
    flat = flat/smflt                 ;divide my median smoothed flat to remove low frequencies
    j = where(flat lt 0.1 or flat gt 10, nneg) ;don-t let the flat set weird values, they-re prob. cosmics
   if nneg gt 0 then flat[j] = 1.              
 
     return, flat

end   

