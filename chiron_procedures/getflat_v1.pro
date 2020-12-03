; Extract flat field, divide by median-smoothed
; Inputs: summed flat field image, orders, extraction width
; Output: extracted and normalized flat field [pixels,orders]
; Oct 13, 2011 AT
function getflat, im, orc, xwid
 
      order = 5 ; polynomial order
      threshold = 0.1 ; min. signal relative to max in each order

      getsky,im,orc,sky = sky   ; subtract scattered light  
      getspec, im, orc, xwid, flat   ; extract im to flat [npix,nord]

     smflt = flat*0.                  ;intialize smoothed flat
     ix = findgen(n_elements(im[*,0])) ; argument for polynomial
	for j = 0, n_elements(flat[0,*])-1 do begin      ;row by row polynomial
	  s = flat[*, j]          
          strong = where(s ge threshold*max(s), nstrong) ; strong signal
          if nstrong lt order+1 then stop, 'GETFLAT: No signal, stopping'
          cf = poly_fit(ix[strong],s[strong],order) 
          ss = poly(ix,cf)    
;	  ss = median(s, medwidth)       ;median smooth the orders

	  zeroes = where (ss eq 0., nz)  ;make sure we don-t divide by 0
	  if nz ne 0 then ss[zeroes] = 1.       
	  smflt[*, j] = ss              ; build smoothed flat
; debugging:
;         plot, ss  &       oplot, s, li=1 & stop
              
	end
    flat = flat/smflt                 ;divide my median smoothed flat to remove low frequencies
    j = where(flat lt 0.1 or flat gt 10, nneg) ;don-t let the flat set weird values, they-re prob. cosmics
   if nneg gt 0 then flat[j] = 1.              

;stop ; debugging 
     return, flat

end   

