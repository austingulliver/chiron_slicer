; Extract flat field, divide by median-smoothed
; Inputs: summed flat field image, orders, extraction width
; Output: extracted and normalized flat field [pixels,orders,3]
; plane 1: flat plane 2: extracted quartz, plane 3: smoothed orders
; Oct 24, 2011 AT
;added redpar and plotting options. 20120412~MJG
;
function getflat, im, orc, xwid, redpar, im_arr=im_arr
 
order = 6 ; polynomial order
threshold = 0.1 ; min. signal relative to max in each order
;getsky,im,orc,sky = sky   ; subtract scattered light  
if keyword_set(im_arr) then imarrsz = size(im_arr)  ; imarrsz[3] is the number of observations

if redpar.flatnorm eq 2 then begin
imsz = size(im)
nrow = imsz[2]
flt = im
smflt = im*0d
	for j = 0, nrow-1 do begin      ;row by row
	  s = flt[*, j]                         
	  ss = median(s, 30)            ;median smooth the rows, original
	  zeroes = where (ss eq 0., nz) ;make sure we don't divide by 0
	  if nz ne 0 then ss[zeroes] = 1.       
	  smflt[*, j] = ss              ; build smoothed flat              
	endfor
	tmp = flt/smflt                 ;divide by median smoothed flat to remove low frequencies
    flat = dblarr(imsz[1],imsz[2],3)
    sp=im
endif ;flatnorm=2


if redpar.flatnorm le 1 then begin
	getspec, im, orc, xwid, sp, redpar=redpar   ; extract im to flat [npix,nord]
	sz = size(sp) & ncol = sz[1] & nord = sz[2]
	flat = fltarr(ncol,nord,3) ; flat, smoothed flat, flat/sm
	smflt = fltarr(ncol,nord)                  ;intialize smoothed flat
	ix = findgen(ncol) ; argument for polynomial
endif;flatnorm <1

if redpar.flatnorm le 1 then begin
	getspec, im, orc, xwid, sp, redpar=redpar   ; extract im to flat [npix,nord]
	sz = size(sp) & ncol = sz[1] & nord = sz[2]
	flat = fltarr(ncol,nord,3) ; flat, smoothed flat, flat/sm
	smflt = fltarr(ncol,nord)                  ;intialize smoothed flat
	ix = findgen(ncol) ; argument for polynomial
endif; flatnorm le 1


if redpar.debug ge 2 then stop

if redpar.debug ge 1 and redpar.debug le 2 then begin
  fdir = redpar.plotsdir + 'flats/'
  spawn, 'mkdir '+fdir
  fdir = redpar.plotsdir + 'flats/' + redpar.date
  spawn, 'mkdir '+fdir
  fname = nextnameeps(fdir+'/'+'flats')
  ps_open, fname, /encaps, /color
  !p.multi=[0,2,3]
endif;debug plots

if redpar.flatnorm le 1 then begin
for j = 0, nord-1 do begin      ;row by row polynomial
  s = sp[*, j]          
  strong = where(s ge threshold*max(s), nstrong) ; strong signal
  if nstrong lt order+1 then stop, 'GETFLAT: No signal, stopping'
  cf = poly_fit(ix[strong],s[strong],order, yfit=yfit) 
  ss = poly(ix,cf)    
  ;	  ss = median(s, medwidth)       ;median smooth the orders
 ; zeroes = where (ss eq 0., nz)  ;make sure we don-t divide by 0
 ; if nz ne 0 then ss[zeroes] = 1.       
  smflt[*, j] = ss              ; build smoothed flat

if redpar.debug ge 1 then begin
  plot, s, li=1, color=50, title='Order '+strt(j), /xsty
  oplot, ss
  loadct, 39, /silent
  oplot, yfit, color=250
  print, j
endif;debug plots
if redpar.debug ge 1 and redpar.debug le 2 then begin
  if j mod 6 eq 5 then begin
	 ps_close
    spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
    fname = nextnameeps(fdir+'/'+'flats')
	 ps_open, fname, /encaps, /color
	 !p.multi=[0,2,3]
  endif
endif;debug plots
endfor
if redpar.debug ge 1 and redpar.debug le 2 then begin 
  ps_close
  spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
endif;psplot
endif;flatnorm le 1

if redpar.flatnorm le 1 then tmp = sp/smflt  ;divide my median smoothed flat to remove low frequencies
j = where(tmp lt 0.1 or tmp gt 10, nneg)     ;don-t let the flat set weird values, they-re prob. cosmics
if nneg gt 0 then tmp[j] = 1.              
flat[*,*,0] = tmp
flat[*,*,1] = sp
flat[*,*,2] = smflt

if redpar.debug ge 2 then stop ; debugging 
return, flat
end   

