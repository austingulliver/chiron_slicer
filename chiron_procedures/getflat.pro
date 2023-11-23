;Input: summed flat field image (im), order coeffecients for all orders (orc), extraction width (xwid) determined as of redpar.xwids
;Output: extracted and normalized flat field [pixels,orders,3]
;
;Summary :
; Extract flat field, divide by median-smoothed
; 
; 
; plane 1: flat plane 2: extracted quartz, plane 3: smoothed orders
; 
; 
;Hisotry :
;added redpar and plotting options. 20120412~MJG
;incorporated weighting to get a better smoothed order 20120510~MJG
;




function getflat, im, orc, xwid, redpar, im_arr=im_arr
 
order = 6       ; polynomial order
threshold = 0.1 ; min. signal relative to max in each order
;getsky,im,orc,sky = sky   ; subtract scattered light  
if keyword_set(im_arr) then imarrsz = size(im_arr)  ; imarrsz[3] is the number of observations



if redpar.flatnorm le 1 then begin               ;runs for flatnorm equal 0 and 1  , .flatnorm is also used in addflat.pro
  	getspec, im, orc, xwid, sp, redpar=redpar    ; extract im to flat [npix,nord]
  	        ;sp (output) extracted specturm ; this case associated spectrum of Master Flat 
  	        
  	sz = size(sp) & ncol = sz[1] & nord = sz[2]
  	flat = fltarr(ncol,nord,3)                   ; flat, smoothed flat, flat/sm  ; Files contain Pixel vs Order
  	smflt = fltarr(ncol,nord)                    ; intialize smoothed flat
  	ix = findgen(ncol)                           ; argument for polynomial
endif; flatnorm le 1




if redpar.debug ge 2 then stop
if redpar.debug ge 1 and redpar.debug le 2 then begin
  fdir = redpar.plotsdir + 'flats/'
  spawn, 'mkdir '+fdir
  fdir = redpar.plotsdir + 'flats/' + redpar.date
  spawn, 'mkdir '+fdir
  fdir = redpar.plotsdir + 'flats/' + redpar.date +'/'+redpar.modes[redpar.mode]
  spawn, 'mkdir '+fdir
  fname = nextnameeps(fdir+'/'+'flats')
  ps_open, fname, /encaps, /color
  !p.font=1
  !p.multi=[0,2,3]
endif;debug plots




if redpar.flatnorm le 1 then begin   ;runs for flatnorm equal 0 and 1
    for j = 0, nord-1 do begin       ;row by row polynomial
          s = sp[*, j]               ;Spectrum for given orders : intensity values    
          
          status = ''  
          strong = where(s ge threshold*max(s), nstrong) ; strong signal
          
          if nstrong lt order+1 then stop, 'GETFLAT: No signal, stopping'
          
          cf = poly_fit(ix[strong],s[strong],order, yfit=yfit, /double, status=status)
          ss1 = poly(ix,cf)
          
          ;now mask out extremely bad regions that affect the fit (e.g. the debris at the center of the chip):
          stronger = where(s ge 0.8*ss1)
          cf2 = poly_fit(ix[stronger],s[stronger],order, yfit=yfit, /double, status=status) 
          ss = poly(ix,cf2)
          
          ;	  ss = median(s, medwidth)       ;median smooth the orders
          ; zeroes = where (ss eq 0., nz)  ;make sure we don-t divide by 0
          ; if nz ne 0 then ss[zeroes] = 1.       
          smflt[*, j] = ss              ; build smoothed flat
          
          ;I tried implementing CONTF since the continuum fit with poly is clearly affected by 
          ;low regions where there are artefacts on the CCD, but I can't quite get contf working 
          ;as well as poly, so I'll comment it out for now. ~20120504 MJG
          ;contf, s, ssc, nord=6, frac=0.5, sbin=30
          
          if redpar.debug ge 1 then begin
              plot, s, li=1, color=50, title='Order '+strt(j), /xsty
              oplot, ss
              loadct, 39, /silent
              oplot, yfit, color=250
              print, j
              x1 = 0.2*n_elements(s)
              x2 = 0.7*n_elements(s)
              y1 = 0.1*max(ss)
              y2 = y1
              xyouts, x1, y1, '(N!dADU!n)!u1/2!n: '+strt(sqrt(max(ss)), f='(F8.1)')
              xyouts, x2, y2, greek('mu')+'/'+greek('sigma')+': '+strt(mean(s/ss)/stddev(s/ss), f='(F8.1)')
          endif;debug plots
          
          if redpar.debug ge 1 and redpar.debug le 2 then begin
              if j mod 6 eq 5 then begin
            	 ps_close
                print, 'fname is: ', fname
                spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
                fname = nextnameeps(fdir+'/'+'flats')
            	 ps_open, fname, /encaps, /color
                !p.font=1
            	 !p.multi=[0,2,3]
              endif
          endif;debug plots
          
    endfor;End loop
    
    if redpar.debug ge 1 and redpar.debug le 2 then begin 
        ps_close
        print, 'fname is: ', fname
        spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
    endif;psplot
    
endif;flatnorm less or equal to 1







if redpar.flatnorm le 1 then tmp = sp/smflt  ;divide my median smoothed flat to remove low frequencies
j = where(tmp lt 0.1 or tmp gt 10, nneg)     ;don-t let the flat set weird values, they-re prob. cosmics
if nneg gt 0 then tmp[j] = 1.   


;Output of Function            
flat[*,*,0] = tmp     ; Flat Spectrum /Smoothed
flat[*,*,1] = sp      ; Flat Spectrum
flat[*,*,2] = smflt   ; Smoothed Spectrum





if redpar.debug ge 2 then stop ; debugging 
return, flat
end   

