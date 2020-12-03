;Input:
;     im (input array (# columns , # rows)) image on which order locations are to
;           be mapped.
;     swid (input scalar) swath width, the number of columns desired in each swath.
;     redpar is the parameter structure. We use here redpar.binning and redpar.pkcoefs 
;     [minreq (optional input scalar keyword)] the minimum number of orders that
;           must be found in order for a successful order location.
;           modeidx indicated which mode is used to set initial peak offset
;           and window for parabola fitting related to xwid
;
;Ouput:
;     orc (output array (# coeff per fit , # orders) OR scalar) either an array
;          containing the coefficients of the polynomials giving the row number as a
;          function of column number for each order OR a scalar equal to zero
;          indicating a consistent set of orders locations could not be found.
;          ALWAYS CHECK TO SEE IF THE orc RETURNED IS A SCALAR, INDICATING TROUBLE!
;          [ome (optional output vector (# orders))] each entry gives the mean of the
;          absolute value of the difference between order locations and the polynomial
;          fit to these locations.

;Summary:
;
;     Maps out and fits polynomials to order positions.  Returns both polynomial
;     coefficients and mean difference between polynomial and actual locations.
;
;
;Notes: 
;    variable 'redpar.' containes the a polynomial as function of the order number. y= The peak found in the middle swath 

;           
;
;History:         
;18-Apr-92 JAV	Removed common block definition.
;30-Apr-92 JAV	Added resampling logic.
;05-Sep-92 JAV	Added minreq logic.
;12-Sep-92 JAV	Added test for parabolic order peak off image.
;28-Sep-92 JAV	Try median of past three peaks (mdpk) when new peak is not in
;		 poff window in case most recent peak was only marginally good.
;29-Sep-92 JAV	Inserted logic to set poff to 2 pixels for binned images.
;12-Dec-92 JAV	Discard orders whose bounding troughs are within poff of edge.
;21-Jan-93 JAV	Only discard orders whose bounding troughs are off edge.
;28-Jan-94 JAV  Improve accuracy in order location fits by first subtracting
;		 the mean row number in each order.
;17-May-08 DAF adapted for CTIO
;24-Feb-10 JMB moved selection of order after check for missing peaks to prevent crash
;				in case where no peaks were found.
; 7-Oct-11 AT: removed ctio_common. Added internal diagnostic:
;              debug=1 to see the overall result, debug=2 to see each
;              swath with peaks
; 18-oct-2011 AT: introduced dpks. Reduced swath smoothing to simple median
; 19-Feb-2012 MJG: Added section for fitting for peak coefficients when setting
;				up for the first time.




pro fords,im,swid,orc,ome,redpar, minreq=minreq

;To Write Master Flat as  Fits for independet analyis
;fname='C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\flats\debugging\mstr_flat_171218.fits'
;writefits, fname, im

;1) Defining Initial Constraints 
debug = redpar.debug  ;
if n_params() lt 3 then  stop, 'syntax: fords,im,swid,orc[,ome].'
if n_elements(minreq) eq 0 then minreq = 0  ;minimum required orders to be found
loadct, 39, /silent
!p.color = 0
!p.background=255
orcdeg = 4          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome) 
                    ;BUT  eventually loss of  precision begins increasing the errors again.MAKE SURE RESIDUALS DECREASE.
mmfrac = 0.25       ;maximum fraction missing peaks allowed
maxome = 10.         ;max allowable mean pixel error in orcs. Previous = 2 i                                   ----------------------change back . Just to allow script continue
sz = size(im)       ;size of image passed: e.g. Master Flat
ncol = sz(1)        ;number of cols in image
nrow = sz(2)        ;number of rows in image
soff = (swid - 1) / 2.0   ;offset to edge of swath window e.g = 16. Along the dispersion direction.  Inherited from original code
nswa = long(ncol/swid)    ;# of 'full' swaths in image

xwid = redpar.xwids[redpar.mode]       ; E.g. 12 for slicer 
poff = xwid/2 + 1                      ; 1  extra  pixel
smbox = xwid/2
dpks = redpar.dpks[redpar.mode]        ; peak shift for each mode, binned pixels . For slicer only = -5  why ??

nord = redpar.nords ; total number of orders
iord = findgen(nord) ; Number of Orders





if debug le 1 then begin
   print,'FORDS: smoothing length, half-width, offset: ', smbox, poff, dpks
   print,'FORDS: Degree of polynomial fit to order locations = '+ strtrim(string(orcdeg),2) 
   print,'FORDS: Number of swaths = ' + strtrim(string(nswa),2)
   print,'FORDS: Number of cols,rows = ' + strtrim(string(ncol),2) + '  ' + strtrim(string(nrow),2)
endif;










;2) Finding swaths + restoring order references

scen = swid * findgen(nswa) + soff ; Centers of each swath
                                   ; Note last swatch (is the widest) will have extra pixels that were not considered since the division to find number of swaths could have not been exact 
aswa = fltarr(nrow,nswa)			     ;array for all swathsa long with all rows 


print,'FORDS: Estimating positions of order peaks in first swath.'
for isw=0,nswa-1 do begin			;loop thru swaths
      aswa(*,isw)=total(im(scen(isw)-soff:scen(isw)+soff,*),1) ;finding swaths + collapsing swatch into vector. subimage becomes a vector(cross dispersion direction) 
endfor
swa = aswa(*,nswa/2) 	;compacted central swath
if smbox gt 2 then swa = median(swa,smbox)	 ;smooth swath to reduce noise                                           ------------------ SWATHS are getting smoothed DO WE NEEDD THIS ?
dmode = strt(redpar.modes[redpar.mode])



;Restoring coefficients of middle swatch stored in ctio.par. This coefficient wil be used as reference to trace the orders

if dmode eq 'slicer' then begin ;If
     pkcoefs =redpar.pkcoefs_slicer   ;' These polynomial will be used later          
     pk = poly(iord,pkcoefs)    ; since iord is vector with all orders. pk has the peaks (used as ref) for each of the orders     
     
endif else begin                ; For any other mode we use the inherited coefficientes 
     pkcoefs = redpar.pkcoefs   ; For any other mode: Inherited this was found for UNBINNED img  
     pk = poly(iord,pkcoefs)/redpar.binning[0]  
     
      
endelse

origina_pk= pk   ;variable not used within procedure. Used for debugging



















;3) Debugging  and  manual Order finding


;Check if red dashed lines match with the peak if Don't then used debug tool bellow and find coefficients again 
;debug =1
if debug gt 0  then begin 
   yy = [0,max(swa)]
   swa = aswa(*,2); seocnd swath
   p1= plot( swa, /xsty, /ysty,  xtitle='Cross Dispersion Direction',  ytitle='Intensity in Central Swath' ) ; These are wrt to actual current img
   for kk=0,nord-1 do p1= plot(pk[kk]*[1,1], yy, li=2, color='red' , /overplot) ; These are wrt to stored Polynomial
   for kk=0,nord-1 do p1= plot( pk[kk]*[1,1] + smbox, yy, li=2, color='gray', /overplot )
   for kk=0,nord-1 do p1= plot( pk[kk]*[1,1] - smbox, yy, li=2, color='gray', /overplot ) 
   stop, 'FORDS: type .cont to continue.'
endif



;Actual RE-DO of order location. User selects pixel values for peaks used to find the new polynomial used to reference the 

IF debug gt 2 THEN BEGIN
   
   
   PRINT, '***********************************************'
   PRINT, 'NOW IDENTIFYING ORDER LOCATIONS....'
   PRINT, 'CLICK IN THE CENTER OF EACH ORDER.'
   PRINT, "(Y VALUE DOESN'T MATTER)"
   PRINT, '***********************************************'
   yy = [0,max(swa)]
   ;plotting central swath
   left_swath = aswa(*,nswa/4) ;  Do one at the time
   right_swath = aswa(*,3*nswa/4)
   swa = aswa(*,nswa/2)
   
   swa= swa 
   
   plot, swa  ; Used as shortcut to increase window size
   stop, 'Make plot BIGGER before continue'
   plot, swa, /xsty, /ysty,  xtitle='Cross Dispersion Direction [Pixel]',   ytitle='Intesity in Central Swath'           
   
   xeye = dblarr(n_elements(iord))
   for eyeord = 0, n_elements(xeye)-1 do begin                     ; Add numbering of Orders as you do it 
  	  cursor, xcur, ycur, /down
  	  xeye[eyeord] = xcur
  	  print,'Selected  >>  Indexed Order : '+  string(eyeord) + '     X-coord : '+ string(xcur) + '     Y-coord :  ' +string( ycur)
  	  oplot, [xcur,xcur], [ycur, ycur], PSYM=7, color=1   ; BEFORE psym =8, color =230
   endfor
   
   ;loadct, 39, /silent
   pkcoefs = poly_fit(iord, xeye, 3,  yfit = yfit) ; before init=[38., 76., -0.04, -0.003, 0d] , fixed=[0,0,0,0,1],
   
   p2= plot( iord, xeye, color='black' , title='Actual vs fitted values ')
   p2= plot(  iord, yfit, color='blue',  /overplot)
   
   ;pkcoefs = res[0:3] * redpar.binning[0] ; ignore binning since we doing this for slicer only 
   pk = poly(iord,pkcoefs) ;/redpar.binning[0] ; default peaks in binned pixels, central swath
   
   p3= plot ( swa, /xsty, /ysty,  xtitle='Cross Dispersion Direction',  ytitle='Counts in Central Swath' )
   
   for kk=0,nord-1 do p3= plot(  pk[kk]*[1,1], yy, LINESTYLE=2, color='blue',/ overplot)
   PRINT, '***********************************************'
   PRINT, 'IF IT LOOKS GOOD ENTER THESE FOR PKCOEFS IN YOUR .PAR FILE'
   PRINT, 'pkcoefs: [', strt(pkcoefs[0]), ',',  strt(pkcoefs[1]), ',',  strt(pkcoefs[2]), ',',  strt(pkcoefs[3]), ']'
   PRINT, '***********************************************'
ENDIF;DEBUG>3 => REDO ORDER LOCATIONS FROM SCRATCH



if debug gt 2 then begin
  nord = n_elements(pk)        ;number of orders
  print,'FORDS: Number of peaks found = ' + strtrim(string(nord),2)
  if nord lt minreq then begin      ;true: too few orders found
      orc = 0         ;scalar orc flags error
      print, 'FORDS: Too few orders found in initial swath - returning without ORCs'
      orc = 0
      return
  endif
endif






















;4) 
;  Loop through the swaths, determining exact positions of peaks by fitting
;  quadratic polynomials in vicinity of previous peaks. Store new peaks, as
;  long as they are reasonably close to previous peak positions. Postions
;  with poor peak determinations are left zero.
    
ords = fltarr(nswa,nord)		;peak locations for swaths

print,'FORDS: Mapping entire image.  Be patient....'

pk = long(pk+0.5)				;adding 0..5 to 'old-reference' peaks

counter=0 ; Used for debugging 



; Start at center, move left and right
FOR direction = -1, 1, 2 DO BEGIN
       ;pk = pk0 ; peaks for EACH ORDER in dispersion direction (of th whole order)
       imin = (direction + 1)/2 ; starting swath
       FOR isw1=imin,nswa/2-1 do begin			;loop through swaths
          	  isw = isw1*direction + nswa/2  ; swath number following direction 
          	  swa = aswa(*,isw)				;recover swath
          	  
          	  
          	  ;if dmode eq 'slicer' then begin       	     
          	   
          	  ;   if (isw ge 0) and (isw lt round(4*nswa/10) ) then pk = poly(iord,pkcoefs_slicer_3_tenth)
          	     
          	   ;  if ( isw ge round(4*nswa/10)  )  and ( isw lt round(6.5*nswa/10.0) ) then pk = poly(iord,pkcoefs)            	     
          	   ;if ( isw ge round(6.5*nswa/10.0)  )  and ( isw lt nswa) then pk = poly(iord,pkcoefs_slicer_right)          	     
          	   ;
          	     
          	  ;endif
          	   
          	  
          	  
          	  
          
          	  
          	  if smbox gt 2 then swa = median(swa,smbox)	 ;smooth swath to reduce noise
          
          	  FOR ior = 0, nord-1 DO BEGIN ;loop through orders within the swath
          	   
          	        
                		 opk = pk(ior)				;old peak location  
                		 
                		
                		 ;if ior < 8 then  poff =3
                		 ;if ior < 29 then poff= 4 else poff=xwid/2 + 1  ; to a ccount for stretched orders in the red end 
                		 
                		               		 
                		if opk lt poff or opk gt nrow-poff-1 then begin    ;e.g. for slicer poff=7 (peak off by )
                    			pk(ior) = 0				;flag peak off edge
                    			;print, 'FORDS: ** (Should not heppend) Problem with the OLD PEAK  it is :' +string(opk)
                    			;PRINT, '           *at swath: '+string(isw) +' in order : '+string( ior) + '  *Order skiped '
                    			;stop,' press .c to skip this order since is to close to the horizontal edges of img  4112 x 1366    '
                    			goto,edge				;peak too near edge,next order
                		 endif
                		
                		 opk_down=long(opk-poff) > 0        ; Number of pixel bellow used to extract order
                		 opk_down=long(opk_down) < (nrow-1) 
                		 
                		 ;print, '-->' +string(opk_down)
                		 
                		 opk_up=long(opk+poff) < (nrow-1)   ; Number of pixel above used to extract order 
                		 opk_up = long(opk_up) > 0
                		               		 
                		 z = swa[opk_down:opk_up]		  ;region where peak is expected
                		 dummy = max(z,mx)			     	;mx is the relative location of maximum (Pixel position)
                		 mx = opk - poff + mx(0)			;  LOCAL max pixel ; This is the where the actual maximum  in given order, given swath is at.
                		 
                		 
                		 ;if mx < 0 then stop, 'the value of mx is '+string(mx)
                		 
                		 if mx lt poff or mx gt nrow-poff-1 then begin   ; This just checks for the very bottom and very top ????
                    			pk(ior) = 0				;flag peak off edge
                    			;print, 'FORDS: ** (Should not heppend) Problem with the PEAK FOUND using max it is '+string(mx)
                    			;PRINT, '           *at swath: '+string(isw) +' in order : '+string( ior) + '  *Order skiped '
                    			;stop , ' press .c to continue'
                    			goto,edge 				;max too near edge,next order
                		 endif
                		 
                		 mx_down= (long(mx-poff)) > 0. 
                		 
                		 mx_down= long(mx_down) < (nrow-1)
                		 
                		 ;print, '-->' +string(mx_down)
                		 
                		 ;if mx_down lt 0 then stop, 'got  mx_down with val :'+string(mx_down)
                		 
                		 
                		 mx_up=long(mx+poff) < (nrow-1)
                		 mx_up=long(mx_up) > 0
                		 
                		 range = mx_up -mx_down
                		 if (range lt 3) then goto, edge
                		 
                		 z = swa(mx_down:mx_up)			;region around max pixel
                		 len_z=size(z)
                		 len_z=len_z[1]
                		 ix = findgen(len_z) 
                		 
                		 
                		 cf = poly_fit(ix,z,2)			;coeff of quadratic fit
                
                		 peak = -cf(1) / (2*cf(2)) + mx - poff	;extremum of polynomial
                		                                        ; -cf(1) / (2*cf(2)) =  the Y value (the index) at which there is a peak. They solve the derivative of y for x 
                		 
                		if peak lt poff or peak gt nrow-poff-1 then begin
                    			pk(ior) = 0
                    			;print, 'FORDS: ** (Should not heppen)problem with the FITTED PEAK it is : '+ string(peak)
                    			;print, '           * is supposed to be within boudaries [ '+ string(poff) + ', ' +  string(nrow-poff-1) + ' ]' 
                    			;PRINT, '           *at swath: '+string(isw) +' in order : '+string( ior) + '  *Order skiped '
                    			;stop , 'press .c to continue'
                    			goto,edge
                		endif
                    
                    ;added
                   
                   
                   
                   ;if (dmode eq 'slicer')  and (ior lt 8) then begin
                    
                    ;ords(isw,ior) = peak
                    ;goto, edge
                    
                   ;endif 
                
                
                
                		 ;Resampling code: We-ve just fit a polynomial to the peak pixel and "poff"
                		 ;  pixels on either side. When the true peak is near the edge of a pixel, we
                		 ;  are oversampling one side of the peak (by nearly a pixel). As the true peak
                		 ;  passes into the next row-s pixel (due to the curvature of the orders), the
                		 ;  extra pixel being oversampled jumps to the *other* side. If the order
                		 ;  shapes were really parabolas, this would have no effect, but they-re not.
                		 ;  The peaks of the parabolic fits jump, when the true peak crosses a pixel
                		 ;  boundary. We correct for this below by splining the pixels around the peak
                		 ;  onto a much finer scale and then fitting another parabola to the splined
                		 ;  points within a well-defined window.
                		 
                		 locut =  (long(peak - poff)) >0		;low index of region to cut
                		 locut = locut < (nrow-1)
                		 hicut = (long(peak + poff + 0.999)) < (nrow-1)  ;high index of cut region
                		 hicut = (hicut) > 0
                		 
                		 range =hicut - locut
                		 
                		 if range gt 0 then begin  ; as a quick fix . If not then it will conserve its value of 0
                		  
                    		 
                    		 zcut = swa(locut:hicut)			;cut region to finely sample
                    		 xcut = findgen(hicut - locut + 1) + (locut - peak) ;indicies for cut region			;  (0 is at true peak)
                    		 ; zfine = fspline(xcut,zcut,xfine)		;finely sample peak region
                    		 
                    		 xfine = findgen(10*range)/10 - poff   ;abscissa for fine resmapling
                    		 
                    		 zfine = spl_interp(xcut,zcut,spl_init(xcut,zcut),xfine,/double) ; 
                    		 cf = poly_fit(xfine,zfine,2)		;fit poly to fine sampling
                    		 peak = -cf(1) / (2*cf(2)) + peak		;peak at extremum of parabola ; ACTUAL AND FINAL PEAK
                    		 ;End Resampling code.
                    		 ; debugging:
                    		 ;      plot,  zfine,  /ysty
                    		 ;      oplot, cf[0] + xfine*cf[1] + xfine^2*cf[2], li=2
                    		 ;      stop
                    		 ; end debugging
                          
                          
                         
                         ;Recall opk is the old Peaks found from the coefficients in ctio.par                		
                    		 if peak ge opk-poff and peak le opk+poff then begin 		; if the finner found peak is withint the 'old' peak range                    			
                        			ords(isw,ior) = peak			  ;valid peak saved for GIVEN ORDER IN FIVEN SWATH
                        			pk(ior) = long(peak+0.5)		;to search near peak in next swath
                    		 endif else begin				;else: maybe last peak off
                    		     
                    		     print,'FORDS: WARNING !Finner peak found is outside of bounds of -referenced- peak. If cont this peak will get saved IF is within the NEW limits. '
                    			   
                    			   if isw ge 3 then begin			;if already in more than the 3rd swath then  can do median
                          			   mdpk = median(ords(isw-3:isw-1,ior))	;median of last three peaks
                          			   if peak ge mdpk-poff and peak le mdpk+poff then begin 	;only keep peaks near pixel max
                              				 ords(isw,ior) = peak		    ;valid peak, save in array
                              				 pk(ior) = long(peak+0.5)		;search near peak in next swath
                          			   endif else begin
                          			     print, 'FORDS: oh oh ! It did not got saved cause outisede of limits' ; ords maintas its values of 0
                          			     
                          			   endelse
                          			   
                             endif
                             
                    		 endelse
                		 endif
                     
                     
                     
                		 edge:                     ;jump here to skip an order of a given swath
                		    
          	  ENDFOR		;end order loop
          
          	  ; Diagnostic of swath peaks : will stop the number of swaths you have
          	  
          	  if debug gt 1 then begin 
              	   if (counter mod 11) eq 0 then begin
              	     
              	   
                    		 yy = [0,max(swa)]
                    		 title =' Swath '+ string( isw ) 
                    		 p0=plot( swa, title=title, xtitle= 'Compacted swath pixels', ytitle='Intensity')   ; plots the all compacted pixels for given swath
                    		 for kk=0,nord-1 do p1=plot( ords[isw,kk]*[1,1], yy, li=1 ,color='red',  /overplot  )  ; this are the selected peaks= 
                    		 ;stop, 'FORDS DEBUG: SWATH plot in swath '+string(isw)
              	  endif
          	  endif
          	  counter=counter+1
          
          	  if (isw1 eq 0) then pk0 = pk ; remember peaks in the central swath 
          
          	  ; More debugging
          	  ; if (isw1 eq 0) and (debug gt 0) then begin
          	  ; plot zero order
          	  ;  yy = [0,max(swa)]
          	  ;  plot, swa
          	  ;  for kk=0,nord-1 do oplot, pk[kk]*[1,1], yy, li=1 
          	  ;  ixx = findgen(nord)
          	  ;  y = redpar.binning[0]*reform(ords(nswa/2,*), nord)
          	  ;  sel = where(y gt 0)
          	  ;  res = poly_fit(ixx[sel], y[sel],3)
          	  ;  print, 'Central swath polynomial: ',res
          	  ;   pk1 = poly(ixx,res) 
          	  ;    stop, 'FORDS DEBUG: FIRST SWATH plot with new peaks Press .C to continue'
          	  ; endif    
       ENDFOR		;end half- swath loop
ENDFOR          ; end direction loop



























;Added by JL for Trouble Shooting:
;
;TroubleShooting A
;x_generated=findgen(74);increment=0.25
;calculated_y=poly(x_generated,pkcoefs)/redpar.binning[0] ;These are the values used as the OLD PEALS 
;p0=plot(x_generated, calculated_y, "r4D-" ,  XTITLE= 'X-Pixles', YTITLE= 'Y-Pixels',TITLE="Trouble Shooting Orders A")


;TroubleShooting B
for orderI=0, nord-1 do begin  
print, 'FORDS: This is indexed order :'+string(orderI)
peak_values = ords(*,orderI) ;  ords(isw,orderI)
x_num= size(peak_values)
x_num= x_num(1)
;print, 'FORDS:  -> Confirm this value is 128 : the number of points to fit a givne order '+string(x_num)
generated_x = INDGEN(x_num)
plot = PLOT( generated_x , peak_values,   XTITLE= 'X-Pixles', YTITLE= 'Y-Pixels', TITLE='Trouble Shooting Orders B', /OVERPLOT)
;;plot = SCATTERPLOT( generated_x , peak_values, SYMBOL='+',  XTITLE= 'X-Pixles', YTITLE-= 'Y-Pixels',TITLE='Troubleshooting Orders', OVERPLOT=1)
endfor

;;
;;End Added Trouble Shooting

; AT Oct 4 2011: find and print quadratic approximation for central swath
if debug gt 0 then begin 
;  plot, ords[*,0], yr=[0,nrow], psym=1
;  for j=1,nord-1 do oplot, ords[*,j], psym=1  
    ix = findgen(nord)
    y = redpar.binning[0]*reform(ords(nswa/2,*), nord)
    sel = where(y gt 0)
    res = poly_fit(ix[sel], y[sel],3)
    print, 'Central swath polynomial: ',res
endif

;Loop through orders, fitting polynomials to order locations determined above.
;  If too large a fraction of the peaks are missing in an order not on the
;  edge, then return with orc set to scalar zero, flagging error condition.
;  Also compute the mean error in the polynomial fit. If this is too large,
;  then return with orc set to scalar zero, flagging error condition.
if debug gt 0  then print,'FORDS: Fitting polynomials to order peaks.'



















orc = dblarr(orcdeg+1,nord)			;init order coefficient array
ome = orc[0,*]				;init order mean error
tomiss = 0					;init total missing peak count




; Fitting : each peak found for a given swath and order
FOR ior = 0,nord-1 do begin

      iwhr = where(ords[*,ior] gt 0,nwhr)		;find valid peaks, (Invalud peaks were previously set to zero)
      nmiss = nswa - nwhr				;number of missing peaks
      if debug  gt 0 then print,'FORDS: Order:',ior,'  # misses:',nmiss
      
      if float(nmiss)/nswa gt mmfrac then begin	; If does not has sufficient peaks to fit
      		
      		if ior le 4 or ior ge nord-4 then goto,jump1				;ignore problems near edges (first 4 and last 4 orders)
      		orc = 0					;scalar zero flags error
      		fstr = strtrim(string(form='(f10.1)',(100.0*nmiss)/nswa),2)
      		print,'FORDS: ' + fstr + '% of peaks in order missing.  -->> Returning without ORCs : Need to debug  <<--'
      		stop
      		return
      endif
      
      x = scen[iwhr]				    ;get swath centers with peaks
      y = ords[iwhr,ior]				;get nonzero peaks    
      tomiss = tomiss + nmiss		;increment total missing peaks count
      mny = total(y) / nwhr			;mean row number, y has the 128 (assuming swath size of 32) peak values found for a given order 
      
  	  y = y - mny						;better precision w/ mean=0. Every peak gets divided by the row'x mean 
  	  ind = indgen(nwhr-2) + 1                    ;indices excluding ends (gm). DOES NOT TAKE neither the 1 nor the last peak-value-swath-PIXEL of a given order
      xp = x(ind)
      yp = y(ind)
      orc(*,ior) = poly_fit(xp,yp,orcdeg,fit)       ;fit polynomial to peaks
                                                    ;poly_fit returns the coefficients of the fit (expect 5 number (degree +1))
                                                    ;"fit" stores the fitted y values 
      ome(ior) = stddev(yp - fit) 
      
      
  
      if ome(ior) gt maxome then begin		;orc mean error too large?, Recall this won't heppend for the 1st and last 4 orders
          print, 'FORDS: Excessive scatter in INDEXED ORDER ' + string(ior) + 'peaks - returning without orcs.'
          orc = 0					;scalar zero flags error
          return
      endif
      
      orc(0,ior) = orc(0,ior) + mny		;renormalize since mny was reduced before for better precision
                                      ;at this point orc does not have 128 points anymore but rather 1 array with the coeff this is why (0,iord)
      if debug gt 0 then begin   
        ;Plots all points for a GIVEN ORDER     
        swath_x=  scen
        calculated_y=poly(swath_x,orc(0,ior))  ; All point (all swaths) for a given order
        order_title = 'Order :' +string(ior) 
        p1=plot(calculated_y ,  XTITLE= 'X-Pixles', YTITLE= 'Y-Pixels',TITLE=order_title, /OVERPLOT)  
       
        
          
      endif
                                      
      jump1:					;jump here if skipping an order
ENDFOR

;----> Up to here: orc stores 1 array per order. Where every array hast the coefficients of the found polynomial 
;                : Recall first and las 4 have not been taken into account 






;
;debug2=1
;Plotting of image along with fitted order to check for congruency
if debug2 eq 1 then begin
  
    im1 = image(im )
   ;im1 = image(imgPath,  BACKGROUND_COLOR="midnight blue",  IMAGE_DIMENSIONS=[360,180], IMAGE_LOCATION=[-180,-90], XRANGE=[-180,0], YRANGE=[-90,90],  DIMENSIONS=[512,512], MARGIN=0)


  for iord=0,  74 do begin    
    x=findgen(4112)
    calculated_y=poly(x,orc(0,iord))
    p1= plot(calculated_y, color='red',/OVERPLOT)    
  endfor


endif
stop, '   ------------------------------------- Save as .png and check consistency' 




































;Getting rid of bad orders: Trim first and last order coefficients until nonzero.

WHILE total(orc(*,nord-1)) eq 0 do begin      ;too few peaks in last order
   orc = orc(*,0:nord-2)                        ;remove last order
   ome = ome(0:nord-2)                          ;remove last error point
   nord = nord - 1                              ;decrement order count
   stop,'FORDS:  WARNING ! : Trimming last (BLUE) order - too few peaks. Account for this in Wavelength Calibration'
ENDWHILE

WHILE total(orc(*,0)) eq 0 do begin ;too few peaks in first order
    orc = orc(*,1:nord-1)                       ;remove first order
    ome = ome(1:nord-1)                         ;remove first error point
    nord = nord - 1                             ;decrement peak count
    print,'FORDS: WARNING ! : Trimming first (RED) order - too few peaks. Account for this in Wavelength Calibration'
ENDWHILE


;Discard first or last order, if they extend beyond edge of image.
x = findgen(ncol)				;column indicies. (E.g. 4112)
yy = poly(x,orc(*,nord-2))    ;Second to last 
y = poly(x,orc(*,nord-1))			;center of last order
; if max(y) gt nrow-poff-1 then begin		;order extends beyond image
; if max(y+0.5*(y-yy)) gt nrow-poff-1 then begin;order extends beyond image
;if max(y+0.5*(y-yy)) gt nrow-1 then begin	;order extends beyond image
if max(y) gt nrow-1 then begin  ;order extends beyond image
    orc = orc(*,0:nord-2)			;remove last order
    ome = ome(0:nord-2)				;remove last error point
    nord = nord - 1				;decrement order count
    stop,'FORDS: WARNING ! : Trimming last order (BLUE) - off edge of image. Account for this in Wavelength Calibration'
end


yy = poly(x,orc(*,1))				;edge of first order
y = poly(x,orc(*,0))				;edge of first order
; if min(y) lt poff then begin			;order extends beyond image
; if min(y-0.5*(yy-y)) lt poff then begin	;order extends beyond image
if min(y-0.5*(yy-y)) lt 0 then begin		;order extends beyond image
    orc = orc(*,1:nord-1)			;remove first order
    ome = ome(1:nord-1)				;remove first error point
    nord = nord - 1				;decrement order count
    print,'FORDS: WARNING ! : Trimming first order (RED)- off edge of image. Account for this in Wavelength Calibration'
endif



;Order coefficients determined.
print, 'FORDS:                  .... Order Tracing Summary ....'
print,'FORDS: Total missing peaks (for all orders) = ' + strtrim(string(tomiss),2)
print,'FORDS: Orders found = ' + strtrim(string(nord),2)
if nord lt minreq then begin			;true: too few orders found
    orc = 0					;scalar orc flags error
    print, 'FORDS: Too few orders found in initial swath'  + ' - returning without ORCs.'
    return
endif
                
                
                
                
                
                
         
         
         
         
         
         
         
                
if debug gt 2 then begin 
      if debug ge 1 and debug le 2 then begin
    	 fdir = redpar.plotsdir + 'fords/'
    	 spawn, 'mkdir '+fdir
    	 fdir = redpar.plotsdir + 'fords/' + redpar.date
    	 spawn, 'mkdir '+fdir
    	 ps_open, nextnameeps(fdir+'/'+dmode+'_ordertrace'), /encaps, /color
      endif;debug plots
      
      ix = findgen(ncol)
      ys = fltarr(ncol,nord)
      for j=0,nord-1 do ys[*,j] = poly(ix,orc[*,j])
      tmp = sqrt( (im - smooth(im,5)) > 0)
      a = max(tmp)  ; mark the orders
      for i=0,ncol/10-1 do for j=0,nord-1 do tmp[i*10,ys[i*10,j]+0.5]=a
      ;order trace:
      display, tmp
      if redpar.debug ge 1 and redpar.debug le 2 then ps_close
    
      print,'Median & max errors of peak position: ',median(ome), max(ome) 
      if debug ge 2 then stop, 'FORDS: debug stop and Order plot. Press .C to continue'
      
      ; profile of central order
      prof = fltarr(2*poff+1)
      mm = nord/2
      for k=-poff,poff do for i=0,ncol-1 do prof[k+poff]+=im[i,ys[i]+k+0.5]
      ym = max(prof)
      plot, indgen(2*poff+1)-poff, prof
      oplot, -poff*[1,1], ym*[0,1], li=1
      oplot, poff*[1,1], ym*[0,1], li=1
      
      if debug ge 2 then stop, 'FORDS: central order cut. Press .C to continue'
endif;debug>0
                






return
END
