; 
; - Used for Boxcar Extraction 
; -This subroutine extracts a curved arc (arc) from an image array (im). The
;   curvature of the arc is determined from polynomial fit coefficients (orc)
;   which traces the curvature of the echelle orders. The particular
;   arc to extract is specified by an order number (onum), which need not be
;   integer. Positions of noninteger orders are interpolated from surrounding
;   orders.
; - Extracts one order at the time.
;   
;  Input: 
;     - im (input array (# columns , # rows)) image from which to extract arc.
;     - orc (input array (# of coeff per fit , # of orders)) coefficients from PIT
;       fit of column number versus row number (of echelle orders, usually). The
;       polynomials trace arcs (orders, usually) indexed by order number, begining
;       with zero closest to row zero and increasing as row number increases.;   
;    - onum (input scalar) order number of arc to extract - need not be integral.
;    - awid (input scalar) full width of arc to be extracted, in the cross dispersion direction.
;          Two specifications are possible:
;             awid <= 1, awid is fraction of the local distance between orders to mash.
;             awid >  1, awid is the specific number of pixels to mash. E.g for Slicer = 12
;             
;  Output: 
;   - arc (output vector (# columns)) counts PER PIXEL in arc extracted from image.
;   - pix [pix (output vector (# columns)] returns the fractional number of pixels
;     mashed in each column to make arc.
;   
;   
;29-Nov-91 GB translated from ANA
;22-Dec-91 GB made to return zeros if arc off image
;11-11-2020  Update by Jorge Lozano

pro getarc,im,orc,onum,awid,arc,pix,hotpix=hotpix, debug = debug, ybi, yti
   
   if n_params() lt 5 then begin
      print,'syntax: getarc,im,orc,onum,awid,arc[,pix]'
      retall
   endif
   
   ;print, 'GETARC : Orders are getting extracted using BOXCAR. Number of pixels extract in the Cross-Dispersion : ' +strtrim(string(awid),1)
  
   if awid le 0 then message,'GETARC: Arc width must be positive - aborting.' 
  
  ;Define useful quantities
    im=double(im)
    
    nrow=n_elements(im(0,*))			  ;number of rows
    ncol=n_elements(im(*,0))        ;number of rows
    maxo=n_elements(orc(0,*))-1		 	;maximum order covered by orc
    ix=findgen(ncol)			        	;vector of column indicies. E.g 4112
    arc=ix*0.0					            ;dimension arc vector   (Vector 4112 filled with 0s)
    pix=1.0					                ;define in case of trouble
  
  ; Interpolate polynomial coefficients for surrounding orders to get polynomial
  ; coefficients.  Note that this is mathematically equivalent to interpolating
  ; the column indicies for surrounding orders, since the column indicies are
  ; linear functions of the polynomial coefficients. However, interpolating
  ; coefficients should be faster.
  ; The +/- 10000 is to force argument of LONG to be positive before truncation.
  
  if awid le 1 then begin			;awid is an order fraction
      	 if onum lt awid or onum gt maxo-awid then begin ;onum must be covered by orc
      	   message,'Requested order not covered by order location coefficients.'
      	 endif 
      
      	 ob=onum-awid/2.0				;order # of bottom edge of arc
      	 ;ob=onum-awid				;order # of bottom edge of arc
      	 obi=long(ob+10000)-10000			;next lowest integral order #A
      	 cb=orc(*,obi)+(ob-obi)*(orc(*,obi+1)-orc(*,obi))
      	 yb=poly(ix,cb)				;row # of bottom edge of swath
      	 ybi=long(yb+10000)-10000			;lowest pixel number in swath
      	 ybfrac=yb-ybi				;fraction of ybi to exclude
      	 if min(yb) lt 0 then begin			;check if arc is off bottom
      	   trace,0,'GETARC: Warning - requested arc is above top of image.'
      	   return
    	   endif
    
      	 ot=onum+awid/2.0				;order # of top edge of arc
      	 ;ot=onum+awid				;order # of top edge of arc
      	 oti=long(ot+10000)-10000			;next lowest integral order #
      	 ct=orc(*,oti)+(ot-oti)*(orc(*,oti+1)-orc(*,oti))
      	 yt=poly(ix,ct)				;row # of top edge of swath
      	 yti=long(yt+10001)-10000			;highest pixel number in swath
      	 ytfrac=yti-yt				;fraction of yti to exclude
      	 if max(yt) gt nrow-1 then begin		;check if arc is off top of im
        	   trace,0,'GETARC: Warning - requested arc is below bottom of image.'
        	   return
      	 endif
  endif else begin				;awid is number of pixels. Default case
      	 
      	 if onum lt 0 or onum gt maxo then begin	;onum must be covered by orc
      	     message,'GETARC: * ERROR: Requested order not covered by order location coefficients.'
      	     STOP, ''
      	 endif 
      	 
      	 ob=onum					            ;order # of middle of arc
      	 obi=long(ob+10000)-10000			;next lowest integral order #
      	 cb=orc(*,obi) + (ob-obi) * (orc(*,obi+1) - orc(*,obi))        ; DO WE REALLY NEED THIS ?
      	 yb=poly(ix,cb)-awid/2.0	>0		;row # of bottom edge of swath
      	 ybi=long(yb+10000)-10000	>0		;lowest pixel number in swath
      	 ybfrac=yb-ybi				        ;fraction of ybi to exclude
      	 
      	 if min(yb) lt 0 then begin			;check if arc is off bottom
        	   trace,0,'GETARC: Warning - requested arc is below bottom   of image.'
        	   stop
        	   return
      	 endif
      	 
      	 
      	 ot=onum                        ;order # of middle of arc
      	 oti=long(ot+10000)-10000      ;next lowest integral order #
      	 ct=orc(*,oti)+(ot-oti)*(orc(*,oti+1)-orc(*,oti))
      	 yt=poly(ix,ct)+awid/2.0  < nrow-1     ;row # of top edge of swath
      	 yti=long(yt+10001)-10000 <  nrow-1    ;highest pixel number in swath
      	 ytfrac=yti-yt                 ;fraction of yti to exclude

      	 if max(yt) gt nrow-1 then begin   ;check if arc is off top of im
      	   trace,0,'GETARC: Warning - requested arc is above top of image.'
      	   return
      	 endif
      	 
  endelse
  
  
  
  
 
  
  ;  for col=0,ncol-1 do begin			;sum image in requested arc
  ;    scol=im(col,ybi(col):yti(col))
  ;    arc(col)=total(scol)
  ;  end
  ;  faster method for summing arc
  
  
  imask = intarr(n_elements(im[*, 0]), max(yti)-min(ybi)+1) ; approximetly same size as img. Initialized with 0's
  isrow = fltarr(n_elements(im[*, 0]), max(yti)-min(ybi)+1) ; same but meant to store double
  
  for row=min(ybi),max(yti) do begin		;loop through valid rows
      srow=im(*,row)				;get CCD row : vector  along the dispersion direction. Contains some pixels than actually are within the order ( the others are background)
      sup =im(*,row+1)
      sdn =im(*,row-1)
      ;Is masking the pixels that are background
      mask=srow*0.				;make a mask for it
      madd=where(row ge ybi and row le yti,nummad)	;choose pixels in this row, madd is the same size as the srow, as ybi, as yti
      if nummad gt 0 then mask(madd)=1.				;that belong in this order
    
      ;Cosmic-Ray Remover
    ;  if keyword_set(hotpix) then begin
    ;  wid = 3
    ;  thresh = 5000
    ;  collow = min(madd)
    ;  colhi  = max(madd)
    ;  sp = srow(collow:colhi)
    ;  if n_elements(sp) gt wid+5 then begin
    ;    ssp = median(sp,wid)
    ;    d = sp - ssp
    ;    ibad = where(d gt thresh,nbad) + collow  ;pixels that have cosmics
    ;    if nbad gt 0 then begin
    ;      ;plot,srow,/ynoz
    ;      ;oplot,ibad,srow(ibad),ps=4,symsize=2,co=300
    ;      srow(ibad) = 0.5*(sdn(ibad) + sup(ibad))     ;replace bad pixels
    ;      ;oplot,srow,co=130
    ;      ;wait,2
    ;    end
    ;   end
    ;   end
        isrow(*, row-min(ybi)) = srow
        imask(*, row-min(ybi)) = mask
        ; srow x mask gives me back the a vector (where the values that survided after multipling by mask will be  the ones elected )
        ; Trick: arc : will keep begin a vectorwith the 4112  values but as it iteratates in the cross dispersion direction it will sum over the 12
        ; corresponding pixels for each column
        arc=arc+srow*mask				;add them into extracted spectr
  endfor
  
  ;Define vectors along edge of swath.
  vb =im(ix+ncol*ybi)				;bottommost pixels in swath
  vt =im(ix+ncol*yti)				;topmost pixels in swath
  
  ;Now subtract out extra pixels at the top and bottom of the swath.
  arc = arc - 0.5 * (vb+vt)                                     $
            - (vb + 0.5*(im(ix+ncol*(ybi+1))-vb)*ybfrac)*ybfrac $
            - (vt - 0.5*( vt- im(ix+ncol*(yti-1))   )*ytfrac)*ytfrac
  pix = yt - yb					;number of pixels mashed in arc
  arc = double(arc) / pix			;convert to counts/pixel
  
  return
end
