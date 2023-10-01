;+
; :Description:
;             Calls the barycentric correction for the stellar observations
;
;
; Output : czi (barycentric correction)
;          jdUTC (julian data)
;-
pro call_qbary, jd=jdUTC, barydir=barydir,czi=czi,coords=coords

  debug=0


  ;----------
  ;>> ra nad dec
  ;----------
  ; need to format them correctly
  ; correction takes place in  coords
  ; RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]


  ;----------
  ;>> 1_ JdUT
  ;----------

  ; Calculated before entering this procedure.
  ;mjd = jdUTC-2440000.d0  ; This modification was originally made by JD in qbarylog.pro. It was not used but might lead to a hint
  ; in the case of malfunction


  ;---------
  ;>>2_ coords
  ;--------


  ; Validate simbadStarName




  ;---------
  ;>>3_ epoch
  ;--------
  epoch =2000.d0

  ;---------
  ;>>4_ pm
  ;--------
  pm = [0.d0,0.d0]  ; pass dummy values for  proper motion




  ; ***********************************************
  ;  Run Correction
  ; ***********************************************
  if debug gt 0 then begin
    ; >>Input needed for qbary.pro
    print,  ' ------------------------------------   '
    print, 'coordinates : '  ; RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]
    print, 'RA : '+strt(coords[0])+ '  DEC : '+strt(coords[1])
    print, 'pm     : ' + string(pm)       ; proper motion [ra,dec] in ARCsec/year [optional]
    print, 'epoch  : ' + string(epoch)
    print, 'barydir: ' + string(barydir)   ; Barycentric directory
    print, 'jdUTC  : ' + string(jdUTC)    ; julian date (double precision) eg 2448489.3462d0
  endif

  if coords[0] eq 0.0 or coords[1] eq 0.0 then begin
    print, ' ** The coordinates (RA and DEC) for the star '+ strtrim(simbadStarName,2) + ' were not found. Check for the name **'

    czi= 0.0
  endif else begin
    qbary,jdUTC,coords,epoch,czi,obs='ctio', pm = pm,barydir=barydir, ha=ha
  endelse

  ;   >>Output of qbary.log
  ;   print, 'ha : hour angle of observation '+string(ha)
  ;   print, 'czi :relativistic redshift' +string(czi)



end
