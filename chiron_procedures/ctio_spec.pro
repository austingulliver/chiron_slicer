;Summary:
;  Reads in image,  divides by FLAT FIELD.
;  Extracts spectrum and background from each order, subtracts background,
;
;Notes:
;   By Default extraction of ThAr  has no CR removal nor  sky substraction is applied
;
;Input:
;    PREFIX (input string)   prefix of all file names (i.e., 'qa11.' for qa11.nnnn.fits)
;        to all observations made with a particular spectrograph setting.
;        The following files are expected to exist:
;	 * prefix.sum - Summed flat field (from addwf.pro)
;	 * prefix.ord - default order location coefficients (from ctio_dord)
;    SPFNAME  (input string) filename of given observation.
;    OUTFNAME (input string) complete path and filename of wdsk-d output.
;    REDPAR   global parameter structure
;    ORC order location coefficients
;    NOSKY (flag) Throw flag to supress sky subtraction.  Use for ThAr
;        images or to speed things up if sky subtraction (which is
;        actually a scattered light subtraction) should not be
;        performed.  Do not combine this flag with cosmics, which
;        requires a good bg subtraction
;    COSMICS (flag) Throw this flag to initiate cosmic ray removal.
;
;
;Output:
;   The following file may be created by ctio_spec:
;	  * spfname.ord - order location coefficients, if they were determined
;         OUTFNAME is the path and filename of the output, reduced spectrum.
;         OUTFNAME.opt -- optimally extracted spectrum from the cosmic
;                         ray removal algorithm.
;
;History:
; 12-Oct-201 AT added parameter file as argument, removed common


pro CTIO_spec,prefix,spfname,outfname,redpar, orc, xwid=xwid, flat=flat, cosmics=cosmics, thar=thar

  debug=1
  if n_params() lt 5 then begin
    print,'syntax: ctio_spec,prefix,spfname,outfname,redpar,orc[,xwid,flat,thar,cosmics ]'
    retall
  endif

  print,''

  print,'CTIO_SPEC: Entering routine. Extracting file ....'
  ;cancel previous erros
  CATCH, /CANCEL

  print,'CTIO_SPEC: Raw File  >> ',spfname.substring(strpos( spfname, 'chi', /REVERSE_SEARCH) )



  ;*******************************************************
  ;   Flat Fielding  BEFORE extraction
  ;*******************************************************
  ;
  ;If variable remove_cr:2 in the ctio.par file then we remove CR in this section using La Cosmic
  ;

  if redpar.remove_crs eq 2    then begin

    print, '         Removing CR s.  Please Wait ..........    '


    ;Time info to place in header once cr's removed
    todayDate = SYSTIME()
    todayDate = strmid(todayDate,20,4) + strmid(todayDate,3,7)

    ;Read only the header :
    frame_hdr = headfits(spfname)

    ; To run La Cosmic I need
    ; 1) The gain
    gain= float(sxpar(frame_hdr,'gain11'))
    ; 2) Th noise
    rn = float(sxpar(frame_hdr,'ron11'))

    input_frame= spfname
    mask_frame = input_frame.insert("-mask", spfname.IndexOf('.fits') )
    input_frame= spfname
    cleaned_frame = input_frame.insert("-out", spfname.IndexOf('.fits') )


    la_cosmic,spfname,gain=gain,readn=rn,sigclip='21.0', /verbose ; ,sigfrac=0.75          ; out: file with image to be cleaned
    ; gain: Gain of the CCD (< 0 to estimate) (default=-1.0)
    ; readn:  Readnoise of CCD (default=0.0)
    ; Th result of lacosmic are 2 files created 1 contains the cleaned image and the other contains the mask
    ; file out (pchi181103.xxxx.fits) has not being alteared
    ; Two files are created  as output of calling this procedur

    ; Once image is read in memory
    ; delete the frames created by la_comics
    mask_cr= readfits(mask_frame)
    dummy= where(mask_cr EQ 1, n_crs )
    print, '         -> Frame : '+ spfname.substring(strpos( spfname, 'chi', /REVERSE_SEARCH) )+  ' has ' + strt(n_crs) + ' cosmic rays'

    file_delete, mask_frame

    head=frame_hdr ; this works as input and output
    ; I need to pass the header reference of the original file as input.
    im = get_image(cleaned_frame, redpar, header=head)
    ; It will output the header 'head'


    history_str = 'CR-CLEANED : '+todayDate+' : ' + strt(n_crs)+ ' CRs removed using LaCosmic.
    ; The statement "CR-CLEANED" serves as reference to identify if the file has been cleaned previouly. Do not remove.

    sxaddpar, head, 'HISTORY', history_str

    file_delete, cleaned_frame








  endif else im = get_image(spfname, redpar, header=head)  ; Retrieve the image as normal.

  if (size(im))[0] lt 2 then begin
    print, 'CTIO_SPEC: Image is not found. Returning from CTIO_SPEC.'
    stop
  endif

  sz = size(im)
  ncol = sz[1]				  ;# columns in image
  nrow = sz[2]				  ;# rows in image
  szf = size(flat)
  ncolf = szf[1]				;# columns in image
  nrowf = szf[2]				;# rows in image

  if  keyword_Set(flat) and (ncol ne ncolf ) and ((redpar.flatnorm ne 0) )  then begin
    stop, 'CTIO_SPEC: HALT! Your image is not the same size as your flat!'

  endif

  ;*******************************************************
  ;   Flat Fielding  BEFORE extraction
  ;*******************************************************
  if  keyword_Set(flat) and  ( (redpar.flatnorm eq 0) or (redpar.flatnorm eq 2) or (redpar.flatnorm eq 4) ) then begin
    im = double(im)/flat
  endif

  ;*******************************************************
  ;   Extract Spectrum
  ;*******************************************************

  if keyword_set(thar) then begin
    ; ThAR Spectrum - no cosmic removal
    getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar

  endif else begin
    ; Stellar Spectrum
    ;getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar, cosmics=cosmics, optspec=optspec, diff=replace, sky=sky ;  APPLYING CR
    
    if redpar.scatter_light gt 0 then begin
      im =substract_scatter_light(im, orc, redpar = redpar)
    endif
    
    getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar, optspec=optspec, diff=replace, sky=sky

  endelse

  spec_o = spec ;save the original spec


  ;*******************************************************
  ;   Flat Fielding  AFTER extraction
  ;*******************************************************

  if keyword_Set(flat)  and ( (redpar.flatnorm eq 1) or (redpar.flatnorm eq 3) ) then begin
    spec = double(spec)/flat
  endif


  specsz = size(spec)
  nords = specsz[2]


  ;*******************************************************
  ;   Plotting - Debugging
  ;*******************************************************

  if redpar.debug ge 1 and redpar.debug le 2 then begin
    !p.multi=[0, 1, 3]
    fdir = redpar.plotsdir + 'extracts/'
    spawn, 'mkdir '+fdir
    fdir = redpar.plotsdir + 'extracts/' + redpar.imdir
    spawn, 'mkdir '+fdir
    fdir = redpar.plotsdir + 'extracts/' + redpar.imdir + redpar.seqnum
    spawn, 'mkdir '+fdir


    for i=0, nords-1 do begin
      ;if redpar.debug ge 1 and redpar.debug le 2 then begin
      ;  fname = fdir+'/'+redpar.prefix+redpar.seqnum+'_Ord'+strt(i)
      ;  if file_test(fname) then spawn, 'mv '+fname+' '+nextnameeps(fname+'_old')
      ;  ps_open, fname, /encaps, /color
      ;endif;debug plots

      if redpar.debug ge 1 then begin
        plot, spec_o[*,i], title=redpar.prefix+redpar.seqnum+' Order '+strt(i)+' Extracted', /xsty, /ysty, ytitle='Flux'
        plot, flat[*,i], title=redpar.date+' '+redpar.modes[redpar.mode]+' Mode Order '+strt(i)+' Flat', /xsty, /ysty, ytitle='Flux'
        plot, spec[*,i], title=redpar.prefix+redpar.seqnum+' Order '+strt(i)+' Spec/Flat', /xsty, /ysty, $
          xtitle='Dispersion Direction [pix]', ytitle='Flux'
      endif

      if redpar.debug ge 1 and redpar.debug le 2 then begin
        ps_close
        spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
      endif
    endfor

  endif
  ;*******************************************************
  ;   END Plotting - Debugging
  ;*******************************************************

  print,'CTIO_SPEC: Extracted File >> ' + outfname
  extracted=size( spec )
  num_pixel_extracted = extracted[1]
  num_orders_extracted =  extracted[2]

  print, 'CTIO_SPEC: Extracted spectrum has '+strtrim(string(num_pixel_extracted),1) +' pixels and ' +strtrim(string(num_orders_extracted),1) +' orders.'
  print, ''

  spec=rotate(spec,2) ; Spectrum get rotated
  wdsk,spec,outfname,1,/new			;write image to disk
  wdsk,head,outfname,2

  return
end
