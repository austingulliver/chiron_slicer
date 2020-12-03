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

debug=redpar.debug
if n_params() lt 5 then begin
  print,'syntax: ctio_spec,prefix,spfname,outfname,redpar,orc[,xwid,flat,thar,cosmics ]'
  retall
endif

print,''
print,'CTIO_SPEC: Entering routine. Extracting file ....'
;cancel previous erros 
CATCH, /CANCEL

print,'CTIO_SPEC: Raw File      >>',spfname

; Read the image file
im = getimage(spfname, redpar, header=head)  
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

if ncol ne ncolf  then begin
  print, 'CTIO_SPEC: HALT! Your image is not the same size as your flat!'
  stop
endif











 ;*******************************************************
 ;   Extract Spectrum 
 ;*******************************************************
     
if keyword_set(thar) then begin
    ; ThAR Spectrum - no cosmic removal
     getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar
    
endif else begin
    ; Stellar Spectrum
    ;getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar, cosmics=cosmics, optspec=optspec, diff=replace, sky=sky
    getspec, im, orc, xwid, spec, gain=redpar.gain, ron=redpar.ron, redpar = redpar, optspec=optspec, diff=replace, sky=sky
    
endelse


spec_o = spec ;save the original spec










if redpar.debug ge 1 then begin
    print, '***********************************************'
    print, 'RIGHT BEFORE FLAT-FIELDING...'
    print, '***********************************************'
    stop, ' '
endif



 ;*******************************************************
 ;   Flat Fielding 
 ;*******************************************************
     
if keyword_set(flat) and redpar.flatnorm eq 1 then spec = double(spec)/flat else print, 'CTIO_SPEC: WARNING: no flat-field correction!'

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
        if redpar.debug ge 1 and redpar.debug le 2 then begin
          	 fname = fdir+'/'+redpar.prefix+redpar.seqnum+'_Ord'+strt(i)
          	 if file_test(fname) then spawn, 'mv '+fname+' '+nextnameeps(fname+'_old')
          	 ps_open, fname, /encaps, /color
        endif;debug plots
        
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

endif;debug plots
; END Plotting - Debug 









 
print,'CTIO_SPEC: Extracted File >> ' + outfname
extracted=size( spec )
num_pixel_extracted = extracted[1]
num_orders_extracted =  extracted[2]

print, 'CTIO_SPEC: Extracted spectrum has '+string(num_pixel_extracted) +' pixels and ' +string(num_orders_extracted) +' orders.'
print, ''

spec=rotate(spec,2) ; Spectrum get rotated  
wdsk,spec,outfname,1,/new			;write image to disk
;writefits,  'C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\files_sent_dr_gulliver\median_tharSpec/181103_1003_extracted_spec.fits', spec 

wdsk,head,outfname,2
    

; DEBUG
;   plot, intspec[*,0]
;   tvscl, congrid(intspec,n_elements(intspec[*,0], 8*n_elements(intspec[0,*]))
;   stop, 'CTIO-SPEC DEBUG: spectrum plot'
		
return
end
