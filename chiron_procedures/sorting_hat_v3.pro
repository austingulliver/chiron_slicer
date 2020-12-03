;+
;	NAME: SORTING_HAT
;
;	PURPOSE: To sort files according to binning and slit pair with ThAr
;				to run reduction code for extraction
;
; Modes for the sorting hat: 
; 	narrow: narrow slit (3x1) fast r/o (templates and I2 
;				alpha Cen mode) R=120,000
;	slicer: slicer (3x1) fast r/o R=90,000
;	slit:  slit (3x1) fast r/o  R=90,000
;	fiber: fiber (4x4) slow r/o R=28,000
;
; KEYWORDS:
;
;	RUN: The current run number (e.g. 'rqa33') used to find possible 
;		  thar solutions 
;
;	OPTIONAL KEYWORDS: 
;  
;	REDUCE: runs reduce_ctio (reduce and get thar soln before running iod2fits)
;          if running reduce, need to pass in array of flats
;
;  IOD2FITS: matches thar solutions to correct observations and writes 
;				in fits format skip is an array of obnm that don-t need 
;				fits files skip=['2268'] thar_soln is the wavelength array
;				soln (if a matching thar not taken this night)
;
;	DOPPLER: a keyword indicating to run the Doppler code on the observations
;				taken with the input slicer position, run number, and date. 
;
;	DOPTAG:	
;
;  END_CHECK: checks to see that all 3x1 binned observations with input slit have been
;             reduced (iodspec), fits (fitspec), thar_soln
;
;	EXAMPLES: 
;		sorting_hat, '110311', run='rqa31', /narrow, /reduce
;		sorting_hat, '110311', run='rqa31', /narrow, /doppler, doptag='dd'
;  
; MODIFICATION HISTORY:
;	20110313 - Modified from old code to work with new dual amp readout ~DAF
; 	20110321 - Narrow slit now ignores objects with the name 'junk' ~MJG
;	20110331 - Now uses quartz for order finding if neither alpha cen nor a bstar
;					are present. ~MJG 
;  20110414 - Fixed a bug when processing files at the start of a new run ~MJG
;  20110808 - structured  (AT). Remaining hard-coded things are  marked with >>
;-
;
pro sorting_hat, night, run=run, iod2fits=iod2fits, reduce=reduce, doppler=doppler, doptag=doptag, $
		end_check=end_check, skip=skip, thar_soln=thar_soln, getthid=getthid, mode = mode


redpar = readpar('ctio.par')
redpar.imdir = night+'/'  ; pass night into redpar

if ~keyword_set(run) then begin
  imdir = redpar.rootdir+redpar.rawdir+redpar.imdir
  l = strlen(imdir)
  tmp = file_search(imdir+'*.fits', count = count) ; look for the data files
  if count eq 0 then stop, 'No data files found!'
; check the QA prefix
  sel = (where(strmid(tmp,l,2) eq 'qa'))[0]
  if sel ge 0 then run = strmid(tmp[sel],l,4) else run = 'chi'+night
endif

if strpos(run,'.') lt 0 then run=run+'.' ; add the point
redpar.prefix = run

print, 'SORTING_HAT: night '+night+' run: '+run
;stop

;**************************************************************
;   Modes keyword
;**************************************************************
if ~keyword_set(mode) then begin 
    print, 'MODE is not defined. Returning from sorting_hat'
    return
endif

modeidx = (where(mode eq redpar.modes))[0] ; which mode?
if modeidx lt 0 then begin
    print, 'Error: unrecognized mode. Returning from sorting_hat'
    return
 endif

logpath = redpar.rootdir+redpar.logdir
logsheet = logpath+night+'.log'

iodspec_path = redpar.rootdir+redpar.iodspecdir
fits_path = redpar.rootdir+redpar.fitsdir
thid_path = redpar.rootdir+redpar.thiddir
thid_path2 = redpar.rootdir+redpar.thidfiledir
;qbcvelfile = rootdir+'bary/qbcvel.ascii'
;xwids =   redpar.xwids                        ; extraction width  


; >>
readcol,logsheet, skip=9, obnm, objnm, i2, mdpt, exptm, bin, slit, f='(a5, a13, a4, a14, a8, a3, a6)'

ut = gettime(mdpt) ; floating-point hours, >24h in the morning

; ******* END-CHECK *********************
;THE END CHECK TO MAKE SURE EVERYTHING HAS BEEN PROCESSED:
 if keyword_set(end_check) then  begin
	x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] and objnm ne 'quartz',n_check)
        if x1[0] lt 0 then begin
          print, 'Sorting_hat: no files found! returning'
          return
        endif

        	for k=0,n_check-1 do begin
			if objnm[x1[k]] eq 'thar' then begin
				fthar=file_search(thid_path+'*'+obnm[x1[k]]+'*',count=thar_count)
				if thar_count eq 0 then print, objnm[x1[k]]+' '+obnm[x1[k]]+' has no ThAr soln '
			endif else begin
				fiod=file_search(iodspec_path+'*'+obnm[x1[k]]+'*',count=iod_count)
				if iod_count eq 0 then print, objnm[x1[k]]+' '+obnm[x1[k]]+'  has no iodspec'
				
				ffits=file_search(fits_path+'*'+obnm[x1[k]]+'*',count=fits_count)
				if fits_count eq 0 then print, objnm[x1[k]]+' '+obnm[x1[k]]+'  has no fitspec'
			endelse 
		endfor       
      endif ; end_check

; ******* REDUCE *******************
;REDUCING THE DATA:	
   if keyword_set(reduce) then begin
		xsl=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx],n_modes)
                if xsl[0] lt 0 then stop, 'Sorting_hat: no files found! Stopping.' 

		obnm1=obnm[xsl]  &   objnm1=objnm[xsl]  
;  i2=i2[xsl]   &  mdpt=mdpt[xsl]   &   exptm=exptm[xsl]
;		bin=bin[xsl]    &   slit=slit[xsl]
		flatindx=where(objnm1 eq 'quartz',num_flat)

 		if num_flat gt 0 then begin ; process dash in the logfile flat numbers
		  tmp=[0]
		  for ii=0,num_flat-1 do begin  ; fabricate flat fields
			dum=obnm1[flatindx[ii]]
			if strlen(dum) gt 5 then begin
				gf=fix(strmid(dum,0,4))  &  gl=fix(strmid(dum,5,4))
				diff=gl-gf+1
				tmp=[tmp,gf+indgen(diff)]
			endif else tmp=[tmp,dum]
		  endfor
		  flatset=tmp[1:*]  ; throw out the dummy
               endif else begin
                 print, 'Sorting-hat: no flat files found. Returning.'
                 return
              endelse

		thariodindx=where(objnm1 eq 'thar' or objnm1 eq 'iodine',num_thariod)
		print, '******************************'
		print, 'THORIUM ARGON AND IODINE OBSERVATIONS TO BE PROCESSED: '
		print, obnm1[thariodindx]
                thar = fix(obnm1[thariodindx])

		starindx=where(objnm1 ne 'quartz' and objnm1 ne 'iodine' and objnm1 ne 'thar' $
			and objnm1 ne 'focus' and objnm1 ne 'junk' and objnm1 ne 'dark' $
                               and objnm1 ne 'bias', num_star)
               star = fix(obnm1[starindx]) ; file numbers

               star = [star,flatset[0]]  ; add first flat to be extracted
;               xwid = redpar.xwids[modeidx]
;		dpks= redar.dpks[modeidx] 
; >>
;	 	if strmid(run,0,1) eq 'r' then run=strmid(run,1,strlen(run)-1)

;print, 'Run: ',run
;print, 'Flatset: ', flatset        
;stop, 'Sorting-HAT: before calling reduce_ctio'

		reduce_ctio, redpar, mode, flatset=flatset, star=star, thar=thar 
    endif  ;reduce

; ******* ThAr processing *******************	
 if keyword_set(getthid) then begin
		xsl=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx],n_modes)
                if xsl[0] lt 0 then stop, 'Sorting_hat: no files found! Stopping.' 
		obnm1=obnm[xsl]  &   objnm1=objnm[xsl]  

;  i2=i2[xsl]   &  mdpt=mdpt[xsl]   &   exptm=exptm[xsl]
;		bin=bin[xsl]    &   slit=slit[xsl]

		tharindx=where(objnm1 eq 'thar',num_thar)
		print, '******************************'
		print, 'THORIUM ARGON TO BE PROCESSED: '
		print, obnm1[tharindx]
                thar = obnm1[tharindx] ; string array

           if keyword_set(thar_soln) then thidfile =  thid_path2+thar_soln+'.thid' else begin 
    
; >>> mode is ignored here! Use THID files of any mode for this night
; (if found) or other nights as initial guess
             tharf=file_search(thid_path2+'r'+run+'*thid',count=tharcount) ; search this night
             if tharcount gt 0 then thidfile = tharf[tharcount -1] else begin ; last found 
;                 findthid, night, redpar, thidfile, run=run ; search back with run specified
                 if strmid(run,0,2) eq 'qa' then findthid, night, redpar, thidfile,run=run $  ; search back
                   else  findthid, night, redpar, thidfile
                 if thidfile eq 'none' or thidfile eq '' then begin 
                   stop, 'No previous THID files found, returning. Press .C'
                   return
                   endif 
              endelse ; tharcount
          endelse ; thar_soln  

         print, 'Initial THID file: '+thidfile
         restore, thidfile
         initwvc = thid.wvc 

         print, 'Ready to go into THID'   
         for i=0,num_thar-1 do begin 
               rdsk, t, iodspec_path+'r'+run+thar[i],1
 ;              stop, 'SORTING_HAT: GETTHID for file '+thar[i]+'  .C to continue'
 ;NEW, AUTOMATED WAY OF DOING THINGS:
 ;auto_thid, thar, thid.wvc, 6, 6, 0.4, thid, awin=10, maxres= 4,/orev, kwnord = 40

               auto_thid, t, initwvc, 6, 6, 0.4, thid, awin=10, maxres=4, /orev, kwnord=40, plot=redpar.debug
;               thid, t, 84., 84.*[6654.,6734], wvc, thid, init=initwvc, /orev 
               save, thid, file=thid_path2+'r'+run+thar[i]+'.thid'
               mkwave, w, thid.wvc
               w = reverse(w,2) ; increase with increasing order numver
               save, w, file=thid_path+'ctio_'+run+thar[i]+'.dat'
         endfor
endif ; getthid


;******* Write FITS files for reduced data ************
; from the input logsheet, find all observations matching the selected mode
if keyword_set(iod2fits) then begin
     x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] $
	     and objnm ne 'quartz' and objnm ne 'junk' and objnm ne 'dark' $
            and objnm ne 'focus' and objnm ne 'bias',n_found)
 
     tharindx=where((objnm eq 'thar') and (bin eq redpar.binnings[modeidx]) and (slit eq redpar.modes[modeidx]),  num_thar)
     if x1[0] lt 0 or num_thar eq 0 then stop, 'Sorting_hat: no matching observations or ThAr for iod2fits. Stop'

      print,'Number of '+mode+' observations: ',n_found
      print, 'ThAr files: ', obnm[tharindx]
 
;  thar file is defined on input?
    if keyword_set(thar_soln) then begin 
          restore, thid_path2+thar_soln+'.dat' ; w (wavelength array)
       endif else begin             
; get all wavelength solutions for this night and this mode,e UT of ThAr exposures 
         wavfiles = thid_path+'ctio_'+run+obnm[tharindx]+'.dat' ; string array of wavelength solution file names  
         wavut = ut[tharindx] ; time of ThAr exposures
; check existence of wavelength solutions, stop if not found
        for k=0,num_thar-1 do begin
            res = file_search(wavfiles[k], count=count)
            if count eq 0 then stop, 'Missing WAVE file '+wavfiles[k]
        endfor 

         restore, wavfiles[0] ; w, first solution of the night
         ww = dblarr(num_thar,n_elements(w[*,0]),n_elements(w[0,*]))
         ww[0,*,*] = w
         for k=1,num_thar-1 do begin ; all other solutions in ww array
           restore, wavfiles[k]
           ww[k,*,*] = w
         endfor
    endelse ;thar_soln 

     for i=0,n_found-1 do begin	
			obnm[i]=strt(obnm[x1[i]], f='(I04)')
			nxck=0
			if keyword_set(skip) then xck=where(obnmx1[[i]] eq skip,nxck) 
			if nxck eq 0 then begin
; >>
				rdsk,sp,iodspec_path+'r'+run+obnm[x1[i]],1   
				rdsk,hd,iodspec_path+'r'+run+obnm[x1[i]],2   
				sz=size(sp)  &   ncol=sz[1]    &    nord=sz[2]
				spec=fltarr(2,ncol,nord)
            if ~keyword_set(thar_soln) then begin ; find closest ThAr
                      ut0 = ut[x1[i]]
                      timediff = abs(ut0 - wavut)
                      sel = (where(timediff eq min(timediff)))[0]  
                      w = ww[sel,*,*]
             endif
				spec[0,*,*]=w
				spec[1,*,*]=sp
				outfile='r'+run+obnm[i]+'.fits'
				writefits,fits_path+outfile, spec,hd
			endif
     endfor  ;  iod2fits
  endif ;iod2fits

;***** DOPPLER ********	
if keyword_set(doppler) then begin
		x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] and objnm ne 'quartz' and objnm ne 'junk',n_found)
;		obnm=obnm[x1]  &   objnm=objnm[x1]  &  i2=i2[x1]   &  mdpt=mdpt[x1]   &   exptm=exptm[x1]
;		bin=bin[x1]    &   slit=slit[x1]
		if n_found gt 0 then print,'Number of '+mode[modeidx]+' observations: ',n_found
; Oct 15 2011: plug the Doppler:
                print, 'Doppler is not yet operational in sorting_hat. Returning.'
                return
	endif ; doppler
	


end 
;**************************************************************
