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
;  20110808 - structured version (AT). Remaining hard-coded marked with >>
;-
;
pro sorting_hat_v1, night, run=run, iod2fits=iod2fits, reduce=reduce, doppler=doppler, doptag=doptag, $
		end_check=end_check, skip=skip, thar_soln=thar_soln, mode = mode
; narrow=narrow, slicer=slicer, normslit=normslit, fiber=fiber, 


redpar = readpar('ctio.par')
redpar.imdir = night+'/'  ; pass night into redpar
redpar.prefix = 'chi'+night+'.' 

if ~keyword_set(run) then run = 'chi'+night

print, 'SORTING_HAT: night '+night
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

; >>> mode is ignored here! It is assumed that THID is already run!
tharf=file_search(thid_path+'ctio_'+run+'*.dat',count=tharcount)

; >> check if this works in general case ???
if tharcount le 0 then begin
   print, 'No THID files for this night'
endif

;  runnum = long(strmid(run, 3, strlen(run) - 3))
;  runnum--
;  lastrun = strmid(run, 0, 3)+strt(runnum)
;retrieve the thid files already processed:

;  spawn, 'ls -1 '+thid_path2+lastrun+'.*', res
;take the result, shorten it to just the observation nubmer, then convert to long:
; >>
;  lress = long(strmid(res, 26, 4)) ; this will not work in general case
;  mlress = strt(lress[n_elements(lress)-1])
;  tharf=file_search(thid_path+'ctio_'+lastrun+'.'+mlress+'.dat',count=tharcount)

;endif ; tharcount

;if tharcount gt 0 then begin  ; .dat is found in the thid_path for this run
;  thar_obnm=strarr(tharcount)
;truncate
;  for i = 0, tharcount-1 do begin;
;	a1=strpos(tharf[i],'_')+1
;	thar_obnm[i]=strmid(tharf[i],a1,10)
;  endfor
;endif ; tharcount

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

		obnm=obnm[xsl]  &   objnm=objnm[xsl]  &  i2=i2[xsl]   &  mdpt=mdpt[xsl]   &   exptm=exptm[xsl]
		bin=bin[xsl]    &   slit=slit[xsl]
		flatindx=where(objnm eq 'quartz',num_flat)

 		if num_flat gt 0 then begin ; process dash in the logfile flat numbers
		  tmp=[0]
		  for ii=0,num_flat-1 do begin
			dum=obnm[flatindx[ii]]
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

		thariodindx=where(objnm eq 'thar' or objnm eq 'iodine',num_thariod)
		print, '******************************'
		print, 'THORIUM ARGON AND IODINE OBSERVATIONS TO BE PROCESSED: '
		print, obnm[thariodindx]
                thar = fix(obnm[thariodindx])


		starindx=where(objnm ne 'quartz' and objnm ne 'iodine' and objnm ne 'thar' $
			and objnm ne 'focus' and objnm ne 'junk' and objnm ne 'dark',num_star)
               star = fix(obnm[starindx]) ; file numbers

; >>
;		bstar_indx=where(objnm eq '128620', num_bstar)
;		if num_bstar eq 0 then bstar_indx=where(strupcase(strmid(objnm,0,2)) eq 'HR',num_bstar)
;		if num_bstar eq 0 then bstar_indx=where(strupcase(strmid(objnm,0,6)) eq 'QUARTZ',num_bstar)
;	        bstar_ind=obnm[bstar_indx[0]]

;		order_ind = -1 ; defined orders on summed flat field

;               xwid = redpar.xwids[modeidx]
;		dpks= redar.dpks[modeidx] 
; >>
;	 	if strmid(run,0,1) eq 'r' then run=strmid(run,1,strlen(run)-1)

;print, 'Run: ',run
;print, 'Flatset: ', flatset        
;stop, 'Sorting-HAT: before calling reduce_ctio'

		reduce_ctio, redpar, mode, flatset=flatset, star=star, thar=thar 
    endif  ;reduce


;******* Write FITS files for reduced data ************
; from the input logsheet, find all observations matching the selected mode
if keyword_set(iod2fits) then begin
	  x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] $
	     and objnm ne 'quartz' and objnm ne 'junk' and objnm ne 'dark' and objnm ne 'focus',n_found)
          if x1[0] lt 0 then stop, 'Sorting_hat: no matching observations for iod2fits. Stop'

		obnm=obnm[x1]  &   objnm=objnm[x1]  &  i2=i2[x1]   &  mdpt=mdpt[x1]   &   exptm=exptm[x1]
		bin=bin[x1]    &   slit=slit[x1]
	if n_found gt 0 then print,'Number of '+mode+' observations: ',n_found
    ; find the matching thar file
    	if ~keyword_set(thar_soln) then begin
	    	for j=0,tharcount-1 do begin
    			xx=where(thar_obnm[j] eq obnm,nxx) 
    		endfor
; >>
	   	 restore,tharf[xx[0]]   ; w (wavelength array)
	 endif else restore, thid_path+thar_soln
		for i=0,n_nfound-1 do begin	
			obnm[i]=strt(obnm[i], f='(I04)')
			nxck=0
			if keyword_set(skip) then xck=where(obnm[i] eq skip,nxck) 
			if nxck eq 0 then begin
; >>
				rdsk,sp,iodspec_path+run+'.'+obnm[i],1   
				rdsk,hd,iodspec_path+run+'.'+obnm[i],2   
				sz=size(sp)  &   ncol=sz[1]    &    nord=sz[2]
				spec=fltarr(2,ncol,nord)
				spec[0,*,*]=w
				spec[1,*,*]=sp
				outfile=run+'.'+obnm[i]+'.fits'
				writefits,fits_path+outfile, spec,hd
			endif
		endfor  ;  iod2fits
	endif ;iod2fits

;***** DOPPLER ********	
;DOPPLER CODE:
if keyword_set(doppler) then begin
		x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] and objnm ne 'quartz' and objnm ne 'junk',n_found)
		obnm=obnm[x1]  &   objnm=objnm[x1]  &  i2=i2[x1]   &  mdpt=mdpt[x1]   &   exptm=exptm[x1]
		bin=bin[x1]    &   slit=slit[x1]
		if n_found gt 0 then print,'Number of '+mode[modeidx]+' observations: ',n_found
; Oct 15 2011: plug the Doppler:
                print, 'Doppler is not yet operational in sorting_hat. Returning.'
                return

;		xa=where(objnm eq '128620' and i2 eq'y',nxa)  &  print,'Number of aCen A observations: ',nxa
;		xb=where(objnm eq '128621' and i2 eq 'y',nxb)
;		xiod=where(strupcase(strmid(objnm,0,2)) eq 'HR' or objnm eq 'iodine' and i2 eq 'y',nxiod);
;			print,'Number of Iodine observations: ',nxiod
;		for k=0,nxiod-1 do begin
;                   if strmid(obnm[xiod[k]],0,1) eq '0' then obnm[xiod[k]]=strmid(obnm[xiod[k]],1,3)
;                   dr_iod_soln, run+'.'+obnm[xiod[k]], tag=doptag, /ctio4k
;                   dop_create_ipcf, tag=doptag, observatory='ctio4k'
;		endfor ;iodines 
;		for j=0,n_nxa-1 do begin	
;			dr_obsa,obsnm=run+'.'+obnm[xa[j]],tag=doptag,run=run
;		endfor ; acen A
	endif ; doppler
	


end 
;**************************************************************
