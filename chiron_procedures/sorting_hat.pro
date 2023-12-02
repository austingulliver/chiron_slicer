;+
;	NAME: SORTING_HAT
;
;	PURPOSE: To sort files according to binning and slit pair with ThAr
;				to run reduction code for extraction
;
; Modes for the sorting hat: 
; 	narrow: narrow slit (3x1) fast r/o (templates and I2 
;				alpha Cen mode) R=137,000
;	slicer: slicer (3x1) fast r/o R=80,000
;	slit:  slit (3x1) fast r/o  R=96,000
;	fiber: fiber (4x4) slow r/o R=27,000
;
; KEYWORDS:
;
;	OPTIONAL KEYWORDS: 
;  
;	REDUCE: runs reduce_ctio (reduce and get thar soln before running iod2fits)
;          if running reduce, need to pass in array of flats
;
; IOD2FITS: matches thar solutions to correct observations and writes 
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
;       sorting_hat, '120313', run='achi120313', mode='narrow',/reduce, obsnm=[1008,1090, 1091] 
;  
; MODIFICATION HISTORY:
;	20110313 - Modified from old code to work with new dual amp readout ~DAF
; 	20110321 - Narrow slit now ignores objects with the name 'junk' ~MJG
;	20110331 - Now uses quartz for order finding if neither alpha cen nor a bstar
;					are present. ~MJG 
;  20110414 - Fixed a bug when processing files at the start of a new run ~MJG
;  20110808 - structured  (AT)
;  20120419 - Modified to work with Torrent 4 amp readout. Added 1x1 binning as an option. ~MJG
;  20121023 - Added the ThAr filename to redpar to be written to the FITS header ~MJG
;  20201201 - Various changes including Windows compatability. Read summary paper   ~J. Andres Lozano
;-
;
pro sorting_hat, night, run=run, iod2fits=iod2fits, reduce=reduce, $
doptag=doptag, end_check=end_check, skip=skip, $
thar_soln=thar_soln, getthid=getthid, mode = mode, obsnm=obsnm, $
bin11 = bin11, flatsonly=flatsonly, tharonly=tharonly, combine_stellar=combine_stellar, redpar =redpar 	

angstrom = '!6!sA!r!u!9 %!6!n'

;#####################################################
;### Set ctio.par  initial variables 
;#####################################################

redpar.imdir = night+'\'  ; pass night into redpar
redpar.date = night
redpar.versiond=systime()

if ~keyword_set(run) then begin
    imdir = redpar.rootdir+redpar.rawdir+redpar.imdir
    
    l = strlen(imdir)  
    tmp = file_search(imdir+'*.fits', count = count) ; look for the data files  
    if count eq 0 then begin 
        print, 'SORTING_HAT: The run name could not be determined!'      
    endif  
    ;check the QA prefix: no less than 5 files per night!
    sel = where(strmid(tmp,l,2) eq 'qa')
    if n_elements(sel) gt 5 then run = strmid(tmp[sel[0]],l,4) else run = 'chi'+night  
endif ;run not specified 


if strpos(run,'.') lt 0 then run=run+'.' ; add the point
redpar.prefix = run                      ; E.g chiYYMMDD.



if ~keyword_set(mode) then stop, 'SORTING_HAT: MODE is not defined. Returning from sorting_hat'
modeidx = (where(mode eq redpar.modes))[0]   ; which mode?
if keyword_set(bin11) then modeidx += 4
redpar.mode = modeidx                        ; pass current mode to other programs
if modeidx lt 0 then  stop, 'SORTING_HAT: >> ERROR << unrecognized mode. Returning from sorting_hat'
    
logsheet = redpar.rootdir+redpar.logdir+night+'.log'
iodspec_path = redpar.rootdir+redpar.iodspecdir+redpar.imdir
fits_path = redpar.rootdir+redpar.fitsdir+redpar.imdir
if ~file_test(fits_path) then spawn, 'mkdir '+ string(34B) + fits_path + string(34B)
if ~file_test(iodspec_path) then spawn, 'mkdir '+ string(34B) + iodspec_path + string(34B) 

thid_path = redpar.rootdir+redpar.thidfiledir
thid_path_dat =   redpar.rootdir+redpar.thiddir
custom_thid_path = redpar.rootdir+redpar.customthidsol
pretag = redpar.prefix_tag



;-------------------
;-----WORK AROUND --
;-------------------
;If combine_stellar AND remove_cr : then reduce each night individually and
;back in reduce_slicer we create the actual master file
;We do this just so we don'thave to alter the finding/ writting master stellar logic

if keyword_set (combine_stellar) and (redpar.remove_crs eq 5) then begin
    combine_stellar = 0 ; we "unset" so it runs them individually
    combine_stellar_changed = 1
endif else combine_stellar_changed = 0
;-------------------


;#####################################################
;### Read .log sheet + parse 
;#####################################################

;readcol,logsheet, skip=9, obnm, objnm, i2, mdpt, juDate,baryCorrec,  exptm, bin, slit, f='(a5, a13, a4, D14,    D20,   a15, a8, a3, a6)'
readcol,logsheet, skip=9, obnm, objnm, bin, slit, ra, dec,  mdpt,  exptm , ccdTem, airMass,juDate,baryCorrec, intensity, f='(a10,     a15,       a8,    a10 ,   a14,   a14,     a28,      a12,     a12,      a10,     a17,    a14 , a17  )'

;now to expand the quartz items in the logsheet:
;print, 'obnm before is: ', obnm       ; These are all file numbers E.g. 1001  1002  .....
   

qcombs = where(strlen(obnm) gt 4)


for qi=0, n_elements(qcombs)-1 do begin ;Converts ranges found in log sheet to the each number in range
	qinit = strmid(obnm[qcombs[qi]], 0,4)
	qfini = strmid(obnm[qcombs[qi]], 5,4)
	ncombs = long(qfini) - long(qinit) + 1
	nobnm = lindgen(ncombs) + long(qinit)
	nobjnm = strarr(ncombs)+objnm[qcombs[qi]]
	;ni2 = strarr(ncombs)+i2[qcombs[qi]]
	nmdpt = strarr(ncombs)+mdpt[qcombs[qi]]
	nexptm = strarr(ncombs)+exptm[qcombs[qi]]
	nbin = strarr(ncombs)+bin[qcombs[qi]]
	nslit = strarr(ncombs)+slit[qcombs[qi]]

	if qcombs[qi] eq 0 then begin  ; First file number
	  obnm = [strt(nobnm, f='(I04)'), obnm[(qcombs[qi]+1):*]]
	  objnm = [nobjnm, objnm[(qcombs[qi]+1):*]]
	  ;i2 = [ni2, i2[(qcombs[qi]+1):*]]
	  mdpt = [nmdpt, mdpt[(qcombs[qi]+1):*]]
	  exptm = [nexptm, exptm[(qcombs[qi]+1):*]]
	  bin = [nbin, bin[(qcombs[qi]+1):*]]
	  slit = [nslit, slit[(qcombs[qi]+1):*]]
	endif
	if qcombs[qi] ne n_elements(obnm)-1 and qcombs[qi] ne 0 then begin   ; All file numbers in between 
	  obnm = [obnm[0:(qcombs[qi]-1)], strt(nobnm, f='(I04)'), obnm[(qcombs[qi]+1):*]]                                      ; Observation Number
	  objnm = [objnm[0:(qcombs[qi]-1)], nobjnm, objnm[(qcombs[qi]+1):*]]                                                   ; Object Number
	 ; i2 = [i2[0:(qcombs[qi]-1)], ni2, i2[(qcombs[qi]+1):*]]                                                               ; I2
	  mdpt = [mdpt[0:(qcombs[qi]-1)], nmdpt, mdpt[(qcombs[qi]+1):*]]                                                       ; Mid -Time (Universtal Time) 
	  exptm = [exptm[0:(qcombs[qi]-1)], nexptm, exptm[(qcombs[qi]+1):*]]                                                   ; Exposure Time 
	  bin = [bin[0:(qcombs[qi]-1)], nbin, bin[(qcombs[qi]+1):*]]                                                           ; Binninng
	  slit = [slit[0:(qcombs[qi]-1)], nslit, slit[(qcombs[qi]+1):*]]                                                       ; Slit
	endif
	if qcombs[qi] eq n_elements(obnm)-1 then begin  ; Last File number
	  obnm = [obnm[0:(qcombs[qi]-1)], strt(nobnm, f='(I04)')]
	  objnm = [objnm[0:(qcombs[qi]-1)], nobjnm]
	  ;i2 = [i2[0:(qcombs[qi]-1)], ni2]
	  mdpt = [mdpt[0:(qcombs[qi]-1)], nmdpt]
	  exptm = [exptm[0:(qcombs[qi]-1)], nexptm]
	  bin = [bin[0:(qcombs[qi]-1)], nbin]
	  slit = [slit[0:(qcombs[qi]-1)], nslit]
	endif
	
	qcombs[qi:*] += (ncombs - 1)
endfor
   ;print, 'obnm after is: ', obnm
ut = get_julian_date(mdpt) ; floating-point hours, >24h in the morning

;-> Up to here: variables obnm,objnm, i2,mdpt ....  are vectors that contain data of log sheet 


; 
;#####################################################
;### Creates log structure which has information about ALL raw files. 
;#####################################################

createLogStructures,redpar,obnm,objnm, /doFromScratch   ; uncomment this for production 




;#####################################################
;################## Image Analysis ####################
;#####################################################
;
;mydebug =1
;;> Used to find the std of all stellar images to be gathered together.
;if mydebug eq 1 then begin
;  
;      restore, redpar.rootdir+redpar.logstdir+'20'+strmid(redpar.date, 0, 2)+'/'+redpar.date+'log.dat' ;restore the log structure (contains .log info)   
;      
;      stellarIdx = where(strt(log.imgtype) eq 'object' and  strt(log.ccdsum) eq '3 1' )
;      
;      print, 'Number of files found : '+ strt(n_elements(stellarIdx) )
;      
;      stellarFileNames = log.filename[stellarIdx]
;      
;      
;      
;      counter = 0
;      ;> Create Data Cube with all stellar frame
;      print, ' Combining'
;      foreach stellarFile, stellarFileNames do begin        
;          if counter eq 0 then begin          
;             im_ref=readfits(stellarFile)
;             sz        = size(im_ref)
;             n_col     = sz[1]          ;# columns in image
;             n_row     = sz[2]          ;# rows in image
;             data_cube =  fltarr(n_col,n_row, n_elements(stellarIdx))        
;             data_cube[*,*,counter] = im_ref
;          endif else begin
;             img=readfits(stellarFile)
;             data_cube[*,*,counter] = img
;          endelse
;          counter= counter +1
;      endforeach
;      
;      columuns = [356,745,1520] ;from 0 to 1431
;      rows = [3575,4012, 1250,2056]; from 0 to 4112
;      foreach column, columuns do begin
;        
;        foreach row, rows do begin
;          ;pdf = HISTOGRAM(data_cube[column, row, *], LOCATIONS=xbin,  nbins=100)
;          
;          
;          values = reform(data_cube[column, row, *]) ; value to evaluate on
;          
;          p =plot(values, SYMBOL=1)
;          
;          
;          
;          
;          print, size(values)
;          mean_pixel =mean( values )
;        
;          
;          
;          print, 'mean value : ' +string(mean_pixel)
;          std_pixel =stddev(values)
;          print, 'std : ' +string(std_pixel )
;          
;          values = data_cube[column, row, *] ; value to evaluate on
;          ;wa want to draw how that gaussian looks like 
;          params= [ 1, mean_pixel, std_pixel] ;
;          
;          y= gaussian(values, params)
;          
;          p= plot(y, title='Prob values  ')
;          
;          print, 'done '
;          
;;          parms[0] = maximum value (factor) of Gaussian,
;;          parms[1] = mean value (center) of Gaussian,
;;          parms[2] = standard deviation (sigma) of Gaussian.
;;          (if parms has only 2 elements then sigma taken from previous
;;          call to gaussian(), which is stored in a common block).
;;          parms[3] = optional, constant offset added to Gaussian.
;
;        endforeach
;
;
;      endforeach
;
;    
;
;      
;      ; > Manipulation of data cube 
;       std_frame = stddev(data_cube,  dimension= 3)
;       writefits, 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\stellar_210208_pixels_std.fits', std_frame
;;       meanStd = mean(std_frame, /double)
;;       print,  'The mean of all pixels standard deviations are: ' + strt(meanStd)
; 
;        
;endif
;


  



;#####################################################
;################## Reducing Data ####################
;#####################################################

if keyword_set(reduce) then begin
  
        restore, redpar.rootdir+redpar.logstdir+'20'+strmid(redpar.date, 0, 2)+'/'+redpar.date+'log.dat' ;restore the log structure (contains .log info)     
    		xsl=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx],n_modes) ;bin =binning E.g. 3x1
        if xsl[0] lt 0 then stop, 'SORTING_HAT: no files found! Stopping.' 
        if n_modes gt 0 then begin
    
        obnm1=obnm[xsl]     ; Fitlered for mode Observation number
        objnm1=objnm[xsl]   ; Filtered for mode Object name
        
        
        ;###################################################################################
        ;################## CR Cleaning from : Flats + Stellar ####################
        ;###################################################################################

      
        ;Remove CR's by sigma Clipping. It takes place BEFORE reduction and normalizes the images to a common value before sigma clipping       
        if redpar.remove_crs eq 1  then begin 
          PRINT, ' '
          print, 'SORTING-HAT:                    >>> Removing Cosmic Rays  <<< '
          print, ' '       
          
          removeCRs_sigma_clip, redpar, log     

              
        endif 
;        
;        
       
        
        
        
        ;#####################################################
        ;################## Master Bias   ####################
        ;#####################################################
    		        
    		        
              if redpar.biasmode eq 0 then begin    ; Otherwise at the moment of getting each image the bias is reduced in other way    		                          
        		       
        		       binning = strtrim(string(redpar.binnings[modeidx]),2)
        		       masterBiasPath = redpar.rootdir+redpar.biasdir+ redpar.date+'_'+binning+'_'+ redpar.master_bias +'_bias.fits';Try to restore before creating from scrath             		 
              		;masterBiasPath = redpar.rootdir+redpar.biasdir+ redpar.date+'_bin31_normal_medbias.dat'               
              		
              		 if (~file_test(masterBiasPath)) or (redpar.bias_from_scratch eq 1) then begin  
              		       
              		       PRINT, ''
                  		   PRINT, ''
                  		   print, " REDUCE-CTIO :           >>> Creating Master Bias <<<   "
                  		   PRINT, ''
                  		   PRINT, ''
              		           		  
              		      CASE binning OF               		          
              		          '3x1': chi_masterbias, redpar = redpar, log = log, /bin31, /normal ; If interested in the fast mode please change this to /fast or add a call the the CASE statement. 
              		          '4x4': chi_masterbias, redpar = redpar, log = log, /bin44, /normal
              		          '1x1': chi_masterbias, redpar = redpar, log = log, /bin11, /normal 
              		      ENDCASE             		    
              	  endif
              endif
            
            
            
            ;#####################################################
            ;## Parse dash from .log  flat file obversation numbers 
            ;#####################################################
            
        		flatindx=where(objnm1 eq 'quartz',num_flat)   ;Gathering all quartz/flat files        
         		if num_flat gt 0 then begin 
            		  tmp=[0]
            		  for ii=0,num_flat-1 do begin  ; fabricate flat fields
                			dum=obnm1[flatindx[ii]]
                			if strlen(dum) gt 5 then begin    ; no need. code above take care of ranges
                				gf=fix(strmid(dum,0,4))  &  gl=fix(strmid(dum,5,4))
                				diff=gl-gf+1
                				tmp=[tmp,gf+indgen(diff)]
                			endif else tmp=[tmp,dum] ; adding file number together
            		  endfor
            		  flatset=tmp[1:*]  ; throw out the dummy
            endif else begin
                 print, 'SORTING_HAT: No flat files found. Returning.'
                 return
            endelse
        
        		thariodindx=where(objnm1 eq 'thar' or objnm1 eq 'iodine',num_thariod) ; gathering all THAR and OIDINE files found in log        	
            thar = fix(obnm1[thariodindx])      
              
        		starindx=where(objnm1 ne 'iodine' and objnm1 ne 'thar' $
        			             and objnm1 ne 'focus' and objnm1 ne 'junk' and objnm1 ne 'dark' $
                           and objnm1 ne 'bias' and objnm1 ne 'quartz' and objnm1 ne 'master_stellar', num_star)
            
            
            
            
            
            ; out of the star indices I want to classify them in a dictionary to find out if there is more than  1 start
            ; Run of combine_stellar tag is set . 
            
            
            if keyword_set (combine_stellar) then begin               
                combine_stellars=dictionary() ; Where the keys are the star names and the values are lists with all the the corresponding observation numbers 
                foreach idx_star, starindx do begin 
                  refined_key = objnm1[idx_star]
                  refined_key = refined_key.replace( ' ', '') ; Get Rid of any dash 
                  refined_key = refined_key.replace( '-', '') ; get rid of empty space
                  refined_key = 'star_'+refined_key
                  ;first characters cannot be number : We simple append 'star' at the beginnign 
                    if combine_stellars.HasKey( refined_key ) then begin 
                      combine_stellars[ refined_key ].Add,  fix(obnm1[idx_star]) ; Add the obersevation number                   
                    endif else begin
                      combine_stellars[ refined_key ]= list(obnm1[idx_star] )
                    endelse
                    
                endforeach
            endif else combine_stellars = !NULL

             
            
                                       
            if keyword_set(flatsonly) then starindx = where(objnm1 eq 'quartz')                               
            if keyword_set(flatsonly) then thar = 0
        		star = fix(obnm1[starindx]) ; file numbers
        		if keyword_set(obsnm) then star = obsnm
        		if keyword_set(tharonly) then star = 0        		     
        
        		if redpar.debug ge 2 then print, 'Sorting-HAT: before calling reduce_ctio'
        		
        		;#####################################################
            ;################## Actual Reduction #################
            ;##################################################### 
            if redpar.flat_from_scratch eq 0 then begin
                  reduce_ctio, redpar, mode, star=star, thar=thar, date=night, combine_stellar=combine_stellars, mstr_stellar_names=mstr_stellar_names ; Since not flatset passed then it will try to restore master flat for the present night            
                  
            endif else if redpar.flat_from_scratch eq 1 then begin
                  reduce_ctio, redpar, mode, flatset=flatset, star=star, thar=thar, date=night, combine_stellar=combine_stellars, mstr_stellar_names=mstr_stellar_names ; Actual reduction code
                  ;flatset : array containning the file numbers ONLY of quartz/flat files
                  ;thar : "   "    "                            ONLY thar and iodine files
                  ;start: "   "   "                             ONLY  start itself
            
              
            endif else stop, 'ERROR : The parameter flat_from_scratch in ctio.par must be either 1 or 0 '
        		
    		endif ;n_modes > 0
 endif  ;reduce

  







;#####################################################
;######### Wavelength Calibration ####################
;#####################################################

PRINT, ' ' 
print, 'SORTING-HAT:                    >>> Wavelength Calibration <<< ' 
print, ' '
 
 if keyword_set(getthid) then begin
		xsl=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx],n_modes)

   if n_modes gt 0 then begin 
    	   obnm1=obnm[xsl]  &   objnm1=objnm[xsl]  
    	   tharindx=where(objnm1 eq 'thar',num_thar)
    	   
    	   print, 'SORTING_HAT:  Thorium/Argon files found: '
    	   print, obnm1[tharindx]
    	   thar = obnm1[tharindx] ;  array of strings
    	   
         if keyword_set(thar_soln) then begin              
              thidfile =  custom_thid_path+thar_soln   
              RESTORE, thidfile
              initwvc = wvc          
         endif else begin 	          	   
        		  if strmid(run,0,2) eq 'qa' then begin ; Otherwise find from the existing files
        			     findthid, night, redpar, thidfile,run=run 
        		  endif else  findthid, night, redpar, thidfile
        		  
        		  
        		  
        		  if thidfile eq 'none' or thidfile eq '' then begin 
                   stop, 'ERROR: No previous THID files found, returning. Type ".c"'
        		  endif 
        		  
        		  print, 'SORTING_HAT: Thid file(s) found :'
        		  print, thidfile        		  
        		  RESTORE, thidfile
        		  initwvc = THID.WVC
        		  
    	   endelse ; thar_soln  

         
;    	   print, ' WVC structure restored is  : '
;    	   print, ' ------------------------------------------------------- '
;    	   print,   initwvc
;    	   print, ' ------------------------------------------------------- '

    	   
    	   ;Make  changes to WVC structure if needs    	 
    	   
    	   initwvc[2]=round(redpar.nords);redpar.nords;74. ;73.   ;Number of Orders
    	   initwvc[3]=138 - round(redpar.nords)  ;For nords= 73 this =65 ; before kept as 65 ;Physical Order Base ; OBASE. Note this has to much with what is input into THID.
    	   ;When adding  (Num of Ordes) + (Base Orders) =  Always 139 (Physically is 138 but addition does not account for obase )

    	   
    	   ; BEFORE 
;    	   initwvc[2]=74;redpar.nords;74. ;73.   ;Number of Orders
;    	   initwvc[3]=64  ;67. ; before kept as 65 ;Physical Order Base ; OBASE. Note this has to much with what is input into THID. 
;    	                      ;When adding  (Num of Ordes) + (Base Orders) =  Always 139 (Physically is 138 but addition does not account for obase )
    	   
    	   print, ' Data used for wavelength calibration ( Thid ): '
    	   print, ' ------------------------------------------------------- '
    	   print, 'Number of columns : ' +string(initwvc[1])
    	   print, 'Number of order : ' +string(initwvc[2])
    	   print, 'Order base : ' +string(initwvc[3])
    	   print, 'Polynomial Coefficients : '  	 
    	   print, initwvc[10:*]    	   
    	   print, ' ------------------------------------------------------- '
    	          	   
    	          	   
    	   ;Find reference pixel from 2017 
    	   ;------------------------------ 
    	   dir_2017= redpar.rootdir + redpar.customthidsol+  'achi171218.1003'  ; RECALL THIS FILE HAS 74 ORDERS !
    	   
    	   
    	   order_num= 1 ; This will always remain constant since is wrt to 'achi171218.1003' an this is meant to remain constant as well 
    	   ref_pixel_2017 = find_ref_peak(dir_2017, redpar=redpar, order_num=order_num) ; returns an array with three elements
                                            	   ; peak 1 : found withint the range  r1=[ 600, 800 ]
                                            	   ; peak 2 : found withint the range  r2=[1975,2050]
                                            	   ; peak 3 : found withint the range  r3=[3820,3920]
    	         	   
    	   
    	   
    	   !p.multi=[0,1,1]    	   
    	   for i=0,num_thar-1 do begin ; For each ThAr file encountered 
    	        
        		  isfn = iodspec_path+pretag+run+thar[i]       		   		  
        		  rdsk, t, isfn,1 ;reading ThAr file       		 
        		  print, ''
        		  print, 'SORTING_HAT: ----------ThAr obs ', thar[i], ' ', strt((1d2*i)/(num_thar-1),f='(F8.2)'),'% complete.----------'       		  
        		  rawfn = redpar.rootdir+redpar.rawdir+redpar.imdir+run+thar[i]+'.fits'       	        		  
        		  header = headfits(rawfn)
        		  
        		  if strt(fxpar(header, 'COMPLAMP')) ne 'TH-AR' then begin ;Checkin file is indeed ThAr file 
            			 print, 'SORTING_HAT: The head of following file was missedidentified : '
            			 print, rawfn
            			 print, 'To get rid of it type the following command: '
            			 print, "chi_junk, date='"+redpar.date+"', seqnum='"+thar[i]+"', reason = 'No ThAr Lamp.', /chi_q, /log"            	
            			 stop
        		  endif else begin
      			 
      			      
      			      
      			      ; Finds if there is any pixel shifted wrt to 171218. It's wrt to 2017 since 
      			      ; this is the first wavelength solution that we found
      			      ;---------------------------------------------------------------------------
      			      current_dir = redpar.rootdir + redpar.iodspecdir+ redpar.imdir + redpar.prefix_tag+ redpar.prefix +thar[i]
      			      order_num= 1  ; Need to be the red order at index [1]; 
      			                    ; This will always be the first as well since this is a red order and even if less order are traced
      			                    ; we getting rid of blue order rather than the red. So this order is guarentee to be in here. 
      			      current_ref_pixel = find_ref_peak(current_dir,  order_num=order_num) 

      			      pixel_offset =  mean( [ref_pixel_2017[0]-current_ref_pixel [0] , ref_pixel_2017[1]-current_ref_pixel [1], $
      			                             ref_pixel_2017[2]-current_ref_pixel [2]  , ref_pixel_2017[3]-current_ref_pixel [3], $
      			                              ref_pixel_2017[4]-current_ref_pixel [4]   ] ) ;  ref_pixel_2017 -current_ref_pixel 
      			     
      			      
      			      
      			      try_again:
      			      print, ' * The shift found is ' +string(pixel_offset) + ' between the current ThAr and the one from 2017'


      			      
          			  auto_thid, t, initwvc, 6.0, 6.0, 0.3, thid, awin=6.0, maxres=0.6, /orev, redpar = redpar, pixel_offset=pixel_offset
          			  ;for fiber, narrow and regular slit modes:
          			  ;thid, t, 64., 64.*[8797d,8898d], wvc, thid, init=initwvc, /orev 
          			  
          			  
          			  ;UNCOMMENT THIS TO RUN THE STANDARD MODE OF THID.PRO
          			  obase=64;64. ; before 65 CHANGE  ; obase (scalar) lowest order number in spectrum
          			  mlam = 65.*[8662.4d,8761.9d] ; has to be modified for now treated as dummy because wvc is passed         			  
          			  ;thid, t, obase, mlam, wvc, thid, init=initwvc, /orev             			
            			 
            			 

          			  if thid.nlin lt 700d then begin         
          			     print, 'SORTING_HAT: Not enough lines found in the recent wavelenght calibration '            			  
            			   print, 'SORTING_HAT: Lines found :  '+strt(thid.nlin)
            			   ; IF fails I want to see the img myself
            			   rdsk,sp_2017,dir_2017,1
            			   rdsk,sp_current,current_dir,1
            			   p1=plot(sp_current[*,0],  title ='Black is current, Blue is 2017')
            			   p2=plot( sp_2017[*,0],color='blue',linestyle=2,   /overplot)
            			   B = ''
            			   print, 'SORTING_HAT: The Pixel shift failed to predict the shift maginitude. Try with the following integer values +/-5  ' 
            			   ; Read input from the terminal:
            			   READ, B, PROMPT='Do you want to try again? If YES insert pixel number if not then press x : '
            			   if B eq 'x' then begin 
            			     stop, 'ERROR : Thid.pro did not work as expected.  INTERVENTION NEEDED!'   
            			   endif else  begin
            			     pixel_offset = fix(B)
            			     GOTO, try_again
            			     
            			     
            			   endelse
            			            			   
          			  endif else begin
          			     print, 'SORTING_HAT: Thid.pro successfully identified '+strt(thid.nlin) + ' lines. '
          			  endelse
          			  
          			  
          			  ;Uncomment this if want to save the found wvc structure for future reference 
          			  ;save, wvc , FILENAME = custom_thid_path +'wvc_slicer_'+redpar.date+'.sav' 
            		
            		 ;Saving THID structure as file
          			 fnm = thid_path+pretag+run+thar[i]
          			 fsuf = '.thid'          			 
          			 if file_test(fnm+fsuf) then spawn, 'mv '+fnm+'.thid '+nextname(fnm,fsuf)          			 
          			 save, thid, file=fnm+fsuf
          			 
                 ;Saving calculated wavelengths as file
          			 mkwave, w, thid.wvc
          			 w = reverse(w,2) ; As a rule of thumb NOW spectra increases with increasing order number (from the bluest to the reddest)
          			 fnm = thid_path_dat+'ctio_'+pretag+run+thar[i]
          			 fsuf = '.dat'          			 
          			 if file_test(fnm+fsuf) then spawn, 'mv '+fnm+'.dat '+nextname(fnm,fsuf)
          			 save, w, file=fnm+fsuf         			 
      		  endelse
      		  
    	   endfor ; END for each ThAr file encountered 
    	   
	 endif;n_modes > 0
endif ; getthid


print, ' '
print, ' '
print, 'SORTING-HAT : -------------------------------End Wavelength Calibration '









;###############################################################
;######### Write FITS files for reduced data ####################
;###############################################################
PRINT, ' '
print, 'SORTING-HAT:                    >>> Writing files on disk <<< '
print, ' '

if keyword_set(iod2fits) then begin
     
     
     x1=where(bin eq redpar.binnings[modeidx] and (slit eq redpar.modes[modeidx]) and (objnm ne 'quartz') and (objnm ne 'iodine') and (objnm ne 'thar') and  (objnm ne 'junk') and (objnm ne 'dark') and (objnm ne 'focus') and (objnm ne 'bias'),n_found)          
     tharindx=where((objnm eq 'thar') and (bin eq redpar.binnings[modeidx]) and (slit eq redpar.modes[modeidx]),  num_thar)     
     
     
     ;*******************************************************
     ; Re-storing thid file and saving wavlenghts in cube format
     ;*******************************************************
     
     if  (num_thar gt 0) then begin             
           thidfiles = thid_path+pretag+run+obnm[tharindx]+'.thid' ; array with strings of wavelength solution file names
           wavut = ut[tharindx] ; time of ThAr exposures. Used to calculate shortest exposure
           
           for k=0,num_thar-1 do begin
               res = file_search(thidfiles[k], count=count)  ;Same files created with when doing wavelength calibration
               if count eq 0 then stop, 'SORTING_HAT: Missing THID file '+ thidfiles[k]
           endfor
    
           restore, thidfiles[0] ; Restoring the first one to later restore the rest
           mkwave, w, thid.wvc
           w = reverse(w,2) ; increase with increasing order number
           ww = dblarr(num_thar,n_elements(w[*,0]),n_elements(w[0,*]))
           ww[0,*,*] = w
           for k=1,num_thar-1 do begin ; all other solutions in ww array
               restore, thidfiles[k]
               mkwave, w, thid.wvc
               w = reverse(w,2) ; increase with increasing order number
               ww[k,*,*] = w
           endfor     
     endif else stop, " ERROR : No extracted ThAr files were found. "
     

     
     ;*******************************************************  
     ; 1) Reduces "master" Stellar image
     ;*******************************************************   
     if  keyword_set ( combine_stellar ) then  begin
      foreach mstr_stellar_name, mstr_stellar_names do begin

            path_mst_stellar= iodspec_path+pretag+mstr_stellar_name
    
            rdsk,spectra_all_stellar,path_mst_stellar,1   ; Reading previously saved spectra (intensities only)
            rdsk,hd,path_mst_stellar,2                    ; Reading header of file above
            sz=size(spectra_all_stellar)  &   ncol=sz[1]    &    nord=sz[2]
            spec=dblarr(2,ncol,nord)
            w = ww[0,*,*]                      ; Since we dealing wiht "master" we arbitrary choose 1st wavesolution
            thidfile_name = thidfiles[0]       ; Used on Header
            spec[0,*,*]=w                      ; Wavelengths for all oders
            spec[1,*,*]=spectra_all_stellar    ; Intensities for all orders (Spectrum )
    
            outfile=pretag+mstr_stellar_name+'.fits' ; Final fits file name.
    
            ;   Modifying "master" stellar  fits headers:
            ;*******************************************************
    
            nt = n_tags(redpar) ;number of tags in the redpar structure
            tnms = string(tag_names(redpar), format='(A-8)')
            endhd = hd[-1]
            hd = hd[0:n_elements(hd)-2]
    
            for ii=0, nt-1 do begin; Due to space constraint delete directory keywords from headers
              remlen = 78 - strlen(tnms[ii]+' = ')
              vals = redpar.(ii)
              val = strt(vals[0])
              for j=1, n_elements(vals)-1 do begin
                val += ', '+strt(vals[j])
              endfor
              hd = [hd, tnms[ii]+'= '+"'"+string(val+"'", format='(A-'+strt(remlen)+')')]
            endfor
            hd = [hd, string('THARFNAM', format='(A-8)')+'= '+"'"+string(thidfile_name+"'", format='(A-'+strt(remlen)+')')]
            hd = [hd,endhd]
    
    
            ;      ; Due to space constraint delete directory keywords from headers :
            sxdelpar, hd, 'LOGSTDIR'
            sxdelpar, hd, 'LOGDIR'
            sxdelpar, hd, 'IODSPECD'
            sxdelpar, hd, 'FITSDIR'
            sxdelpar, hd, 'THIDFILE'
            sxdelpar, hd, 'FLATDIR'
            sxdelpar, hd, 'ORDERDIR'
            sxdelpar, hd, 'BIASDIR'
            sxdelpar, hd, 'CUSTOMTH'
            sxdelpar, hd, 'THIDDIR'
            ;
    
            ;now change the NAXIS and NAXISn values to reflect the reduced data + ADD thid related keywords:
            specsz = size(spec)
            ;fxaddpar, hd, 'OBJECT', 'master_stellar', 'This file was produced by combining '+ string (n_found)+ ' stellar images. '   ; This statment is replaced within reduce_ctio.pro
            fxaddpar, hd, 'BZERO', 0, 'offset data range to that of unsigned short'; added so data does not get shifted when read
            fxaddpar, hd, 'NAXIS', specsz[0], 'Number of data axes'
            fxaddpar, hd, 'NAXIS1', specsz[1], 'Axis 1 length: 0=wavelength, 1=spectrum'
            fxaddpar, hd, 'NAXIS2', specsz[2], 'Axis 2 length: extracted pixels along each echelle order'
            fxaddpar, hd, 'NAXIS3', specsz[3], 'Axis 3 length: number of echelle orders extracted'
            fxaddpar, hd, 'RESOLUTN', thid.resol, 'Resolution determined from the ThAr.'
            fxaddpar, hd, 'THIDNLIN', thid.nlin, 'Number of ThAr lines used for wavelength solution.'
    
    
            writefits,fits_path+outfile, spec,hd
            print, 'SORTING_HAT:  Writing: ', outfile, ' '
        
      endforeach
      

      
      
      
     ;*******************************************************  
     ; 2) Reduces ALL  Stellar files. One by one
     ;*******************************************************     
     endif else  if  (n_found gt 0)  then begin                    

         for i=0,n_found-1 do begin	; For ALL observation matching selected mode                  			
          			
          		 
          			obnm[i]=strtrim(obnm[x1[i]])
          			nxck=0
          			if keyword_set(skip) then xck=where(obnm[x1[i]] eq skip,nxck) ;Inherited from Yale. To check if want to skip input file          			
          			if nxck eq 0 then begin	           			  
            			  
            			  
            			  ; Match intensity with wavelength for all valid files of current date
            			  ;*******************************************************                 			  
            			  file_path = iodspec_path+pretag+run+obnm[x1[i]]
            				rdsk,sp,file_path,1   ; Reading previously saved spectra (intensities only)
            				rdsk,hd,file_path,2   ; Reading header of file above
            				sz=size(sp)  &   ncol=sz[1]    &    nord=sz[2]
            				spec=dblarr(2,ncol,nord)              				
                       
                    ut0 = ut[x1[i]]
                    timediff = abs(ut0 - wavut)
                    sel = (where(timediff eq min(timediff)))[0]   ;Out of all wavelength solutions found with thid.pro pick the one closest to present (inside the loop) file
                    w = ww[sel,*,*]                               ; 'ww' was found when restoring wavelength solution above 
                    thidfile_name = thidfiles[sel]                ;selected (shortest )ThAr filename              
                    
            				spec[0,*,*]=w          				     ; Wavelengths for all oders
            				spec[1,*,*]=sp                     ; Intensities for all orders (Spectrum )
            				outfile=pretag+run+ strtrim(string(obnm[i]),1)+ '.fits' ; Final fits file name.  
            				
            				
            				
            				;      Adding reduction code info to fits headers:
            				;*******************************************************
            				
            				nt = n_tags(redpar) ;number of tags in the redpar structure
            				tnms = string(tag_names(redpar), format='(A-8)')
            				endhd = hd[-1]
            				hd = hd[0:n_elements(hd)-2]
            				
            				for ii=0, nt-1 do begin  ; Due to space constraint, delete directory keywords from headers :       				  
              				  remlen = 78 - strlen(tnms[ii]+' = ')
              				  vals = redpar.(ii)
              				  val = strt(vals[0])
              				  for j=1, n_elements(vals)-1 do begin
                  					 val += ', '+strt(vals[j])                        					 
              				  endfor
              					 hd = [hd, tnms[ii]+'= '+"'"+string(val+"'", format='(A-'+strt(remlen)+')')]
            				endfor
            				print, FILE_BASENAME(thidfile_name, '.fits')
            				hd = [hd, string('THARFNAM', format='(A-8)')+'= '+"'"+string(FILE_BASENAME(thidfile_name, '.fits')+"'", format='(A-'+strt(remlen)+')')]
            				hd = [hd,endhd]
            				

            				; Due to space constraint, delete directory keywords from headers :
            				sxdelpar, hd, 'LOGSTDIR'
            				sxdelpar, hd, 'LOGDIR'
            				sxdelpar, hd, 'IODSPECD'
            				sxdelpar, hd, 'FITSDIR'
            				sxdelpar, hd, 'THIDFILE'
            				sxdelpar, hd, 'FLATDIR'
            				sxdelpar, hd, 'ORDERDIR'
            				sxdelpar, hd, 'BIASDIR'
            				sxdelpar, hd, 'CUSTOMTH'
            				sxdelpar, hd, 'THIDDIR'
            				
            				;now change the NAXIS and NAXISn values to reflect the reduced data + ADD thid related keywords:
            				specsz = size(spec)
            				fxaddpar, hd, 'BZERO', 0, 'offset data range to that of unsigned short'; added so data does not get shifted when read 
            				fxaddpar, hd, 'NAXIS', specsz[0], 'Number of data axes'
            				fxaddpar, hd, 'NAXIS1', specsz[1], 'Axis 1 length: 0=wavelength, 1=spectrum'
            				fxaddpar, hd, 'NAXIS2', specsz[2], 'Axis 2 length: extracted pixels along each echelle order'
            				fxaddpar, hd, 'NAXIS3', specsz[3], 'Axis 3 length: number of echelle orders extracted'
            				fxaddpar, hd, 'RESOLUTN', thid.resol, 'Resolution determined from the ThAr.'
            				fxaddpar, hd, 'THIDNLIN', thid.nlin, 'Number of ThAr lines used for wavelength solution.'
            				
            				
            				writefits,fits_path+outfile, spec,hd
            				print, 'SORTING_HAT:  Writing: ', outfile, '      >> ', strt(i/(n_found - 1d)*1d2),'% complete.'
            				;*******************************************************
            				
            				
            				;*******************************************************
                    ;                    Debugging 
                    ;*******************************************************
            			   if redpar.debug ge 1 and redpar.debug le 2 then begin
                				 fdir = redpar.plotsdir + 'fits/'
                				 spawn, 'mkdir '+fdir
                				 fdir = redpar.plotsdir + 'fits/' + redpar.date
                				 spawn, 'mkdir '+fdir
                				 fdir = fdir + '/halpha'
                				 spawn, 'mkdir '+fdir
                				 fname = fdir+'/'+'halpha'+redpar.prefix+obnm[i]
                				 if file_test(fname+'.eps') then spawn, 'mv '+fname+'.eps '+nextnameeps(fname+'_old')+'.eps'
                				 ps_open, fname, /encaps, /color
                				 !p.multi=[0,1,1]              				 
            			   endif;debug plot fname and dirs          			   
            			   if redpar.debug ge 1 then begin              				
                				 p1=plot( spec[0,*,39], spec[1,*,39], /xsty, /ysty, xtitle='Wavelength['+angstrom+']', ytitle='Flux', title=outfile, yran=[0,1.1*max(spec[1,*,39])] )
            			   endif;debug plotting          			   
            			   if redpar.debug ge 1 and redpar.debug le 2 then begin
                				 ps_close
                				 spawn, 'convert -density 200 '+fname+'.eps '+fname+'.png'
            	       endif ;ps_close & png
            	      ;*******************************************************
                  
                  endif ; end if nxck eq 0
         endfor       ; iod2fits : Massive For loop
   
             
     ; ########
     ; # 3) If none then Stop 
     ; ########      
     endif else  stop, 'ERROR : Tag "iod2fits" was passed but no files to reduce were found. '
     
    
endif ;iod2fits










	
	
	
	
	
	
	
	
;##############################################
;################# End Check ##################
;##############################################
if keyword_set(end_check) then  begin
	    x1=where(bin eq redpar.binnings[modeidx] and slit eq redpar.modes[modeidx] and objnm ne 'quartz',n_check)
      if x1[0] lt 0 then begin
        print, 'SORTING_HAT : no files found! returning'
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





;-------------------
;-PART of WORK AROUND --
;-------------------
;If combine_stellar AND remove_cr : then reduce each night individually and
;back in reduce_slicer we create the actual master file
;We do this just so we don'thave to alter the finding/ writting master stellar logic
; Here I am just setting back to combine_stellar =1 so it returns as such to reduce_slicer
if keyword_set (combine_stellar_changed)  then combine_stellar = 1





return 
end ;sorting_hat.pro
