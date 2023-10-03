;+
; :Description:
;
;  5 main tasks :
;  1) Creation of .log files using the procedure logmaker.pro
;  2) Processing of the spectra using sorting_hat.pro : by default it will create  a master stellar
;  3) Barycentric Correction
;  4) Shift of spectra using the barycentric correction
;  5) Order splicing
;
;
;  E.g. (To run spectra individually )
;      reduce_slicer, 181103, /post_process
;
;  E.g. (To produce master stellar)
;      reduce_slicer, 210128, /post_process , /combine_stellar
;
;
;  History :
;         4/16/2021 Written by Jorge Lozano
;
;  Notes :
;         combine_stellar might get altered within sorting_hat if remove_crs:0.5 is set. This is a hack to
;         used to create a master stellar with cr:0.5 AFTER reduction
;-



PRO reduce_slicer,  $
  night, $
  no_log=no_log, $                        ; If set it wont create a new .log file
  no_reduction =no_reduction, $           ; If set it wont reduce the spectra of the given night
  combine_stellar = combine_stellar, $    ; If reduction step is run this determines if reduction should consider  individual frames or
                                          ; collect them to produce a master stellar
  post_process = post_process, $          ; Post processing includes shift found from  barycentric correction +  splice the spectra
  ;star_name=star_name                    ; Name of the Star. IT MUST BE INSERTED AS ONE WORD E.G 'HR2943'.  (as opposed to 'HR 2943' )
                                          ; depricated. Software now finds the name of the star directly from the headers.


; Definning constants in a common block can be referenced by any program unit that declares that common block.
  constants                              
  COMMON all_constants,autom,automJPL,autokm,cms,ckm,radtosec,pctoAU,$
    yeartosec,yrtos,ltyr,lightyear,pctom,secperday,daytosec,$
    century,precise,ddtor,msun,msung,mearth,mmoon,$
    mmoong,rearth,rearthkm,rsun,rsunkm,Gcgs,G,angstrom

  ; Constants/Variables + paths
  cms = 2.99792458d8 ; Constant. Workaround sometimes constants does not give back expected value-> Why?
  
  ctparfn = getenv('CHIRON_CTIO_PATH')
  if ctparfn eq '' then message, 'Before running the pipeline you need to set the environment variable CHIRON_CTIO_PATH to be equal to the full path for your ctio.par file.'
  
  ;#####################################################
  ;# 1) Create .log file
  ;#####################################################

  if ~keyword_set(no_log) then begin ; If this is keyword is set then is expected the log sheet to exist already !!

    print, '**************************************************'
    print, 'Creating the .log file for '+strt(night)+' ......'
    print, '**************************************************'

    if keyword_set(no_bary) then begin
      logmaker_v2, strt(night), /nofoc, prefix='chi'
    endif else begin
      ; Run bary Correction as well
      ; It is very important to get the name of the start Correct to run the barycorrection successfully
      logmaker_v2, strt(night), /nofoc, prefix='chi', stellar_bary_correc=stellar_bary_correc
      ;   stellar_bary_correc(Output): is a list wich elements are strucutres with the form {file_name:obs_file[i] , correction:czi }
    endelse

  endif

  ;#####################################################
  ;# 2) Spectra Reduction
  ;#####################################################


  if ~keyword_set(no_reduction) then begin

    print, '**************************************************'
    print, 'Spectra Reduction for  '+strt(night)+'......'
    print, '**************************************************'


    sorting_hat, strt(night), mode='slicer', /reduce, /getthid, /iod2fits, combine_stellar=combine_stellar ,$
      thar_soln='wvc_slicer_171218.sav',redpar =redpar


  endif




  ;#####################################################
  ;#  Determine name of the files to be considered for either barycentric  and  splice
  ;#####################################################


  cut_spectra=1
  do_shift=1

  if keyword_set (post_process) then begin

    print, ''
    print, 'Runing Post processing for '+strt(night)+'......'
    print, ''

    ; Collecting missing info if any of the other 2 tags were not set.
    if keyword_set(no_reduction) then begin
      ; When not doing reduction ONLY postprocessing
      redpar = readpar(ctparfn)
      redpar.imdir = strt(night)+'\' ; setting some extra variables that will get used.
      redpar.prefix ='chi'+strt(night) +'.'

      redpar.logdir =  redpar.logdir  + '20'+strmid(strt(night), 0, 2)+'\'
    endif else redpar=redpar ;else use the same as in sorting_hat



    ;##################
    ;# Collect bary correction individually or in groups
    ;##################
    ;        if keyword_set(no_log) then begin ; Else we keep the stellar_bary_correc create by logmaker_v2
    ; Need to obtain the 'stellar_bary_correct' by reading the existing sheet
    ; This can be improved if  taken the variable already created in sorting_hat
    logsheet = redpar.rootdir+redpar.logdir+strt(night)+'.log'
    readcol,logsheet, skip=9, obnm, objnm, bin, slit, ra, dec,  mdpt,  exptm , ccdTem, airMass,juDate,baryCorrec, intensity,f='(a10,     a15,       a8,    a10 ,   a14,   a14,     a28,      a12,     a12,      a10,     a17,    a14 , a17  )'

    ;-----------------
    ;>>  master files
    ;-----------------
    
    ; Get all bary correction in 1 array
    ; stellar_bary_correc(Output): is a list wich elements are structures with the format {file_name:obs_file[i] , correction:czi }
    if keyword_set(combine_stellar) then begin
      
      
      ;--------------------------
      ; Clean CRs when combine_stellar and remove_crs:0.5 are set.
      ;--------------------------
      ; I create the master file before looking for it but to do so I have to find individual files all over again
      ; It might seem like repeated logic but it's needed
      bary_indices = where(float(baryCorrec) ne 0.0, c_bary )
      obnm = obnm[ bary_indices]

      if  redpar.remove_crs eq 0.5 then begin  ; before it had ~keyword_set(combine_stellar) and
        
          ;Find individual files all over again 
              
          all_file_names = list()
          
          ;1.  Gather all file names
          for index = 0L, n_elements(obnm)-1 do begin
            file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(redpar.prefix+strt(obnm[index])+'.fits')
            all_file_names.add, file_name
          endfor
          
          
          
          all_file_names = all_file_names.toarray()
  
          
          ; 3. Finding the file numbers to create the stellar file name properly
          ; TODO: I am assuming there will be only 1 stellar file,  have to make algorithim to find discontinuities and get back them 
          ; in grous. I remmeber doing something similar in reduce_ctio.pro
          positions= stregex(all_file_names, '([0-9]+)\.fits$', length=len)
          stellar_indices = all_file_names
          for index = 0L, n_elements(stellar_indices)-1 do begin
              stellar_indices[index]=strmid(stellar_indices[index], positions[index] ,4) ; 4 since we cutting the array to get E.g. .[1568].fits
          endfor
         
           
         ;3. Creating file name 
          master_name= redpar.prefix_tag+'m'+redpar.prefix +strt(stellar_indices[0])+'_'+strt(n_elements(stellar_indices))+'.fits' ;recnums[0]+'_'+strtrim(string(n_elements(recnums)),2)
          
        
          ;4.  Remove CRs  : this is the stack approach
          remove_cr_by_sigma, all_file_names, combine_stellar, redpar=redpar, master_name=master_name  ; The files themselves get updated. So files in the folder fitspec get updated
      endif
      
      
      ;---------------------------
      ;Search for master and place in stellar_bary_correc
      ;---------------------------
      
      ; Search for master file just created
      str_file_type=  redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag+'m'+redpar.prefix+'*.fits'
      print,'Master file search ',str_file_type
      post_process_files = file_search(str_file_type, count = count_master) ; look for the data files. Name of the file itself

      ;Extract Files information from those found master file  :
      if count_master le 0  then stop, 'reduce_slicer:  STOP : No master files found for post process. Check the files exist within the directory '
      stellar_bary_correc=list()
      print,'count_master is ',count_master
      for index = 0L, count_master-1 do begin ; For the number of master files present.
        file_name =post_process_files[index]
        file_name =file_name.extract("[0-9]{4}_[0-9]+")
        split = strsplit(file_name, '_', /extract)
        obnm_matches = indgen( LONG(split[1]) )+LONG(split[0])

        indices=list()
        foreach obnm_candidate, obnm_matches do begin
          match_idx= where(obnm eq obnm_candidate, nn)
          if nn gt 0  then indices.Add, match_idx
        endforeach
        indices=indices.ToArray() ; All the indices(accoring to our bigger array) for the current master file
        print,'indices are',indices
        print,'bary_indices are',bary_indices
        print,'baryCorrec are',baryCorrec[bary_indices[indices]]
        bary_corrections= float(baryCorrec[bary_indices[indices]])
        print,'bary_corrections are',bary_corrections
        bary_mean =mean(bary_corrections)
        print,'mean barycentric correction is ',bary_mean

        stellar_bary_correc.add, {file_name:post_process_files[index] , correction:bary_mean}


      endfor
      
    endif else begin
      ; >> For all individual files
      bary_indices = where(float(baryCorrec) ne 0.0, c_bary )
      if c_bary le 0 then stop, 'reduce_slicer:  STOP : No barycentric corrections were identified from the .log file. '
      obnm = obnm[ bary_indices]
      baryCorrec = baryCorrec[bary_indices]
      stellar_bary_correc=list()

      for index = 0L, n_elements(obnm)-1 do begin
        stellar_bary_correc.Add, {file_name:redpar.prefix+strt(obnm[index])+'.fits' , correction:baryCorrec[index] } ; Meant to be output
      endfor
      
      
      ;--------------------------
      ; Clean CRs when runnning individually and remove_crs:0.5 is set.
      ;--------------------------
      if  redpar.remove_crs eq 0.5 then begin  ; before it had ~keyword_set(combine_stellar) and

          all_file_names = list()
          ;1.  Gather all file names
          foreach structure, stellar_bary_correc do begin
            file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(structure.file_name,2)
            all_file_names.add, file_name
          endforeach
          all_file_names = all_file_names.toarray()
  
          ;2.  Remove CRs  : this is the stack approach
          remove_cr_by_sigma, all_file_names, combine_stellar ; The files themselves get updated. So files in the folder fitspec get updated
      endif


    endelse


    foreach structure, stellar_bary_correc do begin ; Iterate of each file to post process


      if ~keyword_set(combine_stellar) then begin
        ; Modifying path of individual files since they have the path of prev rar directory
        file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(structure.file_name,2)

      endif else file_name = structure.file_name ; for master stellar we already modfied the name of the file

      ;Read data: this data is the output of sorting_hat
      new_cube = readfits(file_name, hd) ; recall data come in the form [2,4112,74] where [0,*,*] -> wavelength

      n_orders =(size(new_cube))[3]






      ;#########################
      ;# Flattend the spectrum usign the convex hull technique.
      ;#########################
      ; For the algortihim to work properly it's neccesary to
      if redpar.flat_spec eq 3 then begin
        for order = 0L, n_orders-1 do begin
          new_cube[1,*,order] = flat_spectrum(new_cube[1,*,order]) ; reliable as far as there are no CRs
        endfor
      endif


      ;##################
      ;# Remove Artifact
      ;##################

      if (cut_spectra eq 1) then begin
        print, ''
        print, ' Removing artifact for night '+strt(night)+'......'
        print, ''

        new_cube= splice_spectrum( new_cube, /maskArtifact)

      endif





      ;#########################
      ; Remove CRs | fft approach, order by order
      ;#########################
      ; SIDE EFFECT: If combine stellar set and fft approach set, this will apply fft to a master file (WARNNING master file could have already been removed CRs)
      if  redpar.remove_crs eq 4 then begin ; Previously it was limited for individual only. Meaning I added  ~keyword_set(combine_stellar)

        print, ''
        print, ' Cleaning all CRs from ' +structure.file_name
        print, ''

        total_crs=0
        for order = 0L, n_orders-1 do begin
          order_crs = 0

          new_cube[1,*,order] = cr_remove_fft( new_cube[1,*,order] , redpar.sigma_multiplier, redpar.skirt_level, redpar.frac, order_crs) ; reliable as far as there are no CRs
          ; order_crs gets changed within and we can use its values now
          total_crs = total_crs +  order_crs

        endfor


        history_str = 'CR-CLEANED :  ' + strt(total_crs) + ' crs found using FFT.'
        print, history_str
        sxaddpar, hd, 'HISTORY', history_str
      endif



      ;##################
      ;# Cut spectra
      ;##################

      if (cut_spectra eq 1) then begin
        print, ''
        print, ' Cutting the spectra for night '+strt(night)+'......'
        print, ''


        ; pass array along with type of cut
        splice_type= 'pixel_cut_of_3200px'
        new_cube= splice_spectrum( new_cube, splice_type=splice_type)

        history_str = ' The orders were spliced using '+ splice_type
        sxaddpar, hd, 'HISTORY', history_str

      endif else print, "REDUCE_SLICER :  The Spectra has NOT been reduced in size "



      ;#####################################################
      ;# 3) Flattened spectrum after extraction : using divition by  flats
      ;#####################################################

      ; to finish
      if redpar.flat_spec eq 1 or  redpar.flat_spec eq 2 then begin
        produce_cube_flats, night

        flat_names= ['_flat.fits', '_smooth_flat.fits']
        name = strt(night)+flat_names[redpar.flat_spec -1]
        name = redpar.rootdir+ redpar.flatdir+ 'cube_flats\'  +name

        flat=readfits(name)  ; reads 2 x 3200 x 73
        new_cube[1,*,*] =  new_cube[1,*,*] / flat[1,*,*]



        history_str = ' Spectra was divided by flat  '
        sxaddpar, hd, 'HISTORY', history_str



      endif



      ;##################
      ;# Wavelength Shift
      ;##################

      if (do_shift eq 1)  then begin
        print, ''
        print, 'Shifting wavelengths for night '+strt(night)+'......'
        print, ''

        new_cube[0,*,*]=new_cube[0,*,*]*(1+ ( structure.correction / cms )  )
        ;recall radial velocity is in [m/s] . cms is brought from different procedure. CMS is the speed of light in [m/s]
        ;Make remark in header
        history_str = ' Wavelength (bary)corrected by a factor of : '+ strt(structure.correction) + ' [m/s]'
        sxaddpar, hd, 'HISTORY', history_str


      endif else   print, "REDUCE_SLICER :  The Spectra has NOT been shifted to compensate for the barycentric correction "





      ;#####################################################
      ;# 4) Merge them
      ;#####################################################

      if  redpar.merge_orders eq 1 then begin
        ; join all orders in one big 2 dimensional array
        ; or order = 0L, n_orders-1 do begin
        all_wavelengths =[]
        all_intensities =[]
        for order = 0L, n_orders-1 do begin
          intensities = REFORM( new_cube[1,*,order] )
          wavelengths = reform( new_cube[0,*,order] )
          ; -----
          ; Check for intersection for MERGING
          ; -----
          if order gt 0 then begin
            if prev_wavalengths[-1] gt wavelengths[0]  then begin
              ;These orders intersect. We keep the prev_intensities. Thus, cut the intentisities at index target
              diffs =  wavelengths-prev_wavalengths[-1]
              diff_idxs = where(diffs ge 0 , ns)
              intensities = reform( intensities [ diff_idxs[0]: * ] )
              wavelengths = reform( wavelengths [ diff_idxs[0]: * ] )
            endif
          endif

          prev_wavalengths = wavelengths
          all_wavelengths  = reform( [ all_wavelengths , wavelengths ] )
          all_intensities  = reform( [ all_intensities, intensities ] )
        endfor

        new_cube = make_array(2, n_elements(all_intensities), /double   ) ; not a cube anymore but we maintain the same notation.
        new_cube[0,*] = all_wavelengths
        new_cube[1,*] = all_intensities

      endif

      ;p=plot(wavelengths,intensities, /overplot  ) ;, title=t)



      ;#####################################################
      ;# 4) Store in directory  post_procesed
      ;#####################################################
      ; Store them individual_in new_folder

      processed_file_path =  redpar.rootdir + redpar.fitsdir + redpar.imdir  +'post_processed\'
      if ~file_test(processed_file_path) then spawn, 'mkdir '+ string(34B) + processed_file_path + string(34B)
      path_slides= strsplit(file_name, '\',  /EXTRACT)
      processed_file_path =processed_file_path + path_slides[-1]

      writefits,  processed_file_path, new_cube, hd




    endforeach





  endif


  print, '**************************************************'
  print, '               End Of Scripts'
  print, '**************************************************'





END