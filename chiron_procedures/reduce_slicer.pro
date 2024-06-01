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



PRO reduce_slicer,                            $
  nights,                                     $        ; If set then nights 
  years=years,                                $        ; If nights is a path then this variable selects the folder corresponding to given years. If not set then all folders are slected.
  no_log=no_log,                              $        ; If set it wont create a new .log file
  no_reduction = no_reduction,                $        ; If set it wont reduce the spectra of the given night
  combine_stellar = combine_stellar,          $        ; If reduction step is run this determines if reduction should consider individual frames or collect them to produce a master stellar
  combine_an = combine_an,                    $        ; If set activate combine across nights 
  stars_nm_cb_an =stars_nm_cb_an,             $        ; Name of stars to combine across nights. Must be given as an array  and star names need to be one word E.G 'HR2943'.  (as opposed to 'HR 2943')
  post_process = post_process,                $        ; Post processing includes shift found from  barycentric correction +  splice the spectra
  automation = automation,                    $        ; If set activate automation process

; Definning constants in a common block can be referenced by any program unit that declares that common block.
  constants                              
  COMMON all_constants,autom,automJPL,autokm,cms,ckm,radtosec,pctoAU,$
    yeartosec,yrtos,ltyr,lightyear,pctom,secperday,daytosec,$
    century,precise,ddtor,msun,msung,mearth,mmoon,$
    mmoong,rearth,rearthkm,rsun,rsunkm,Gcgs,G,angstrom

  ; Constants/Variables + paths
  cms = 2.99792458d8 ; Constant. Workaround sometimes constants does not give back expected value-> Why?
  
  if ~isa(nights, /number) then nights = get_nights_dir(nights, years)
  
  ;Check if pipeline should run in automation mode 
  if keyword_set(automation) then begin
    combine_an=1
    logsheets_paths=LIST()
  endif else begin
    automation = 0
  endelse
  
  foreach night, nights do begin
    if keyword_set(combine_an) then begin
      ; For combine across nights it is necessary for combine_stellar and post_process to be True. 
      combine_stellar=1
      post_process=1
    endif
    
    ;#####################################################
    ;# 1) Create .log file
    ;#####################################################
  
    if ~keyword_set(no_log) then begin ; If this is keyword is set then is expected the log sheet to exist already !!
  
      print, '**************************************************'
      print, 'Creating the .log file for '+strt(night)+' ......'
      print, '**************************************************'
  
      if keyword_set(no_bary) then begin
        logmaker_v2, strt(night), /nofoc, prefix='chi', redpar = redpar, debug = debug 
      endif else begin
        ; Run bary Correction as well
        ; It is very important to get the name of the start Correct to run the barycorrection successfully
        logmaker_v2, strt(night), /nofoc, prefix='chi', stellar_bary_correc=stellar_bary_correc, redpar = redpar, debug=debug
        ;   stellar_bary_correc(Output): is a list wich elements are strucutres with the form {file_name:obs_file[i] , correction:czi }
      endelse
    endif else begin
      ctparfn = getenv('CHIRON_CTIO_PATH')
      if ctparfn eq '' then message, 'Before running the pipeline you need to set the environment variable CHIRON_CTIO_PATH to be equal to the full path for your ctio.par file.'
      redpar = readpar(ctparfn)
    endelse
    
    if keyword_set(automation) then begin
      logsheets_paths.add, redpar.rootdir+redpar.logdir+strt(night)+'.log'
    endif
    if keyword_set(combine_stellar) then n_iterations=1 else n_iterations=0
    
    for i = 0L, n_iterations do begin
        if i eq 1 then combine_stellar=0
  
        ;#####################################################
        ;# 2) Spectra Reduction
        ;#####################################################
      
        if ~keyword_set(no_reduction) then begin
      
          print, '**************************************************'
          print, 'Spectra Reduction for  '+strt(night)+'......'
          print, '**************************************************'
      
          sorting_hat, strt(night), mode='slicer', /reduce, /getthid, /iod2fits, combine_stellar=combine_stellar ,$
            thar_soln='wvc_slicer_171218.sav',automation = automation, redpar =redpar, debug=debug
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
            redpar.imdir = strt(night)+'\' ; setting some extra variables that will get used.
            redpar.prefix ='chi'+strt(night) +'.'
            redpar.prefix_out = 'chi'+strt(night) +'_'
      
            redpar.logdir =  redpar.logdir  + '20'+strmid(strt(night), 0, 2)+'\'
          endif
      
          ;##################
          ;# Collect bary correction individually or in groups
          ;##################
          logsheet = redpar.rootdir+redpar.logdir+strt(night)+'.log'
          readcol,logsheet, skip=9, obnm, objnm, bin, slit, ra, dec,  mdpt,  exptm , ccdTem, airMass,juDate,baryCorrec, intensity,f='(a10,     a15,       a8,    a10 ,   a14,   a14,     a28,      a12,     a12,      a10,     a17,    a14 , a17  )'
          ;-----------------
          ;>>  Master files
          ;-----------------
          ; Get all bary correction in 1 array
          ; stellar_bary_correc(Output): is a list wich elements are structures with the format {file_name:obs_file[i] , correction:czi }
          if keyword_set(combine_stellar) then begin
            
            ;--------------------------
            ; Clean CRs when combine_stellar and remove_crs:5.0 are set.
            ;--------------------------
            ; I create the master file before looking for it but to do so I have to find individual files all over again
            ; It might seem like repeated logic but it's needed
            bary_indices = where(float(baryCorrec) ne 0.0 and slit eq redpar.modes[1] , c_bary )
            obnm = obnm[ bary_indices]
            objnm = objnm[ bary_indices]
      
            if  redpar.remove_crs eq 5.0 then begin
              
                stellar_exp_by_star = dictionary()
                for index = 0L, n_elements(obnm)-1 do begin
                  refined_key = objnm[index]
                  refined_key = clean_key_dictionary(refined_key)
                  file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(redpar.prefix_out+strt(obnm[index])+'.fits')
                  if stellar_exp_by_star.HasKey( refined_key ) then begin
                    stellar_exp_by_star[ refined_key ].add,  file_name ; Add the obersevation number
                  endif else begin
                    stellar_exp_by_star[ refined_key ]= list(file_name )
                  endelse
                endfor
               
                foreach star_exp, stellar_exp_by_star.keys() do begin
                  all_file_names = stellar_exp_by_star[star_exp].ToArray()
                  positions= stregex(all_file_names, '([0-9]+)\.fits$', length=len)
                  stellar_indices = all_file_names
                  for index = 0L, n_elements(stellar_indices)-1 do begin
                    stellar_indices[index]=strmid(stellar_indices[index], positions[index] ,4) ; 4 since we cutting the array to get E.g. .[1568].fits
                  endfor

                  ;3. Creating file name
                  master_name= redpar.prefix_tag+'m'+redpar.prefix_out +strt(stellar_indices[0])+'_'+strt(n_elements(stellar_indices))+'.fits'
                  indir=redpar.rootdir+redpar.fitsdir+redpar.imdir+master_name
                  if (n_elements(stellar_indices) eq 1) then begin
                      spectrum = readfits(all_file_names[0], hd)
                      writefits,  indir, spectrum, hd
                  endif else if (n_elements(all_file_names) eq 2) then begin
                      master_sp = readfits(all_file_names[0], /silent)
                      n_pixels = (size(master_sp))[2]
                      n_orders = (size(master_sp))[3]
                      stellar_stack=make_array(n_pixels, n_orders, n_elements(all_file_names),/double )
                      for idx = 0L, n_elements(all_file_names)-1 do begin
                        spectrum = readfits(all_file_names[idx], hd) ; Will read [2,4112,74]
                        spectrum = reform(spectrum[1,*,*])
                        stellar_stack [*,*,idx]= spectrum
                      endfor
                      if redpar.master_stellar eq 'mean' then begin
                        master_stellar = mean(stellar_stack, /double, dimension=3)
                      endif else begin
                        ; else -> redpar.master_stellar eq 'median'
                        master_stellar = median(stellar_stack, /double, dimension=3)
                      endelse
                      master_sp[1,*,*] = master_stellar 
                      writefits,  indir, master_sp, hd
                  endif else begin
                    ;4.  Remove CRs  : this is the stack approach
                    remove_cr_by_sigma, all_file_names, combine_stellar, redpar=redpar, master_name=master_name, automation=automation  ; The files themselves get updated. So files in the folder fitspec get updated
                  endelse
                endforeach
            endif
            
            ;---------------------------
            ;Search for master and place in stellar_bary_correc
            ;---------------------------
            ; Search for master file just created
            str_file_type=  redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag+'m'+redpar.prefix_out+'*.fits'
            print,'Master file search ',str_file_type
            post_process_files = file_search(str_file_type, count = count_master) ; look for the data files. Name of the file itself
      
            ;Extract Files information from those found master file  :
            if count_master le 0  then stop, 'reduce_slicer:  STOP : No master files found for post process. Check the files exist within the directory '
            stellar_bary_correc=list()
            print,'count_master is ',count_master
            for index = 0L, count_master-1 do begin ; For the number of master files present.
              file_name =post_process_files[index]
              file_name =file_name.extract("[0-9]{4}_[0-9]+.fits$")
              file_name =file_basename(file_name,'.fits')
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
            bary_indices = where(float(baryCorrec) ne 0.0 and slit eq redpar.modes[1], c_bary)
            if c_bary le 0 then stop, 'reduce_slicer:  STOP : No barycentric corrections were identified from the .log file. '
            obnm = obnm[ bary_indices]
            objnm = objnm[ bary_indices]
            baryCorrec = baryCorrec[bary_indices]
            stellar_bary_correc=list()
      
            for index = 0L, n_elements(obnm)-1 do begin
              stellar_bary_correc.Add, {file_name:redpar.prefix_out+strt(obnm[index])+'.fits' , correction:baryCorrec[index], star_name:objnm[index] } ; Meant to be output
            endfor
            
            ;--------------------------
            ; Clean CRs when runnning individually and remove_crs:5.0 is set.
            ;--------------------------
            if  redpar.remove_crs eq 5.0 then begin  ; before it had ~keyword_set(combine_stellar) and
      
                stellar_exposures = dictionary()
                ;1.  Gather all file names
                foreach structure, stellar_bary_correc do begin
                  file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(structure.file_name,2)
                  refined_key = structure.star_name
                  refined_key = clean_key_dictionary(refined_key)
                  if stellar_exposures.HasKey( refined_key ) then begin
                    stellar_exposures[ refined_key ].Add,  file_name ; Add the obersevation number
                  endif else begin
                    stellar_exposures[ refined_key ]= list(file_name )
                  endelse
                endforeach
                
                ;2.  Remove CRs  : this is the stack approach
                foreach star_name_key,  stellar_exposures.Keys() do begin
                  all_file_names = stellar_exposures[star_name_key].ToArray()
                  print, all_file_names
                  remove_cr_by_sigma, all_file_names, combine_stellar, redpar=redpar, automation=automation ; The files themselves get updated. So files in the folder fitspec get updated
                endforeach 
                
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
              position_artifact= list()
      
              new_cube= splice_spectrum( new_cube, position_artifact=position_artifact, redpar=redpar, /maskArtifact)
              
              print, '......Done removing artifact......'
              foreach order_artifact, position_artifact do begin          
                str_pos_artifact = "[" + strt(order_artifact[1]) + " , " + strt(order_artifact[2]) + "]" 
                artifact_n= 'ARTIFACT_ORDER_'+strt(order_artifact[0])
                sxaddpar, hd, 'HISTORY', str_pos_artifact
                print, str_pos_artifact
              endforeach
              print, '..................................'
            endif
     
            ;#########################
            ; Remove CRs | fft approach, order by order
            ;#########################
            ; SIDE EFFECT: If combine stellar set and fft approach set, this will apply fft to a master file (WARNNING master file could have already been removed CRs)
            if  (redpar.remove_crs eq 4 or automation) then begin ; Previously it was limited for individual only. Meaning I added  ~keyword_set(combine_stellar)
              tempVar= sxpar(hd, "CR_MT")
              cr_already_clean = isa( sxpar(hd, "CR_MT"), /number )
              if cr_already_clean then begin
                  print, ''
                  print, ' Cleaning all CRs by FFT for ' + structure.file_name
                  print, ''
          
                  total_crs=0
                  for order = 0L, n_orders-1 do begin
                    order_crs = 0
                    new_cube[1,*,order] = cr_remove_fft( new_cube[1,*,order] , redpar.sigma_multiplier, redpar.skirt_level, redpar.frac, order_crs) ; reliable as far as there are no CRs
                    ; order_crs gets changed within and we can use its values now
                    total_crs = total_crs +  order_crs
                  endfor
           
                   comment_cr_mt = "CR done by FFT After reduction"
                   comment_num_cr = strt(total_crs)
                   ; The statement "CR-CLEANED" serves as reference to identify if the file has been cleaned previouly. Do not remove.
                   sxaddpar, hd, 'CR_MT', comment_cr_mt
                   sxaddpar, hd, 'NUM_CRs', comment_num_cr
                   print, "Done running FFT to remove CRs. Number of Crs found = " + comment_num_cr
              endif else print, "NOT running FFT, CRs already removed for " +  structure.file_name
            endif
      
            ;##################
            ;# Cut spectra
            ;##################
      
            if (cut_spectra eq 1) then begin
              print, ''
              print, ' Cutting the spectra for night '+strt(night)+'......'
              print, ''
     
              ; pass array along with type of cut
              splice_type= 'custom'
              p_to_cut = redpar.pixel_to_cut
              new_cube= splice_spectrum( new_cube, splice_type=splice_type, pixel_to_cut=p_to_cut)
      
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
              sxaddpar, hd, 'BCV', history_str
      
      
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
    endfor  
  endforeach
 
  ;#####################################################
  ;# 5) Coaddition across nights
  ;#####################################################
  
  if keyword_set(combine_an) then begin
      star_eles= dictionary()
      counter = 0
      print, "****************** All stars Names ******************"
      foreach night, nights do begin
        str_file_type=  redpar.rootdir+ redpar.fitsdir+ strt(night) + "\post_processed\" + redpar.prefix_tag+'mchi'+strt(night)+'*.fits'
        post_process_files = file_search(str_file_type, count = count_master)
        if count_master ne 0 then begin 
            for i=0, count_master-1 do begin 
              post_process_file=post_process_files[i]
              header = headfits(post_process_file)
              star_name = strt(fxpar(header, 'OBJECT'))
              star_name = clean_key_dictionary(star_name)
              print, star_name + " found in " + file_basename(post_process_file, ".fits")
              if ~keyword_set(stars_nm_cb_an) then begin
                  if star_eles.HasKey( star_name ) then begin
                    star_eles[ star_name ].add, post_process_file ; Add the obersevation number
                  endif else begin
                    star_eles[ star_name ]= list(post_process_file)
                  endelse
              endif else if total(stars_nm_cb_an eq star_name) eq 1 then begin
                  if star_eles.HasKey( star_name ) then begin
                  star_eles[ star_name ].add, post_process_file ; Add the obersevation number
                endif else begin
                  star_eles[ star_name ]= list(post_process_file)
                endelse
              endif
            endfor
        endif
      endforeach 
      
      ;Create directory to save files 
      dir_name = redpar.rootdir+redpar.fitsdir+"combine_across_nights"
      file_mkdir, dir_name
      ;If custom stars for combine across nights -> print the name for them
      if keyword_set(stars_nm_cb_an) then begin
        print, "****************** Stars Selected for combine across nights stellar ******************"
        print, star_eles.keys()
      endif
      foreach star_name, star_eles.keys() do begin
          star_list=star_eles[star_name]
          nele = star_list.count()
          if nele gt 1 then begin 
            result = wv_linearization_coaddition(star_list)
            master_name= redpar.prefix_tag + 'mchi_' + star_name + '.fits'
            indir=dir_name + "\" + master_name
            ; Creating new header
            template_hd = headfits(star_list[0])
            ; Extracting master files names used for combine
            names_an = file_basename(star_list.toarray(), ".fits")
            
            ; Deleting unnecessary headers
            sxdelpar, template_hd, 'HISTORY'
            sxdelpar, template_hd, 'NUM_CRS'
            sxdelpar, template_hd, 'CR_MT'
            sxdelpar, template_hd, 'RESOLUTN'
            sxdelpar, template_hd, 'THARFNAM'
            sxdelpar, template_hd, 'VERSIOND'
            sxdelpar, template_hd, 'IMDIR'
            sxdelpar, template_hd, 'THIDNLIN'
            ;Adding information to header
            
            sxaddpar, template_hd, 'HISTORY', "Date created comb-acr-n file: "   + SYSTIME()
            sxaddpar, template_hd, 'HISTORY', "Num masters used for comb-acr-n: "  + strt(nele)
            foreach name, names_an do begin
              sxaddpar, template_hd, 'HISTORY', "File used for comb-acr-n: " + name
            endforeach
             
            ;Saving combined master spectrum
            writefits, indir, result, template_hd
         endif 
      endforeach
  endif 
  
  if keyword_set(automation) then begin
    create_final_report, redpar=redpar, logsheets_paths=logsheets_paths
  endif
  
  print, '**************************************************'
  print, '               End Of Scripts'
  print, '**************************************************'
END