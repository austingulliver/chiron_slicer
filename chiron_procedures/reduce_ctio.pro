pro reduce_ctio,  redpar, mode, flatset=flatset, thar=thar, $
  order_ind=order_ind,  star=star, date=date, combine_stellar=combine_stellar, mstr_stellar_names= mstr_stellar_names

  ; Batch job to drive raw reduction for CTIO
  ; Based on 12-Mar-2011 DF revised for the e2v-4k chip
  ; Oct 15, 2011 AT
  ; revised 20120405 ~MJG

  ; redpar = readpar('file.par') contains imdir, prefix, and other directories
  ; mode:  one of: narrow, slicer, slit, fiber
  ; flatset = [indices]
  ; thar = [thar1, iod1, iod2] calibration files
  ; order_ind =  order definition file, set -1 to use summed flat instead
  ; star = [indices] ; stellar spectra to reduce
  ; date: yymmdd (used to create directories)



  ;>> Define the number of pixels used in the cross dispersion direction for a given order.
  xwid = redpar.xwids[redpar.mode] -  redpar.pixel_not_extracted ; Set up this parameter in ctio.par

  ;2. Prefix added to FITS headers:
  prefix=redpar.prefix   ; e.g. 'chi111003'
  if strpos(prefix,'.') lt 0 then prefix=prefix+'.' ; add the point

  ; First check if all relevant input parameters are specified

  if ~keyword_set (order_ind) then begin
    print, 'REDUCE_CTIO: ORDER definition is not given as input. Using Master Flat to trace the orders'    ; is it using the master flat to trace the orders?
    order_ind = -1
  endif

  ; Identify the mode
  modeidx = redpar.mode

  ;1. CCD-Image Input Directory Path, where raw images are stored.
  indir=redpar.rootdir+redpar.rawdir+redpar.imdir ;e.g. /mir7/raw/090102/

  ;6. OUTPUT Directory Path and Output Prefix (ie, tapename for run)
  outdir= redpar.rootdir + redpar.iodspecdir + date + '/'
  if ~file_test(outdir) then spawn, 'mkdir '+outdir



  ; Try to read from the disk previously saved flats
  if ~keyword_set (flatset) then begin
    name = redpar.rootdir+redpar.flatdir+prefix+mode+'.sum'
    tmp = file_search(name, count=flatcount)
    if flatcount eq 0 then begin
      print, 'REDUCE_CTIO: FLATS are not given at input, not found on disk, returning.'
      return
    endif else begin
      print, 'REDUCE_CTIO: reading previously saved flat from disk'
      rdsk, sum, name, 1 ; restore saved flat
      flatfnums='SUM'
    endelse
  endif else begin ; flats are given
    nrecf = n_elements(flatset)
    recnums = strtrim(string(flatset,format='(I4.4)'),2)  ;convert to strings
    flatfnums = prefix + recnums
    flatfnames = indir + prefix + recnums + '.fits'  ;array of flat-field file names
  endelse

  ;7.  Record number of Stellar spectra here:
  nrec = n_elements(star)
  recnums = strt(star, f='(I4.4)')  ; convert to string with leading zeros
  spnums = prefix + recnums
  spfnames = indir + prefix + recnums +'.fits' ; string array of spectrum(STARS) file names
  outprefix = redpar.prefix_tag +  prefix
  outfnames= outdir + outprefix  + recnums



  ; Order-Finding Exposure: strong exposure, such as iodine or bright star(B star)
  if order_ind ge 0 then begin ; NOTour case coming from sorting_hat (guess: We will do the order tracing wrt to the master flat)
    recint = order_ind
    recnums = strtrim(string(recint,format='(I4.4)'),2) ;convert to strings with leading zeros
    ordfname = indir + prefix + recnums + '.fits'
  endif else ordframe='FLAT'

  ;THORIUMS:  Insert record numbers to reduce  here:
  ;3. Record numbers for thar and iodine (don-t need sky subtraction)
  if keyword_set(thar) then threc = thar else threc = -1
  if threc[0] ge 0 then begin
    thnrec = n_elements(threc)
    threcnums = strtrim(string(threc,format='(I4.4)'),2) ;convert to strings
    thspfnames = indir + prefix + threcnums + '.fits'  ;string array of (ThAR) file names
    thoutfnames = outdir + outprefix  + threcnums
  endif else threcnums = 'none'

  print,''
  print,'    ****ECHOING PARAMETER VALUES FROM REDUCE_CTIO****'
  print,'If values are incorrect stop the program'
  print,' '
  print,'SPECTRA:'
  print,spnums
  print,' '
  print,'FLATS:'
  print,flatfnums
  print,' '
  print,'DEFAULT ORDER FILE:'
  print, order_ind
  print,' '
  print, 'THORIUM/IODINE: '
  print, prefix + threcnums
  print,' '

  if redpar.debug ge 1 then stop, 'REDUCE_CTIO: press .C to continue'
  PRINT, ''
  PRINT, ''
  print, " REDUCE-CTIO :           >>> Creating MasterFlat <<<   "
  PRINT, ''
  PRINT, ''

  ;##################################################
  ;##### Create / Restore Master Flat ###############
  ;##################################################
  name = redpar.rootdir+redpar.flatdir+prefix+mode+'.sum'

  if keyword_set(flatset) then begin
    ;if redpar.debug then stop, 'REDUCE_CTIO: debug stop before flats, .c to continue'
    ADDFLAT, flatfnames,sum, redpar, im_arr, masterFlatName=name  ; crunch the flats (if redpar.flatnorm=0 then sum = wtd mean)               Creation of MASTER FLAT
    ; output : sum [#pixelsX, #pixelsY] , it returns bias substracted master flat image
    if (size(sum))[0] lt 2 then stop,  '>> ERROR << : Master Flat was not produced' ; no data!
    
  endif else begin

    check_if_exist = FILE_TEST(name)
    if check_if_exist eq 0 then stop, 'ERROR: You set the parameter flat_from_scrath=0 but there is no previous master flat for this nigth. Change flat_from_scrath=1 in the CTIO.PAR file  and run again'
    rdsk, sum, name, 1  ; get existing flat from disk
    bin = redpar.binnings[modeidx] ; set correct binning for order definition
    redpar.binning = [fix(strmid(bin,0,1)), fix(strmid(bin,2,1))]
    print, 'REDUCE_CTIO: Master Flat read from disk: '+name
    print, 'REDUCE_CTIO: Restored Master flat has a binning of ', redpar.binning
  endelse

  PRINT, ''
  PRINT, ''
  print, " REDUCE-CTIO :           >>> Order Tracing <<<   "



  ;##################################################
  ;##############  Trace the Orders #################
  ;##################################################

  ;SLICERFLAT=1 means use narrow slit to define slicer order locations
  if redpar.slicerflat eq 0 or mode ne 'slicer' then begin

      order_file_save=''
      trace_orders = 1
      traces_dir= redpar.rootdir+redpar.orderdir
        
      if (redpar.use_prev_tracing eq 1 or redpar.automation eq 1) then begin 
        mask = traces_dir + '*'+mode +'.orc'
        all_files = file_search(mask, count = count)
        if count ge 1 then begin
            nights_nums_str    = all_files.substring( strlen(traces_dir)+3, strlen(traces_dir)+8  )
            nights_nums        = double(nights_nums_str)
            night_differences  = abs( double(redpar.date) - nights_nums) ; sorted in ascending from lowest to highest (we want the lowest nights since these are the closest )
            night_diff_indices = sort(night_differences)
            night_differences  = night_differences[night_diff_indices]
            nights_nums        = round(nights_nums[night_diff_indices])
            
            if redpar.automation eq 1 then begin
              if night_differences[0] eq 0.00 then begin
                name_or_file = prefix.substring( 0,2) + strt(nights_nums[0]) +'.'+mode +'.orc'
                modify_date = file_modtime(filepath(name_or_file,  ROOT_DIR=traces_dir))
                curr_time = systime(/SECONDS)
                time_diffe = abs(modify_date-curr_time)
                if time_diffe le 86400 then begin
                  order_file_save = traces_dir + name_or_file
                endif
              endif              
            endif else begin
              order_file_save=redpar.rootdir+redpar.orderdir+  prefix.substring( 0,2) + strt(nights_nums[0]) +'.'+mode +'.orc'
            endelse
           
           if order_file_save ne '' then begin
             rdsk, orc, order_file_save, 1
             orc_sz = size(orc)
             if orc_sz[1] ge redpar.nords and orc_sz[2] ge 4112 then  trace_orders=0 ; this will stop iterating and the continue with the
           endif
        endif
      endif
      
      if trace_orders then begin
        print, 'REDUCE_CTIO : Creating a -->NEW<-- Order tracing from scratch'
        if order_ind ge 0 then begin ; If a default order file was passed as param
          ctio_dord, ordfname, redpar, orc, ome
        endif else ctio_dord, ordfname, redpar, orc, ome, image=sum  ; this is our case(no order definition passed as param)
        ;sum : crunched flat passed as param
        ;ordfname is passed empty and instead image param is passed
        ; orc :(output)(array (# coeffs , # orders))] coefficients from the polynomial fits to the order peaks
        ;  EXCEPT ! for the slicer mode. If the slicer mode is currently running then  the vairable orc  will
        ; contain a 2-d array e.g. array with the  middles of all the orders for all the columns (4112 ) E.g.  74 x 4112

        order_trace_file = redpar.rootdir+redpar.orderdir+prefix+mode+'.orc'
        wdsk, orc, order_trace_file, /new
        print, 'REDUCE_CTIO: The new Order Coefficients are stored in : '
        print, order_trace_file
      endif   
      
  endif else begin
      if (redpar.use_prev_tracing eq 1 ) then begin
        if order_ind ge 0 then begin ; If a default order file was passed as param
          ctio_dord, ordfname, redpar, orc, ome
        endif else ctio_dord, ordfname, redpar, orc, ome, image=sum  ; this is our case(no order definition passed as param)
        ;sum : crunched flat passed as param
        ;ordfname is passed empty and instead iamge param is passed
        ; orc :(output)(array (# coeffs , # orders))] coefficients from the polynomial fits to the order peaks
        ;  EXCEPT ! for the slicer mode. If the slicer mode is currently running then  the vairable orc  will
        ; contain a 2-d array e.g. array with the  middles of all the orders for all the columns (4112 ) E.g.  74 x 4112
        ;name = redpar.rootdir+redpar.orderdir+prefix+mode+'.orc'
        ;wdsk, orc, name, /new
   
      endif else begin
        print, 'REDUCE_CTIO: Order tracing got restored from :'
        name = redpar.rootdir+redpar.orderdir+prefix+'narrow.orc'
        print, name
        rdsk, orc, name, 1
        orc[0,*] += redpar.dpks[modeidx]
        ;now subtract 2 outer orders since the slicer is larger than the slits:
        redpar.nords -= 2
        orc = orc[*,1:(redpar.nords - 1)]
      endelse
  endelse

  ;##################################################
  ;##############  Get Flat Field   #################
  ;##################################################
  ;  Modifed to account for "flatting" taking place before and after  extraction

  if (redpar.flatnorm eq 0) then begin
    ff=1.0 ; Dividing stellar img by 1 will make no difference
    print, 'REDUCE_CTIO: The Spectrum is not  flattend '

    ; For now even though we are not using it. We want ot create teh extracted version of the flat
    ; fo further analtsis
    ; ;--------------------------------------------------------------------------------------------
    ;if redpar.debug then stop, 'REDUCE_CTIO: debug stop before getting flat'

    flat = getflat(sum, orc, xwid, redpar, im_arr=im_arr) ; Master Flat gets input into this method
    ;sum (input) Master Flat found as as a MEAN or MEDIAN of im_arr
    ;im_arr (input) images used to create Master Flat
    ;flat (output)  Cube-form: extracted spectrum from master flat where  [*,*,0] : Flat Spectrum /Smoothed
    ;                                                                     [*,*,1] : Flat Spectrum
    ;                                                                     [*,*,2] : Smoothed Spectrum
    ;xwid (input) Order width: For e.g. 12 for slicer



    name =  redpar.rootdir+redpar.flatdir+prefix+mode+'.flat'
    ;    fitsname = redpar.rootdir+redpar.flatdir+prefix+mode+'.flat.fits'



    wdsk, flat, name, /new



  endif else if (redpar.flatnorm eq 1) or (redpar.flatnorm eq 3) then begin
    ; Spectrum gets flattend AFTER extraction



    ;if redpar.debug then stop, 'REDUCE_CTIO: debug stop before getting flat'

    flat = getflat(sum, orc, xwid, redpar, im_arr=im_arr) ; Master Flat gets input into this method
    ;sum (input) Master Flat found as as a MEAN or MEDIAN of im_arr
    ;im_arr (input) images used to create Master Flat
    ;flat (output)  Cube-form: extracted spectrum from master flat where  [*,*,0] : Flat Spectrum /Smoothed
    ;                                                                     [*,*,1] : Flat Spectrum
    ;                                                                     [*,*,2] : Smoothed Spectrum
    ;xwid (input) Order width: For e.g. 12 for slicer



    name = string(34B) + redpar.rootdir+redpar.flatdir+prefix+mode+'.flat' +string(34B)
    ;    fitsname = redpar.rootdir+redpar.flatdir+prefix+mode+'.flat.fits'



    wdsk, flat, name, /new
    ;    writefits, fitsname, flat ; Saves the same file but as the fits version


    print, 'REDUCE_CTIO: The stellar spectrum is flattend, AFTER extraction.'
    print, 'REDUCE_CTIO: a Cube-extracted Master Flat (Flat/Smoothed, Flat, Smoothed ) was  stored as '+fitsname
    ff = flat[*,*,1] ; the actual flat

  endif else if (redpar.flatnorm eq 2) or (redpar.flatnorm eq 4)  then begin
    ; Spectrum gets flattend BEFORE extraction. By the time we get to the  stellar image we divide it by the flat before we extract
    ; the image produced by dividing the stellar by the flat


    ff=sum ;

    print, 'REDUCE_CTIO: The stellar images are beign divided by the flat, BEFORE extraction.'



  endif else stop, 'ERROR:  The parameter flatnorm (in the ctio.par file )  must be number from 0 to 4'








  if redpar.debug ge 2 then stop, 'Debug stop after flat field, .c to continue'



  ;##################################################
  ;##############  Reduce ThAr      #################
  ;##################################################

  if keyword_set(thar) then begin
    numthar=n_elements(threc)
    FOR i=0,numthar-1 do begin
      redpar.seqnum = strt(threcnums[i])
      CTIO_SPEC,prefix,thspfnames[i], thoutfnames[i],redpar, orc,xwid=xwid , cosmics=0 , /thar
      ; prefix(input) :
      ; thspfnames[i] (input)
    ENDFOR
    CATCH, /CANCEL ; clear the catch block in case we bailed on the last one
  endif







  ;##################################################
  ;##############  Reduce Stellar    ################
  ;##################################################

  if redpar.debug gt 1 then STOP
  ;*************************************  Reduce Stellar Spectrum
  

  if keyword_set(combine_stellar)  then begin

    ; Iterate over the created dictionary. Key-> star name and Value-> List of observation numbers
    ; It will create the same number of master stellars as entries in the dictionary
    mstr_stellar_names=list() ; name of the created master stellars, return for future reference
    name_idx=0
    foreach star_name_key,  combine_stellar.Keys() do begin

      ; All obervation numbers for the current night
      recnums = strt(combine_stellar[star_name_key].ToArray(), f='(I4.4)')  ; convert to string with leading zeros
      spfnames = indir + prefix + recnums +'.fits' ; string array of spectrum(STARS) file names

      nrec = n_elements(spfnames)
      print, 'REDUCE_CTIO: * Please wait... ( Creating Master Stellar )'
      ; >> find dimenesions of img
      im_ref = double(readfits( spfnames[0]))
      sz = size(im_ref)
      n_col = sz[1]          ;# columns in image
      n_row = sz[2]          ;# rows in image
      data_cube =  dblarr(n_col,n_row, nrec)


      avgs =list() ; average of each image

      FOR i=0,nrec-1 do begin ; Iterate over every Stellar image
        data = double(readfits( spfnames[i], hd))
        data_cube[*,*,i] = data
        avgs.add,  mean( data_cube[*,*,i], /double)
      ENDFOR

      nsize = size(data_cube)
      combined_files= nsize[3]
      combined_files = strtrim(string(combined_files), 1)

      ; ##########################################
      ; Extra step to scale iamges before operation
      ; ##########################################
      ; At this point I have ther master flat as a cube
      avgs = avgs.toarray()
      common_avg  = mean(avgs, /double) ; used as a master average. This is the common level for all iamges to scale to



      ;>>Scaling image per image
      for index = 0L,  nrec -1 do begin
        img           = data_cube[*,*,index]
        goldenFactor  =   mean(img, /double) /common_avg
        img= img*goldenFactor
        data_cube[*,*,index]   = img
      endfor




      ;Calculating MASTER (mean/median) stellar :
      ;---------------------------
      master_stellar= weighted_master_frame(data_cube, redpar.master_stellar )

      ;  Naming convention for the master stellar is
      ;  wmchiYYMMDD_XXX_LLL.fits   where the extra m stands for master
      ;  XXX corresponds to the observation number of the first exposure used to create  the master
      ; stellar and LLL is the number of exposures used to create the master stellar.

      mstr_stellar_names.Add, 'm'+prefix +recnums[0]+'_'+strtrim(string(n_elements(recnums)),2)
      fname_master_stellar = indir + mstr_stellar_names[name_idx]+'.fits'    ; Saved in corresponding raw directory
      out_mast_stellar= outdir + redpar.prefix_tag+mstr_stellar_names[name_idx]

      ; >> Creating ranges of file used to append to Header
      range_files= make_range_from_vector(star)

      history_str = 'MASTER STELLAR frame made of ' + strtrim(string(combined_files),2)+ ' files: '+range_files
      sxaddpar, hd, 'HISTORY', history_str
      writefits,  fname_master_stellar, master_stellar, hd
      CTIO_SPEC,prefix,fname_master_stellar,out_mast_stellar,redpar, orc, xwid=xwid, flat=ff
      name_idx=name_idx+1
    endforeach
    
    ;      stop, '  CHECK OF EXTRACTED MASTER STELLAR '
  endif else begin
    ;---------------------------
    ; Reducing Individually
    ;---------------------------
    FOR i=0,nrec-1 do begin
      redpar.seqnum = recnums[i]
      CTIO_SPEC,prefix,spfnames[i],outfnames[i],redpar, orc, xwid=xwid, flat=ff ;put it back to remove CR /cosmics
    ENDFOR

  endelse
end
