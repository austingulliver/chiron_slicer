

;+
; :Description:
;
;  3 main tasks : 
;  1) Creation of .log files using the procedure logmaker.pro
;  2) Processing of the spectra using sorting_hat.pro : by default it will create  a master stellar 
;  3) Barycentric Correction
;  4) Shift of spectra using the barycentric correction
;  5) Order splicing
;  
;  
;  E.g.
;      reduce_slicer, 181103, star_name='HR2943', /post_process
;  
;  
;  History :
;         4/16/2021 Written by Jorge Lozano 
;   
;
;-



PRO reduce_slicer,  $
night, $
no_log=no_log, $                       ; If set it wont create a new .log file 
no_reduction =no_reduction, $          ; If set it wont reduce the spectra of the given night 
combine_stellar = combine_stellar, $   ; If reduction step is run this determines if reduction should consider  individual frames or
                                      ; collect them to produce a master stellar 
post_process = post_process, $        ; Post processing includes shift found from  barycentric correction +  splice the spectra          
star_name=star_name                   ; Name of the Star. IT MUST BE INSERTED AS ONE WORD E.G 'HR2943'  (as opposed to 'HR 2943'  )

constants
common CONSTANTS,autom,automJPL,autokm,cms,ckm,radtosec,pctoAU,$
  yeartosec,yrtos,ltyr,lightyear,pctom,secperday,daytosec,$
  century,precise,ddtor,msun,msung,mearth,mmoon,$
  mmoong,rearth,rearthkm,rsun,rsunkm,Gcgs,G,angstrom

; Some validation 



;by default it creates them all 


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
          logmaker_v2, strt(night), /nofoc, prefix='chi', star_name=star_name, stellar_bary_correc=stellar_bary_correc, /barycorr
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
                   thar_soln='wvc_slicer_171218.sav',redpar =redpar   ; ,/remove_cr thar_soln was added
      ;sorting_hat, night, mode=modes[i], /reduce, /getthid, /iod2fits ,thar_soln='wvc_slicer_171218.sav'
    

      ; 1) make it return the redpar. !!!! TO FIX
      ; add redpar for object name   as objectname
 
  
endif




;#####################################################
;#  Determine name of the files to be considered for either barycentric  and  splice 
;#####################################################


cut_spectra=1
do_shift=1

;To do post processing we expecting to run it along with sorting_hat for now. FIX SO THEY ARE INDEPENDENT IN THE FUTURE 

if keyword_set (post_process) then begin
    print, '**************************************************'
    print, 'Runing Post processing for '+strt(night)+'......'
    print, '**************************************************'
    
    
    ;##################
    ;# Collect files for post-process 
    ;##################
    
    ; Get all bary correction in 1 array
    ; stellar_bary_correc(Output): is a list wich elements are structures with the format {file_name:obs_file[i] , correction:czi }
    if keyword_set(combine_stellar) then begin 
       
       ; Search for master file just created
       str_file_type=  redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag+'m'+redpar.prefix+'*.fits'
       post_process_files = file_search(str_file_type, count = count_master) ; look for the data files. Name of the file itself       
       if count_master ne 1 then stop, 'REDUCE_SLICER: None or more than one master stellar files found within the directory.' 
       
       ; Produce mean barycentric correction
       array_all_bary = list()
       foreach structure, stellar_bary_correc do begin
              array_all_bary.ADD, structurecorrection
       endforeach
       
       bary_mean= array_all_bary.toarray()
       bary_mean=mean(bary_mean)

        
       ; overwrite stellar_bary_correc
       stellar_bary_correc=list()
       stellar_bary_correc.add, {file_name:post_process_files[0] , correction:bary_mean}
       
       
    endif ; Else we keep the stellar_bary_correc create by logmaker_v2

      
      
    for index = 0L, n_elements(stellar_bary_correc)-1 do begin
          


    endfor

        
    foreach structure, stellar_bary_correc do begin ; Iterate of each file to post process 
         
         
         if ~keyword_set(combine_stellar) then begin 
          ; Modifying path of individual files since they have the path of prev rar directory
              file_name= redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag +strtrim(structure.file_name,2)         
         endif else file_name = structure.file_name ; for master stellar we already modfied the name of the file 
         
         ;Read data: this data is the output of sorting_hat
         data_cube = readfits(file_name, hd) ; recall data come in the form [2,4112,74] where [0,*,*] -> wavelength          
         
         
         
         

        
        ;##################
        ;# Wavelength Shift
        ;##################
        
        if (do_shift eq 1) and ~keyword_set(no_log) and ~keyword_set(no_bary) then begin
            print, '**************************************************'
            print, 'Shifting wavelengths for night '+strt(night)+'......'
            print, '**************************************************'
            
            data_cube[0,*,*]=data_cube[0,*,*]*(1+ ( structure.correction / cms )  )
            ;recall radial velocity is in [m/s] . cms is brought from different procedure. CMS is the speed of light in[m/s]
            new_cube= data_cube
            ;Make remark in header
            history_str = ' Wavelength (bary)corrected by a factor of : '+ strt(structure.correction)
            sxaddpar, hd, 'HISTORY', history_str
            
            
        endif else  if (do_shift eq 0) then begin
            print, "REDUCE_SLICER :  The Spectra has NOT been shifted to compensate for the barycentric correction "
        endif else begin         
            print, '*REDUCE_SLICER : The keyword post_process was set but the no_bary and no_log were also set. '
            stop, ' Please remove the keyword no_log and no_bary to continue. '
        endelse
    
        ;##################
        ;# Cut spectra
        ;##################
        
        if (cut_spectra eq 1) then begin 
          print, '**************************************************'
          print, ' Cutting the spectra for night '+strt(night)+'......'
          print, '**************************************************'
          
          ; pass array along with type of cut 
          splice_type= 'pixel_cut_of_3200px'
          new_cube= splice_spectrum( data_cube, splice_type)
          
          history_str = ' The orders were spliced using '+ splice_type
          sxaddpar, hd, 'HISTORY', history_str
           
        endif else print, "REDUCE_SLICER :  The Spectra has NOT been reduced in size "
    
    
       
        
        ;#####################################################
        ;# 3) Store in directory  post_procesed
        ;#####################################################

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