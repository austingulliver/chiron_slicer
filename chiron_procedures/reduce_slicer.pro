

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
;-



PRO reduce_slicer,  $
night, $
no_log=no_log $                       ; If set it wont create a new .log file 
no_reduction =no_reduction $          ; If set it wont reduce the spectra of the given night 
combine_stellar = combine_stellar $   ; If reduction step is run this determines if reduction should consider  individual frames or
                                      ; collect them to produce a master stellar 
post_process = post_process           ; Post processing includes shift found from  barycentric correction +  splice the spectra          
star_name=star_name                   ; Name of the Star




; Some validation 



;by default it creates them all 


;#####################################################
;# 1) Create .log file
;#####################################################

if ~keyword_set(no_log) then begin ; If this is keyword is set then is expected the log sheet to exist already !! 
  
      print, '**************************************************'
      print, 'Creating the .log file for '+strt(night)+'......'
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
      print, 'Spectra Reduction for'+strt(night)+'......'
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
    ;   stellar_bary_correc(Output): is a list wich elements are strucutres with the form {file_name:obs_file[i] , correction:czi }
    if keyword_set(combine_stellar) then begin 
       
       ; Search for master file just created
       str_file_type=  redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag+'m'+redpar.prefix+'*.fits'
       post_process_files = file_search(str_file_type, count = count_master) ; look for the data files. Name of the file itself       
       if count_master ne 1 then stop, 'REDUCE_SLICER: None or more than one master stellar files found within the directory.' 
       
       ; Produce mean barycentric correction
       
       ; overwrite 
       
       
       
      
    
    endif else begin
      str_file_type=  redpar.rootdir+ redpar.fitsdir+ redpar.imdir +redpar.prefix_tag+redpar.prefix+'*.fits' ; without the 'm' of master 
      post_process_files = file_search(str_file_type, count = count_all_files) ; look for the data files. Name of the file itself
      if count_all_files eq 0 then stop, 'REDUCE_SLICER: No individual files found for further processing.'

      
    endelse
      
    
    foreach file_name, post_process_files do begin ; Iterate of each file to post process 
      
      
      

    endforeach

    

    
    
    
    ;##################
    ;# Wavelength Shift
    ;##################
    
    if (do_shift eq 1) and ~keyword_set(no_log) and ~keyword_set(no_bary) then begin
        print, '**************************************************'
        print, 'Shifting wavelengths for night '+strt(night)+'......'
        print, '**************************************************'
        ;   stellar_bary_correc(Output): is a list wich elements are strucutres with the form {file_name:obs_file[i] , correction:czi }
        
        ;>> 1) Restore each file, 2) Shift only wavelength accorfing to Eq. 3)Update HISTORY header and 4) continue to check if cut is needed.
        
        
    endif else  if (do_shift eq 0) then begin
        print "REDUCE_SLICER :  The Spectra has NOT been shifted to compensate for the barycentric correction "
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
    
    endif else print, "REDUCE_SLICER :  The Spectra has NOT been reduced in size "
    
    
    
    
    
    
    
    
endif




;#####################################################
;# 3) Store in directory  post_procesed
;#####################################################


if ~keyword_set(no_bary) and ~keyword_set(no_splice)   then begin
  
  
endif 




END