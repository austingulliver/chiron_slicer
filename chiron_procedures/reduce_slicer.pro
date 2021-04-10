

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
no_bary =no_bary$                     ; If set it wont do the barycentric correction
no_splice= no_splice$                 ; If set it wont splice the spectr





; Some validation 



;by default it creates them all 


;#####################################################
;# 1) Create .log file
;#####################################################

if ~keyword_set(no_log) then begin
  
      print, '**************************************************'
      print, 'Creating the log file for '+strt(night)+'......'
      print, '**************************************************'
    
      logmaker, strt(night), date=strt(night), /nofoc, /override,  prefix='chi', object_name = object_name 
      
      if  object_name eq '' then stop, 'REDUCE_SLICER: >> Error << There are stellar frames found for '+strt(night)
      
      
      ; NEED TO DO VALIDATION FOR OBJECT NAME  !!! TO FIX 
      
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











if ~keyword_set(no_bary) then begin

  ;#####################################################
  ;# 3) Barycentric correction
  ;#####################################################
      print, '**************************************************'
      print, 'Runing Barycentric Correction for'+strt(night)+'......'
      print, '**************************************************'
    
      ;If the procedure sorting_hat is not run then the user need to specify the directory for the barycentric correction
    
      if keyword_set(no_reduction) then begin  ; redpar does not exist so the paths have be hardwired. For our case they already are within qbarylog
        qbarylog,  prefix= 'chi' ,simbadStartName=object_name, log_content=log_content    
      endif else begin
        bary_dir= redpar.rootdir + redpar.logdir + '20' + strt(strmid(strt(night),0,2)  )+'\'+strt(night)+'.log'
        qbarylog, bary_dir,  prefix= 'chi' ,simbadStartName=object_name, log_content=log_content
      endelse
      ; log_content(output): structure with sam size as the number of observations regitered in the .log file 
      ; where temp = {bcvel, filename:'', object:'', cz:0.d0, mjd:0.d0, ha:0.d0, type:''}
      ; We are after for all cz (corrections) and filenmaes that beleng to stellar objects 
      
      stellar_obj_names = reform(log_content.object)      
      stellar_indices=where(stellar_obj_names ne 'iodine' and stellar_obj_names ne 'thar' $
                      and stellar_obj_names ne 'focus' and stellar_obj_names ne 'junk' and stellar_obj_names ne 'dark' $
                      and stellar_obj_names ne 'bias' and stellar_obj_names ne 'quartz' and stellar_obj_names ne 'master_stellar', num_star)

      
      ; recall cz is in [m/s]
      if keyword_set(combine_stellar) then begin
           ; >> Consider only 1 file : master stellar  
           ; mean of all
          
      endif else begin            
            ; >> Consider Files Individually 
            
            if num_star gt 0 then begin
                  foreach stellar_idx, stellar_indices do begin
  
                  endforeach
              
              
            endif else print, "*************** Barycentric Correction was not applied *************** : There is no nough information in bary log file."
          

        
      endelse
      
  
      
      ; 1) Make it return the all the shifts along with the observation numbers in .TO FIX !!!
      ; 2) how to account for master stellar
      
      ; no new file is created rather a the same file is used and a comment about the shift in  header is added 



      ; NEED TO SPECIFY THE FILE THAT ARE GONNA GET CONSIDERED
      ;
      ;#####################################################
      ;# 3) Shift of Spectra
      ;#####################################################

endif








;#####################################################
;# 3) Splice of Spectra
;#####################################################

if ~keyword_set(no_splice) then begin
    ; Restore all files 
  
  
endif

;#####################################################
;# 3) Store in directory  post_procesed
;#####################################################


if ~keyword_set(no_bary) and ~keyword_set(no_splice)   then begin
  
  
endif 




END