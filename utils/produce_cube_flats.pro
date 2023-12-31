
;+
 ; :Description:
 ;  If finds the master flats (original, smooth, orignal/ smooth ) created for a given night 
 ;  at stores in memory (out_directory) a data cube format of these files. 
 ; : Input:
 ;   Night : Night number e.g. 171218
 ;   
 ;   
 ;   E.g.  produce_cube_flats, 210423
 ;-
pro produce_cube_flats, night, out_directory=out_directory
  compile_opt idl2




spawn, 'cd', pwddir  
case pwddir of    
    'C:\Users\mrstu': ctparfn = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron_procedures\ctio.par'
    'C:\Users\gulliver': ctparfn = 'C:\F disk\ctio.par'
    ELSE : ctparfn=!NULL
    ; E.g. 'your_current_directory': ctparfn = 'absolute_path_to_ctio.par'
    ; Note: Let the program run. It will stop with the message bellow. Copy/Paste the printes direcotory in 'your_current_directory'
endcase

if ctparfn eq !NULL then begin
  print, '******************************************************'
  print, 'You must be running things from a different directory.'
  print, 'Your current working directory is: '
  print, pwddir
  print, 'ctparfn has not set. '
  print, 'Either changed your working directory, or modify the case'
  print, 'statement above this line.'
  print, '******************************************************'
  stop
endif

redpar = readpar(ctparfn) ; Contains all parsed default values

redpar.date = strt(night)

 
if ~keyword_set(out_directory) then  begin
  out_directory =  redpar.rootdir+redpar.flatdir + 'cube_flats\'  
  if ~file_test(out_directory) then spawn, 'mkdir '+ string(34B) + out_directory + string(34B)
endif


;---------------------
;Restoring Flats
;---------------------
rdsk, flats, redpar.rootdir+redpar.flatdir+'chi'+  redpar.date +'.slicer.flat', 1
sz=size(flats)
n_flats=sz[3]

;---------------------------------
;Restoring wavelength Calibration 
;--------------------------------

; Which of all ThAr files to use ?????
; For now find all the match at pick the first one 

thid_files =  redpar.rootdir+redpar.thidfiledir + 'wchi' + redpar.date + '.*.thid'
thid_files =  FILE_SEARCH(thid_files, count=ct)

if ct le 0 then stop, ' No THID solution found. Please check directory : '+thid_file

restore, thid_files[0] ; Restoring the first one to later restore the rest
mkwave, w, thid.wvc

w = reverse(w,2)




; Need to extract the flat, fix it and produce trimmed  version 
; and then create the smooth and the flat by the smooth 


;----------------------------
; Flat 
;----------------------------

spectra = reverse( reverse( reform(flats[*,*,1]), 1), 2 ) ; THIS WOULD HAVE TO CHANGE if the output of the flats files changes as well 
; Altear the data cube to match with expected input for splice_spectrum
sz_flat =size(spectra)
nord =  sz_flat[2]
ncol =  sz_flat[1]
flat_cube=dblarr(2,ncol,nord) ; It will fill with 0.0 and We don't care about (0,*,*)
flat_cube[0,*,*]=w
flat_cube[1,*,*]=spectra
splice_type= 'pixel_cut_of_3200px'
flat_cube= splice_spectrum( flat_cube, splice_type, /maskArtifact)


file_out_path=  out_directory + redpar.date +'_flat.fits'
writefits, file_out_path,flat_cube
print, '  |  Produced : '+file_out_path +'  | '



;----------------------------
; Smooth Flat && Flat / Smooth Flat
;----------------------------

; Now produce the smooth and the smooth divided by the flat versions 

sz=size(flat_cube)
nord =  sz[3]
ncol =  sz[2]

smooht_flat=dblarr(2,ncol,nord) ; It will fill with 0.0 and We don't care about (0,*,*)
by_smooth_flat=dblarr(2,ncol,nord) ; It will fill with 0.0 and We don't care about (0,*,*)




ix=findgen(ncol)
for order = 0L, nord-1 do begin  
  cfs = poly_fit(ix,  flat_cube[1,*,order ] ,6, yfit=yfit) 
  smooht_flat[1,*,order ] = yfit 
  smooht_flat[0,*,order]=reform(flat_cube[0,*,order ] )
 
  
  ; By smooth 
  by_smooth_flat[1,*,order ] = flat_cube[1,*,order ] / yfit   
  by_smooth_flat[0,*,order]      = reform(flat_cube[0,*,order ])
endfor



file_out_path=  out_directory + redpar.date +'_smooth_flat.fits'
writefits, file_out_path,smooht_flat
print, '  |  Produced : '+file_out_path +'  | '

file_out_path=  out_directory + redpar.date +'_flat_by_smooth.fits'
writefits, file_out_path,by_smooth_flat
print, '  |  Produced : '+file_out_path +'  | '









;;-------------------------------
;;Create Data cube for each flat 
;;------------------------------
;
;; Data cube follow = [2, 4112, 73]
;
;identifiers=['smooth','original', 'by_smmooth']
;for idx = 0L, n_flats-1 do begin
;  
;  
;  spectra = reverse( reverse( reform(flats[*,*,idx]), 1), 2 )
;  
;  ; Altear the data cube to match with expected input for splice_spectrum
;  sz_flat =size(spectra)
;  nord =  sz_flat[2]
;  ncol =  sz_flat[1]
;
;  flat_cube=dblarr(2,ncol,nord) ; It will fill with 0.0 and We don't care about (0,*,*)
;  
;  flat_cube[0,*,*]=w
;  flat_cube[1,*,*]=spectra
;   
;  
;  splice_type= 'pixel_cut_of_3200px'
;  flat_cube= splice_spectrum( flat_cube, splice_type, /maskArtifact)
;
;  
;  ; Add to history what is this 
;  file_out_path=  out_directory + redpar.date +'_'+identifiers[idx] + '.fits'
;  writefits, file_out_path,flat_cube
;  print, '  |  Produced : '+file_out_path +'  | '
;
;endfor


print  , ' '
print, ' End of Script : Please check files created '
print  , ' '

end

