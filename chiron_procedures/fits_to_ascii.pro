
;SUMMARY 
; Finds all xchiYYMMDD.xxxx.fits  files within directory and converts every file to a ASCII type file
; The script converts every NaN value to a 0.

;Coded by J.Andres L.
;Last Time Modified 11/19/2020
;need to set your environment variable chirondata

;setenv, "chirondata=C:\Users\mrstu\IDLWorkspace\my_project\given_29_05_2020\Chiron\chirondata"
;date="181103"
;importPath = getenv('chirondata')+ '\'+string(date)

directory_path =   "C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\fitspec\171218"
output_directory = "C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\fitspec\171218\ascii_versions"

cd, directory_path, current=oldDirec

string_match= "achi171218.1145.fits"   ;"xchi"+string(date)+".*.fits"
logFiles=get_file_list(string_match,numFiles) ; Finds all files that match criteria


print, "------- Please Wait ----------"

if numFiles ne 0 then begin
    for i=0, numFiles-1 do begin
      spec = READFITS( logFiles[i], header, /NOSCALE)
  
      name=STRMID(logFiles[i],0,16)  
      print, spec[0,*,1]
      specSize=size(spec)
      
      for j=0, specSize(1)-1 do begin
        specDim= spec[j,*,*]
        specDim[ where( finite(specDim,/NAN) )   ]= 0.0   ; Filter all NAN values
        spec[j,*,*] =specDim
      endfor
      cd, output_directory, CURRENT=old
      free_lun,101
      openw,101,name+"txt"
      printf,101,name+"txt"
      printf,101," Produced by fits_to_ascii.pro  "
      printf,101,"  These are the first 11 Blue Orders  "
      printf,101, "       Wavelength    Intensity    "
  
      writecol, name+"txt" ,spec[0,*,*],spec[1,*,*],fmt='(F18,F12)',filnum=101
  
  
      free_lun,101
      cd,old
    endfor
endif
print," ---------------------------"
print," All Good!  Check Your Files"
print," ---------------------------"

end