


; uncomment to prove there the change in 'DC' level is not real.Therefore is the algorithim itself
 
dir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\171218\chi171218.'


stellar_file_nums = indgen(1185-1145) ;1145 - 1184 : 40 files
stellar_file_nums = stellar_file_nums + 1145

file_dirs = dir + strtrim(string(stellar_file_nums),1) + '.fits'


for index = 0L, n_elements(file_dirs)-1 do begin
   img=readfits(file_dirs[index])
   print, size(img)
   
   img =img[1054, 1449:1545] ;  img[1295, 2176:2280]
  
   if (index mod 2L) eq 0L  then color = 'blue' else color='black'
   
   p0 =plot(img,color=color, /overplot)

endfor



;
;dir = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\171218\achi171218.'
;
;
;stellar_file_nums = indgen(1185-1145) ;1145 - 1184 : 40 files
;stellar_file_nums = stellar_file_nums + 1145  
;
;
;file_dirs = dir + strtrim(string(stellar_file_nums),1)  +'.fits'
;
;
;for  index = 0L, n_elements(file_dirs)-1 do begin
;  ;rdsk,data,  file_dirs[index],1
;  data = readfits(file_dirs[index])
;  
;  first_order= data[1,*,0]
;  if (index mod 2L) eq 0L  then color = 'blue' else color='black'
;   p0 =plot(first_order,color=color, /overplot)
;;  
;  
;  
;endfor




END