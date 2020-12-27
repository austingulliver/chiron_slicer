
; Comparing  ThAr from 2020 and 2018

;Directory 2020
dir_20 ='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\201121\achi201121.1003' 


rdsk, cube1, dir_20,hd

print, size(cube1)


dir_20_2= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\201121\achi201121.1180' 
rdsk, cube2, dir_20_2



;p1= plot(cube1[*,*,1])
;p2= plot(cube2[],color='blue', /overplot)



end