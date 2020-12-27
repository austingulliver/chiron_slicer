
;Comparing ThAr files for 181103 and 171218






;getting WVC 
wvc_path = "C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\thid\thidfile\sl181103_myWaveSol.sav"

restore, wvc_path

wvc_input=wvc

order=0 



; - - -- -  FILE 1 : extracted spectra  ThAr 171218  are 1003, 1144


;Directory 2020
dir_20 ='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\171218\achi171218.1003'



rdsk,sp,dir_20,1   


;rdsk2fits, filename=fitsname, data = flat
;rdsk, x, path1
;cube1=readfits(path1) ; 1432 x 4112
;restore, path1


;mkwave2,w_output1,wvc_input, pixel_offset=0; ,pixel_offset=-3


;p1=plot(w_output1,sp [*,order],  title ='ThAr-Comparison nights 171218.1003 - 201121.1180')
p1=plot(sp [*,order],  title ='ThAr-Comparison nights 171218.1003 - 201121.1180')



; - - -- -  FILE 2   ThAr for 181103 are 1003,1163 and 1196
dir_20_2= 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\iodspec\190222\achi190222.1003'

rdsk,sp2,dir_20_2,1  



;mkwave2,w_output2,wvc_input,pixel_offset=0

;p2=plot(w_output2, sp2[*,0],color='blue',linestyle=2,   /overplot)

p2=plot( sp2[*,0],color='blue',linestyle=2,   /overplot)


;p1=plot(w_output2[*,order], sp2[*,order], color='blue', title ='ThAr-181103.1003', /overplot)




End