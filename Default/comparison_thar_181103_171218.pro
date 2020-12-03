
;Comparing ThAr files for 181103 and 171218




;getting WVC 
wvc_path = "C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\thid\thidfile\sl181103_myWaveSol.sav"

restore, wvc_path

wvc_input=wvc

order=0 



; - - -- -  FILE 1 : extracted spectra  ThAr 171218  are 1003, 1144


path1= "C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\iodspec\171218\achi171218.1003"
rdsk,sp,path1,1   

;rdsk2fits, filename=fitsname, data = flat
;rdsk, x, path1
;cube1=readfits(path1) ; 1432 x 4112
;restore, path1


mkwave2,w_output1,wvc_input,pixel_offset=-3


p1=plot(w_output1[*,order], sp[*,order],  title ='ThAr-171218.1003')


; - - -- -  FILE 2   ThAr for 181103 are 1003,1163 and 1196
path2= "C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\tous\mir7\iodspec\181103\achi181103.1003"

rdsk,sp2,path2,1  



mkwave2,w_output2,wvc_input,pixel_offset=0


p1=plot(w_output2[*,order], sp2[*,order], color='blue', title ='ThAr-181103.1003', /overplot)




End