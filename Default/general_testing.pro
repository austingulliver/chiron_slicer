

file = 'C:\Users\aleja\Desktop\Desktop\Job\Gulliver\Reduction-Pipeline-Software\chiron_reduc_pipeline\chiron\tous\mir7\fitspec\210723\post_processed\wchi210723.1151.fits'

y=readfits(file)


p=plot( y[0,*,0:5],y[1,*,0:5], title=' Comparison pre CR removal - Sigma:2.5 - Night: 210423 -  Order: 6th blue ')


; plot all disctibution for sigma clipping :


;nums=indgen(1170-1159+1) + 1159
;file_names='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\test_delete\no_crs_removed\wchi210526.'+strt(nums)+'.fits'

nums=indgen(1160-1157+1) + 1157
file_names='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210423\test_delete\no_crs_removed\wchi210423.'+strt(nums)+'.fits'


;labels = strt(nums)
;colors= ['green', 'black', 'blue', 'purple', 'brown', 'red', 'blue', 'dodger blue', 'olive', 'black','magenta', 'indigo']

p=plot([0],[0], title=' Comparison pre CR removal - Sigma:2.5 - Night: 210423 -  Order: 6th blue ')
;
;
;counter= 0
;plots = list()
foreach file, file_names do begin
  y=readfits(file)
  p=plot( y[1,*,5], '-:1', name='Extracted Observations' ,/overplot)
;  c= colors[counter]
;  n= labels[counter]
;  p=plot(y[1,*,5] ,color='black', name=n, /overplot)
  ;plots.add, p
  
  ;counter = counter+ 1
endforeach





; Pre procceesed
file_names='C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210423\test_delete\crs_removed_pre\wchi210423.'+strt(nums)+'.fits'



yt= 'Intensity'
xt=' Wavelength'


foreach file, file_names do begin
  y=readfits(file)
  p1=plot( y[1,*,5] , color='blue'  , name='Cleaned Observations (of CR s) ' ,xtitle=xt, ytitle=yt,xtickfont_size=20, ytickfont_size=20, /overplot)
endforeach

p1.title.font_size=20

!null = LEGEND(target=[p,p1], font_size=15)


;


end 


