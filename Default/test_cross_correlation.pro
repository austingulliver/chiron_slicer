

image_data = readfits("C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\210208\chi210208.1040.fits")


scipy_signal= Python.Import('scipy.signal')

section = image_data[ 1000:1050, 5  ]  

min_val =  min(section)
section=float(section-min_val )




my_order = float( image_data[1019:1031, 2000]  - min_val )



corr= scipy_signal.correlate(section, my_order, mode='same' )





;
;
;correlated = c_correlate(section, signal, lag)


p =plot( section,  LAYOUT=[1,2,1] )
p =plot( my_order,  color='blue', /overplot, LAYOUT=[1,2,1] )
p=plot(corr,/CURRENT,  LAYOUT=[1,2,2] )



END