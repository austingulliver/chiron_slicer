





; manually extracted fro image 
order_peaks=[4,16,28,40,51,64,76,88,101,113,126,138,151,164,177,190,203,217,230,244,258,272,286,300,315,330,344,359,375,390,406,421, $
  437,453,470,486,503,520,537,554,572,590,608,626,644,663,682,701,720,740,760,780,801,821,842,864,885,907,929,952,975,998,1021,1044,1069,1093,$
  1118,1142,1168,1194,1220,1246,1274,1301,1329,1357]

lenPeaks = 76
orders = indgen(76)



Result = POLY_FIT( orders, order_peaks, 3 )
fitted_orders = poly(orders,Result)

print, Result

p2= plot(fitted_orders,'r4D-' ,Title='Polynomial that gets passed' )

old_fitted_y=poly(orders,[26.4117,34.9785,0.114412,0.00182212])/3.
p1= plot(old_fitted_y , color='blue', /overplot)





end