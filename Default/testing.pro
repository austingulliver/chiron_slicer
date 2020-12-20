

y= [      17.2598 ,     17.2598 ,     26.9658    ,  17.2598   ,   1139.05  ,    601.086,269.516    ,  1095.47      ,1211.75    ,  1148.29   ,   17.2598  ,    17.2598]

a0=max(y) -min(y)
a1=n_elements(y) /2.0
a2=10.0
a3=min(y)

array= double([a0,a1,a2,a3])

x= indgen(n_elements(y))
;yfit = GAUSSFIT(x, y, coeff, NTERMS=4, ESTIMATES=array)

cfit=  POLY_FIT(x, y, 2,yfit=yfit )
; y = C2 X^2  + C1 X + C0
c2=cfit[2]
c1=cfit[1]
c0=cfit[0]

print, cfit


;find derivitive and equal to 0
center= -c1 /(2.0 *c2)

print, 'cnter'
print, center 

p1=plot(y)
p2=plot(yfit,"r1D-",/overplot)

;print, coeff[1]
end