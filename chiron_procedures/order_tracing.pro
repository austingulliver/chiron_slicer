

  
FUNCTION trace_all_orders, img, inital_order_peaks
  ;+
  ; :Description:
  ;    Based on  initial positions which mark the middle of  an order (initial order peaks), it finds the gaussian by left order, find the peaks 
  ;    repeats based on th peak foudn. Same for the right side of an order.
  ;
  ; :Params:
  ;    img  : 2d image of the master flat  containning the orders ot be traced
  ;    inital_order_peaks : Y initial positions found as peaks of all orders in the cross dispersion direction 
  ;
  ;
  ;
  ; :Author: J.Andres Lozano
  ;-

  
  
  ;f1=image(img)
  ;subImg=img[ *, 75:115]
  ;Y =100

  img_size=size(img)
  n_columns=img_size[1]
  n_rows= img_size[2]

  ;order_ys=LONARR(4112)

  order_ys = MAKE_ARRAY(N_ELEMENTS(inital_order_peaks), n_columns, /INTEGER, VALUE = 0) ; (#of Orders, # X Pixels  )
                                                                                        ; Array that gets returned with all orders traced 
  
  FOR i=0, N_ELEMENTS(inital_order_peaks)-1 DO BEGIN  ; Each peaks marks the begging of an order in the img

      Y=long(inital_order_peaks[i])
  
      order_ys[i,n_columns/2]=Y ; For the middle
  
      ; >> Left side of Order
      ;----------------------.
  
      FOR X = n_columns/2, 1,-1  DO BEGIN    ; change to 4111
  
        back_X =X-1      
  
        if (Y le 1) or (Y ge n_rows-1 ) THEN BREAK ;if y=0 THEN WE ARE AT THE VERY EDGE OF THE IMAGE THEREFORE  part of the order is missing
  
        values= [ img[back_X, Y-1],img[back_X, Y ], img[back_X, Y+1] ]
        max_neighbor = MAX(values, idx)
  
        Y = Y-1 +idx
        order_ys[i,back_X]=Y

       ENDFOR

    
        ; >> Right side of Order
        ;-----------------------
        Y=long(inital_order_peaks[i])    
        FOR X=n_columns/2, n_columns-2  DO BEGIN
    
            forward_X =X+1
      
            if (Y le 1) or (Y ge n_rows-1 ) THEN BREAK ;if y=0 THEN WE ARE AT THE VERY EDGE OF THE IMAGE THEREFORE  THE ORDER ENDS AT THIS POINT  BECAUSE IS INCOMPLETE
      
            values= [ img[forward_X, Y-1],img[forward_X, Y ], img[forward_X, Y+1] ]
            max_neighbor = MAX(values, idx)
      
            Y = Y-1 +idx
            order_ys[i,forward_X]=Y
    
        ENDFOR

  ENDFOR


  RETURN, order_ys

END




FUNCTION create_polynomial,img


  PRINT, '***********************************************'
  PRINT, 'NOW IDENTIFYING ORDER LOCATIONS....'
  PRINT, 'CLICK IN THE CENTER OF EACH ORDER.'
  PRINT, "(Y VALUE DOESN'T MATTER)"
  PRINT, '***********************************************'

  ;plotting central swath
  ;left_swath = aswa(*,nswa/4) ;  Do one at the time
  ;right_swath = aswa(*,3*nswa/4)
  ;swa = aswa(*,nswa/2)
  iord=INDGEN(76)
  swa=img[2056,*]

  yy = [0,max(swa)]

  swa= swa

  plot, swa  ; Used as shortcut to increase window size
  stop, 'Make plot BIGGER before continue'
  plot, swa, /xsty, /ysty,  xtitle='Cross Dispersion Direction [Pixel]',   ytitle='Intesity in Central Swath'

  xeye = dblarr(n_elements(iord))
  for eyeord = 0, n_elements(xeye)-1 do begin                     ; Add numbering of Orders as you do it
    cursor, xcur, ycur, /down
    xeye[eyeord] = xcur
    print,'Selected  >>  Indexed Order : '+  string(eyeord) + '     X-coord : '+ string(xcur) + '     Y-coord :  ' +string( ycur)
    oplot, [xcur,xcur], [ycur, ycur], PSYM=7, color=160   ; BEFORE psym =8, color =230
  endfor

  ;loadct, 39, /silent
  pkcoefs = poly_fit(iord, xeye, 3,  yfit = yfit) ; before init=[38., 76., -0.04, -0.003, 0d] , fixed=[0,0,0,0,1],

  p2= plot( iord, xeye, color='black' , title='Actual vs fitted values ')
  p2= plot(  iord, yfit, color='blue',  /overplot)

  ;pkcoefs = res[0:3] * redpar.binning[0] ; ignore binning since we doing this for slicer only
  pk = poly(iord,pkcoefs) ;/redpar.binning[0] ; default peaks in binned pixels, central swath

  p3= plot ( swa, /xsty, /ysty,  xtitle='Cross Dispersion Direction',  ytitle='Counts in Central Swath' )

  for kk=0,n_elements(iord)-1 do p3= plot(  pk[kk]*[1,1], yy, LINESTYLE=2, color='blue',/ overplot)
  PRINT, '***********************************************'
  PRINT, 'IF IT LOOKS GOOD ENTER THESE FOR PKCOEFS IN YOUR .PAR FILE'
  PRINT, 'pkcoefs: [', strt(pkcoefs[0]), ',',  strt(pkcoefs[1]), ',',  strt(pkcoefs[2]), ',',  strt(pkcoefs[3]), ']'
  PRINT, '***********************************************'


  RETURN, xeye
END



;Summary:
;       called from ctio_dord
;       Main method calls  trace_all_orders and create Polynomial
;Input:
;      img:  2-D image of the master flat
;      initial_order_peaks:
;
;Output:


FUNCTION order_tracing, img, redpar

  

  ; Definning Constants
  debug=redpar.debug
  flat= img
  img_size=size(flat)     ;size of image passed: e.g. Master Flat
  n_columns=img_size[1]   ;number of cols in image
  n_rows= img_size[2]     ;number of rows in image  
  nord = redpar.nords ; total number of orders
  iord = findgen(nord) ; Generated Orders in range [0,nord]
  orcdeg = 4.          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome)
  ;BUT  eventually loss of  precision begins increasing the errors again.MAKE SURE RESIDUALS DECREASE.
  mmfrac = 0.05       ;maximum fraction missing peaks allowed. Only Up to 5% of the spectrum can be missing.
  maxome = 10.        ;max allowable mean pixel error in orcs. Previous = 2 i ---------------------NEEDS IMPLEMENTATION 
  pkcoefs =redpar.pkcoefs_slicer   ;' These polynomial will be used later          
  y_peaks = poly(iord,pkcoefs) 


 
  ;Trcaing all Order : Returns 2-D array [iord,n_columns] where values are the Y image index for the corresponding n_column
  traced_orders = trace_all_orders(flat, y_peaks)


  ;Debugging : Plotting all Orders by value found in tracing
  if debug gt 0 then begin
      p1=plot(traced_orders[0,*] )
      for i= 1, nord-1 do begin
          p1=plot(traced_orders[i,*], /overplot)
      endfor    
  endif
 


  orc= dblarr(orcdeg+1,nord) ; Initializing array to store polynomia of each order.
  
  ;Fitting each order
  FOR ior = 0,nord-1 DO BEGIN

    ;Checking if each order has enough points to be fitted
    iwhr = where(traced_orders[ior,*] gt 0,nwhr)   ;find valid peaks, (Invalud peaks were previously set to zero)
    nmiss = n_columns - nwhr              ;number of missing peaks

    if float(nmiss)/n_columns gt mmfrac then begin ; If does not has sufficient peaks to fit
      STOP, 'Indexed Order : ' +string(nord) +' is not complete.    You need to decrease the number of orders extracted in ctio.par or refit the peaks of central pixels using tool in this procedure before continue '
    endif

    x=findgen(n_columns)
    y=traced_orders[ior,*]
    mny = total(y) / n_columns    ;mean row number, y has the 128 (assuming swath size of 32) peak values found for a given order
    y = y - mny                   ;better precision w/ mean=0. Every peak gets divided by the row'x mean

    orc(*,ior) = poly_fit(x,y,orcdeg,fit)       ;returns polynommial coefficientes of a given order
                                                ;fit are the fitted y values found for x
    orc(0,ior) = orc(0,ior) + mny   ;renormalize since mny was reduced before for better precision                        
                                    ;just changing the constant term

  ENDFOR
  

  ;Debugging : Plotting all  FITTED Orders
 DEBUG=0
  if debug gt 0 then begin
      
      ;kernel = [ [0,1,0],[-1,0,1],[0,-1,0] ]

      ;result = CONVOL(img, kernel,  /EDGE_ZERO)
      ;dummy = image(img)
      img2=img
      for ior = 0,nord-1 DO BEGIN
        x=findgen(n_columns)
        calculated_y=poly(x,orc(*,ior))
        
        
        img2[x,calculated_y] = 0d
        ;p1=plot(calculated_y ,color='red',/overplot)
      endfor
      
       dir= redpar.rootdir +redpar.debugging + 'order_tracing_master_flat.fits'
       writefits, dir, img2
      
  endif
 
 




   RETURN, orc
end