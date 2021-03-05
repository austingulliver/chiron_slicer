
 ;; TO FIX : MAKE SURE POSITIONS PASSED ARE U VALUES NOT ROUNDED 
 
 
 
 FUNCTION valdate_relative_mid_pixel, y_to_compare,absolute_order_center
 
   comparison_dif = abs(y_to_compare - absolute_order_center)
   allowed_pixels = 4
   if comparison_dif gt allowed_pixels then  return,  double(y_to_compare)  else return, absolute_order_center
 
 END  
 
 PRO  find_next_y_peak,  x, y, img , order_width, order_number=ord_number,out_in_y_to_compare = out_in_y_to_compare , out_absolute_order_center, only_nearest_max ; y is y without rounding
   ;+
   ; :Description:
   ;     We have to process been calculated in parallel. Nearest max and using gaussian 
   ;    
   ;
   ; :Params:
   ;    img  : 2d image of the master flat  containning the orders ot be traced
   ;    y : Y initial POSITION  found as peaks of all orders in the cross dispersion direction
   ;    x : X Position 
   ;    order_width :
   ;    
   ; :Author: J.Andres Lozano
   ;-
       
       
   
   ; This is only for comparison : nearest  maximum out of the next 3 pixels
   ; ---------------------------
   values_nearest= [ img[x, out_in_y_to_compare-1],img[x, out_in_y_to_compare ], img[x, out_in_y_to_compare+1] ]
   max_neighbor = MAX(values_nearest, idx)

   out_in_y_to_compare= out_in_y_to_compare-1 +idx ; This is the absolute Y 
   
   
   
   
   ;Actual Algorithim 
   ; ------------------
   
   if only_nearest_max eq 0 then begin
    
  
       values  = MAKE_ARRAY(order_width)  
       middle_index =  y    
                ; Only value that gets rounding E.g. 7.5->8  7.4->7       

      
       if (order_width mod 2 eq  0) then begin ; If orderwidth is even : . Case for the slice mode
             
             ; >> Since even number of pixels then we need to find which values discard ;
             end_bottom =  img [ x , round(y - order_width/2)]
             sz=size(img)
             if round(y + order_width/2)  gt sz[2]-1 then used_y= sz[2]-1 else  used_y= round(y + order_width/2)
             end_top = img [x , used_y ]
             ; We keep the one wiht the highest value
          
             if (end_top ge end_bottom) then keep_top = 1  else  keep_top = 0
          
             ; meaning from middle you take (width/2) above and (width/2) - 1 bellow
          
             for i=0, order_width-1  do begin          
                 if keep_top eq 1 then begin
                             img_y_index = round( i + middle_index - (order_width/2) - 1)  ; width/2 is int cause this is even case. We round just in case
                 endif else  img_y_index = round( i + middle_index - (order_width/2) )     ; keep the bottom
                 
                 img_y_index = img_y_index  < sz[2]-1
                 values[i]=  img (x,img_y_index)
             endfor
             
       endif else begin ; when is odd : then just get same number of pixel top and bottom          
             keep_top = -1 ; to identify this case later on 
             pixels_num_extract= float( floor(order_width/2) )          
             for i=0 ,  order_width-1  do begin
               img_y_index = round( i + middle_index - pixels_num_extract )
               values[i]=  img (x,img_y_index)
             endfor          
       endelse
       ; Either way I end up with populated values to be used in the gaussian
      
      
      ; 1 st try 
      ;-----------------
       relative_pixels = indgen(n_elements(values ) ); starts from 0
       y_intensities = values
       
       ;Estimations 
       min_y= min(y_intensities) 
       a0=max(y_intensities)- min_y           ; hegiht of gaussian 
       a1=n_elements(y_intensities) /2.0      ; center of gaussain 
       a2=1.0
       a3=min_y                               ; the constant term
        
       guess = double([a0,a1,a2,a3])
       
       y_fit= gaussfit(relative_pixels, y_intensities, out_coefficients, NTERMS=4, ESTIMATES=guess )
       ;A1 is the center of the Gaussian, 
      
       ;plots debugging
       relative_order_center = out_coefficients[1]  ; RELATIVE x 
       
       
       
        
       
       if (relative_order_center ge order_width) or (relative_order_center < 1)  then  begin ; Gaussian fit not possible 3 peak is higher than the middle peak and algorithim gets confused
            
            
            ; Check if they align with artifact
            ; --------------------------------
            artifact_x_lower =3385
            artifact_x_upper =3423
            if (ord_number ge 60 )and( (x  ge  artifact_x_lower ) and (x  le  artifact_x_upper ) )  then begin
              ; These are hard coded values for the artifact
              ; We return the same Y as the previous 
              out_absolute_order_center= y  
              GOTO, skip_cause_artifact            
              
            endif
            
            ;print, '* ORDER_TRACING: Gaussian failed is returning a relative  value of : ' + string(relative_order_center)
            
            
            ; 2 nd try
            ;-----------------
            ;Trying to force a gaussia-like shape by setting edges to equal min value 
            y_intensities[-1]=min_y
            ;y_intensities[-2]=min_y
            y_intensities[0]=min_y
            ;y_intensities[1]=min_y
            
            
            ; now try again 
            
            y_fit= gaussfit(relative_pixels, y_intensities, out_coefficients, NTERMS=4, ESTIMATES=guess )
            
            values_used= y_fit
            relative_order_center = out_coefficients[1]  ; RELATIVE x 
            
            
            
            if (relative_order_center ge order_width)  or (relative_order_center le 1)  then  begin
               
               ; 3th  try
              ;-----------------
               ;If it still fails then  we try with 2nd order polynomial rather than gaussian 
               cfit= POLY_FIT(relative_pixels, y_intensities, 2, yfit=y_fit ) ; 2nd order polymial
               
               ; y = C2 X^2  + C1 X + C0
               c2=cfit[2]
               c1=cfit[1]
               c0=cfit[0]
               
               ;find derivitive and equal to 0
               relative_order_center= -c1 /(2.0 *c2)
               
               
               if (relative_order_center ge order_width)  or (relative_order_center le 1) then begin
                 print, 'It failed after changing edges values  + fitting 2nd order polynomial . Values used are : '
                 print,y_intensities

                 ;STOP, '* ORDER_TRACING: EVERYHTING FAILED  is returning a relative  value of : ' + string(relative_order_center)
                 
                 relative_order_center = order_width/2.0
                 ;p1= plot(relative_pixels, y_intensities)
                 ;p2= plot(relative_pixels, y_fit ,"r1D-" ,/overplot)              
                
                
                
               endif
              
              
            endif
            
           
           
       endif
       
      
       if keep_top eq -1 then  absolute_order_center = float( middle_index - pixels_num_extract +  relative_order_center  )      
       if keep_top eq 1 then  absolute_order_center = float( middle_index -  (float(order_width)/2.0) - 1.0 +  relative_order_center   )    
       if keep_top eq 0 then  absolute_order_center = float( middle_index -  (float(order_width)/2.0)  +  relative_order_center    )    
       
       
       ; This is the last check. We know the nearest mas technique works 
       out_absolute_order_center =valdate_relative_mid_pixel(out_in_y_to_compare,absolute_order_center )
       ;out_absolute_order_center =absolute_order_center
       
       ;p1= plot(relative_pixels, y_intensities)
       ;p2= plot(relative_pixels, y_fit ,"r1D-" ,/overplot)
       ;print, 'peak found at : '+string(relative_order_center )
       
       ;stop, 'Seee how gaussian behaves . plt of actual values gaussian and peak '
       
 ;  return, absolute_order_center   ; It returns absolute Y postion 
      
      skip_cause_artifact:  
     
    endif ;End not nearest max neighbor
END
 
 
  
FUNCTION trace_all_orders, img, inital_order_peaks,redpar=redpar, only_nearest_max=only_nearest_max
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

  order_ys = MAKE_ARRAY(N_ELEMENTS(inital_order_peaks), n_columns, /FLOAT, VALUE = 0) ; (#of Orders, # X Pixels  )
  
                                                                                        ; Array that gets returned with all orders traced 
  mode_index=redpar.mode
  order_width =redpar.xwids[mode_index]                                                 ; This is not for extraction jus for tracing 
 
  
  FOR i=0, N_ELEMENTS(inital_order_peaks)-1 DO BEGIN  ; Each peak marks the begging of an order in the img. For a given order 
      
      print, ' - - - --  -  ORDER : '+STRING(i)
      Y=inital_order_peaks[i]
  
      order_ys[i,round(n_columns/2)]=Y ; For the middle
      
     
      ; >> Left side of Order
      ;----------------------.
  
       FOR X = round(n_columns/2), 1,-1  DO BEGIN    ; change to 4111
    
          back_X =X-1      
    
          if (Y le 1) or (Y ge n_rows-1 ) THEN BREAK ;if y=0 THEN WE ARE AT THE VERY EDGE OF THE IMAGE THEREFORE  part of the order is missing
    
          ; find values to be used in gaussian 
          
          if  (X eq round(n_columns/2) ) then compare_Y = round(Y); just to state the first value
           
          find_next_y_peak, back_X, Y, img , order_width,order_number=i,out_in_y_to_compare = compare_Y,  out_absolute_order_center, only_nearest_max ; The y for back_X. Ouput : absolute Y position  
           ;output compare_Y and Y
               
          
                  
          ;values= [ img[back_X, Y-1],img[back_X, Y ], img[back_X, Y+1] ]          
          ;max_neighbor = MAX(values, idx)    
          ;Y = Y-1 +idx
          
          if only_nearest_max eq 1 then  Y=compare_Y else Y=out_absolute_order_center
          order_ys[i,back_X]=Y
  
       ENDFOR

    
        ; >> Right side of Order
        ;-----------------------
        Y=inital_order_peaks[i]   
        FOR X=n_columns/2, n_columns-2  DO BEGIN
    
            forward_X =X+1
      
            if (Y le 1) or (Y ge n_rows-1 ) THEN BREAK ;if y=0 THEN WE ARE AT THE VERY EDGE OF THE IMAGE THEREFORE  THE ORDER ENDS AT THIS POINT  BECAUSE IS INCOMPLETE
      
            ;values= [ img[forward_X, Y-1],img[forward_X, Y ], img[forward_X, Y+1] ]
            ;max_neighbor = MAX(values, idx)
      
            ;Y = Y-1 +idx
            if  (X eq round(n_columns/2) )  then compare_Y = Y; just to state the first value    
             
            
            find_next_y_peak, forward_X, Y, img , order_width, order_number=i,out_in_y_to_compare = compare_Y,  out_absolute_order_center, only_nearest_max 
            
            if only_nearest_max eq 1 then  Y=compare_Y else Y=out_absolute_order_center
              
                
            
            order_ys[i,forward_X]=Y
    
        ENDFOR

  ENDFOR


  RETURN, order_ys ; Peaks (are NON INTEGERS found by gaussiain  ) values returned 

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
  iord=INDGEN(74)
  swa=img[2056,*]

  yy = [0,max(swa)]

  swa= swa

  plot, swa  ; Used as hack to increase window size
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

  
  
  stop, " *End of finding the new Polynomial. Copy/Paste polynomial, delete reference to this script and run again. "
  RETURN, xeye
END





;Summary:
;      ** Main Function **
;       called from ctio_dord
;       Main method calls  trace_all_orders and create Polynomial
;       Only optimized for slicer but can be applied to other modes with minor adjustments 
;Input:
;      img:  2-D image of the master flat
;      initial_order_peaks:
;
;Output:


FUNCTION order_tracing, img, redpar

  

  ;------------------------------
  ; >> Constants
  ;------------------------------
  debug=redpar.debug;redpar.debug
  flat= img
  img_size=size(flat)     ;size of image passed: e.g. Master Flat
  n_columns=img_size[1]   ;number of cols in image
  n_rows= img_size[2]     ;number of rows in image  
  nord = redpar.nords     ;total number of orders
  iord = findgen(nord)    ;Generated Orders in range [0,nord]
  orcdeg = 4.          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome)
  ;BUT  eventually loss of  precision begins increasing the errors again.MAKE SURE RESIDUALS DECREASE.
  mmfrac = 0.05       ;maximum fraction missing peaks allowed. Only Up to 5% of the spectrum can be missing.
  maxome = 10.        ;max allowable mean pixel error in orcs. Previous = 2 i ---------------------NEEDS IMPLEMENTATION 
 
  
  
  
  
  
  
  ;------------------------------
  ; >> Find Middle-Y reference points
  ;------------------------------  
  ;dummy = create_polynomial(img)  ; To find new polynomial  if needed
  pkcoefs =redpar.pkcoefs_slicer  ; This polynomial was found using function "create_polynomial"
  y_peaks = poly(iord,pkcoefs) 





  ;------------------------------
  ; >> Traces all Orders 
  ;------------------------------ 
  ;Returns 2-D array [iord,n_columns] where values are the Y image index for the corresponding n_column
  traced_orders = trace_all_orders(flat, y_peaks,redpar=redpar, only_nearest_max = 0) 


 
 
 
 
 
 
 
  ;------------------------------
  ; >> Debugging
  ;------------------------------
   
  if debug gt 0 then begin
    ;Plotting ALL Orders by values found in tracing
      p1=plot(traced_orders[0,*] )
      for i= 1, nord-1 do begin
          p1=plot(traced_orders[i,*], /overplot)
      endfor  
      stop, 'Type .cont to continue '    
  endif
  
  


  
  
  
  
  
  ;------------------------------
  ; >> Fitting each order
  ;------------------------------
  orc= dblarr(orcdeg+1,nord) ; Initializing array to store polynomia of each order.
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
  






  ;------------------------------
  ; >> Debugging
  ;------------------------------
  
  
  if debug gt 0 then begin
    ;  >> Plots FITTED orders on top of raw image
    dummy = image(img)
    img2=img
    for ior = 0,nord-1 DO BEGIN
      x=findgen(n_columns)
      calculated_y=poly(x,orc(*,ior))
      img2[x,calculated_y] = 0d
      p1=plot(calculated_y ,color='red',/overplot)
    endfor
    dir= redpar.rootdir +redpar.debugging + redpar.date+'.order_tracing_over_master_flat.fits'
    writefits, dir, img2
    stop, 'Check for created file and Type .cont to continue ' 
  endif

  if debug gt 0 then begin     
    
       ; >> Plots FIITED orders: 1 red, 1 in the midle and 1 blue order:
       x=indgen(n_elements(traced_orders[0,*]))
       pa= plot( x,traced_orders[5,*] ,SYMBOL= 'dot',SYM_THICK=3, LINESTYLE= 6, TITLE= 'Indexed Order 5 -  Red Order' )
       calculated_y=poly(x,orc(*,5))   
       pa= plot( x,calculated_y ,COLOR= 'red', /overplot)       
           
       pb= plot( x,traced_orders[35,*] ,SYMBOL= 'dot',LINESTYLE= 6, SYM_THICK=3, TITLE= 'Indexed Order 35 -  Middle Order' )
       calculated_y=poly(x,orc(*,35))
       pa= plot( x,calculated_y ,COLOR= 'red', /overplot) 
       
       
       pc= plot( x,traced_orders[70,*] ,SYMBOL= 'dot',SYM_THICK=3,LINESTYLE= 6, TITLE= 'Indexed Order 70 -  Blue Order' )
       calculated_y=poly(x,orc(*,70))
       pa= plot( x,calculated_y ,COLOR= 'red', /overplot)
       stop, 'Type .cont to continue ' 
      
                 
  endif
 
 



   ;stop, "forced stop delete this "
   RETURN, orc
end