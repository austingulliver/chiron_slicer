  ;SUMMARY :
  ;Program to build a wavelength scale from coefficients from solvarr.
  ;
  ;INPUT :  
  ; wvc (input vector) wavelength information from fndwvc.pro. WVC structure produced by THID.PRO
  ; ord (input scalar) absolute order number for which to find wavelengths. By default extracted from WVC 
  ; col1 (input scalar) starting column number of segment.
  ; col2 (input scalar) ending column number of segment.
  ; vel= (input scalar) radial velocity (in km/s) shift to apply to
  ;   wavelength scale (positive velocities increase wavelengths).
  ; pixel_offset (input scalar) : spectra was found shifted in the pixel direction by n pixels and THID.PRO could not respond to it. 
  ;                             : This offset of pixels compensates for that 
  ;                             : by default =0
  ;   
  ;OUTPUT:
  ; w (output vector) wavelength scale for order ord, columns col1-col2.   
  ; 
  ;HISTORY :
  ; 
  ;04-Aug-92 JAV  Create.
  ;15-Aug-92 JAV  Now assume fit is vs. column/100, not column. Also now
  ;    fitting cubic vs. column/100.
  ;19-Sep-92 JAV  Added huge new code section to handle version 2 wvc format.
  ;    Can now build entire wavelength array, if requested.
  ;15-Jul-93 JAV,GB  Version 2.2; cross terms added; fits vs. order/100 now;
  ;    bug in order polynomial fixed; increased nord to 2.
  ;03-Nov-93 GB Added vel= keyword and logic.
  ;06-Dec-93 JAV  Documented last change.
  ;07-Jun-94 JAV  Negative ord, col1, and col2 are now equivalent to not
  ;   specifying the arguments at all.
  ;15-Jul-93 JAV,GB  Version 2.3; made cols/1000
  ;23-Jan-97 JAV  Return wavelength array in double precision.
  ;27-Nov-99 JAV  Allow 4 or 6 cross-terms (through version 2.5).
  ;15-Nov-2020 Jorge Lozano. Customized to allow displacement in the pixel direction 


pro mkwave2,w,wvc,ord,col1,col2,vel=vel,pixel_offset=pixel_offset
  
  
  
  if n_params() lt 2 then begin
    print,'syntax: mkwave,w,coefs [,ord,col1,col2,vel=]'
    retall
  endif

  if n_elements(ord)  eq 0 then  ord = -1     ;set default values
  if n_elements(col1) eq 0 then col1 = -1     
  if n_elements(col2) eq 0 then col2 = -1   
  if n_elements(pixel_offset) eq 0 then pixel_offset = 0

  
print,  'pixel offset is  :'  +string(pixel_offset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; VERSION 1.0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Handle version 1.0 .wvc files.
  if n_elements(wvc) eq 6 then begin
    message,/info,'This is apparently an old wvc format.'
    message,/info,'You should convert to the new format.'
    if n_params() lt 3 then $
      message,'You must specify ord as third argument when using old wvc.'
    if col2 lt 0 then $
      col2 = long(wvc(5)) - 1     ;build to last column
    len = col2 - col1         ;desired number of columns
    ic = (col1 + dindgen(len)) / 100d0    ;column indicies / 100
    nl = (wvc(0) + wvc(4)*ord) $
      + ic*(wvc(1) + ic*(wvc(2) + ic*wvc(3))) ;calc. n*lambda efficiently
    w = nl / ord        ;recover wavelengths
    return
  endif



 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;; VERSION 2
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  ;0)>> Extract info from wvc information block.
  vers = wvc(0)         ;file structure version number
  ncol = wvc(1)         ;number of columns in spectrum
  nord = wvc(2)         ;number of orders in spectrum
  obase = wvc(3)        ;base order in spectrum
  fill = wvc(4:6)       ;filler in reserved space
  ncross = fix(wvc(7))        ;number of cross-terms
  coldeg = fix(wvc(8))        ;degree of column (pixel)fit poly
  orddeg = fix(wvc(9))        ;degree of order fit poly
  coeff = wvc(10:*)       ;fit coefficients





  ;1)>> Build column indicies/100.0 for polynomial construction.
  if col1 lt 0 then col1 = 0                    ;begin in first col
  if col2 lt 0 then col2 = long(ncol) - 1       ;build to last column
  len = col2 - col1 + 1                         ;desired number of columns
  ic = (col1 + dindgen(len)) / 100d0            ;column indicies / 100
  if vers ge 2.3 then ic = ic / 10.d0           ;column indicies / 1000 (2.3 up)



  ;2)>> Set the desired order range.
  if ord lt 0 then begin        ;true: build entire array (Default value ord=-1)
    obeg = obase                ;got it from WVC
    oend = obase + nord - 1     ;end with last order
  endif else begin              ;else: only one order
    obeg = ord                  ;begin with the order we want
    oend = ord                  ;end with the order we want
  endelse
  no = oend - obeg + 1          ;number orders to build



  ;3)>> Loop thru orders, building wavelength array.
  w = dblarr(len,no)                  ;init wavelength array
  for i=0,no-1 do begin               ;loop thru orders
      order = (obeg + i) / 100d0      ;current order
      nlc = dblarr(len)               ;init n*lambda column piece
      
      
      ;This has been modified to allow for displacement of pixels IF number of coeff in x is 6      
      if coldeg eq 6 then begin        
           nlc= ((ic^6 )*coeff(6) )  + ((ic^5 )*coeff(5) )   + ((ic^4 )*coeff(4) )  +  ((ic^3 )*coeff(3) ) +  ((ic^2 )*coeff(2))  + ( (ic+(pixel_offset/1000d0))*coeff(1) )         
      endif else begin
          for j=coldeg,1,-1 do begin        ;loop back thru col coeffs
              nlc = ic * (nlc + coeff(j))   ;build polynomial for PIXEL (or columns)
          endfor        
      endelse      
    
      
      nlo = dblarr(len)                         ;init n*lambda order piece
      for j=coldeg+orddeg,coldeg+1,-1 do begin  ;loop back thru order coeffs
          nlo = order * (nlo + coeff(j))        ;build polynomial for ORDERS
      endfor
      
      if vers lt 2.2 then begin         ;no cross-terms
          nlx = 0.0
      endif else begin                  ;cross-terms added
          inx = coldeg + orddeg +1      ;start of cross-terms
          case ncross of
              4: nlx = order*ic *( coeff(inx) + ic*coeff(inx+1) + order*coeff(inx+2) + ic*order*coeff(inx+3) )
              6: nlx = order*ic *( coeff(inx) + ic*coeff(inx+1) + order*coeff(inx+2) + ic*order*coeff(inx+3) $
                                                                + ic^2*coeff(inx+4) + order^2*coeff(inx+5))
              else: begin
                  print, 'mkwave: unexpected number of cross-terms: ' $
                    + strtrim(ncross, 2)
                  return
              end
          endcase
      endelse
      
      nl = coeff(0) + nlc + nlo + nlx   ;build n*lambda
      w(*,i) = nl / double(obeg + i)    ;recover wavelengths
  endfor


  ;4) >> velocity shift (Optional)
  if keyword_set(vel) then begin
    w = w * (1.0 + vel/2.9979246e5)   ;apply velocity shift
  endif

  return
end
