;+
; :Description:
;  Used to get the boxcar extraction of the frame (im) passed as an argument.
;-

;+
; :Description:
;
;-
pro getarc_slicer,  im, orc, orderNum, redpar, arc, ybi, yti

  ; recall orc for slicer is order_ys[ # of orders , # of columns] E.g 74 x 4112
  
  
  
  order_width =  redpar.xwids[redpar.mode ] - round(redpar.pixel_not_extracted )
  ; >>Define some constants
  im=double(im)
  ncol=n_elements(im[*,0])        ;number of columns
  ix=findgen(ncol)                ;vector of column indicies. E.g 4112
  arc=ix*0.0                      ;dimension arc vector   (Vector 4112 filled with 0s)


  yti = orc[orderNum, * ] +  round((order_width/2.0) - 1 ) ; The y index values of the TOP edge of the order
  ; If full slicer then  round((order_width/2.0) - 1 ) = 5

  ybi = orc[orderNum, * ]   - round(order_width/2.0)    ; The y index values of the BOTTOM edge of the order
  ; If full slicer then round(order_width/2.0) = 6


  ; The min and max value of min(ybi),max(yti) implicently define the swath to be considered

  for row=min(ybi),max(yti) do begin    ;loop through valid rows
    srow=im[*,row]        ;get CCD row : vector  along the dispersion direction. Contains some pixels than actually are within the order ( the others are background)

    ;>>  masking the pixels that are background
    mask=srow*0.        ;make a mask for it
    madd=where(row ge ybi and row le yti,nummad)  ;choose pixels in this row, madd is the same size as the srow, as ybi, as yti
    if nummad gt 0 then mask[madd]=1.       ;that belong in this order




    ; srow x mask gives me back the a vector (where the values that survided after multipling by mask will be  the ones elected )
    ; Trick: arc : will keep begin a vectorwith the 4112  values but as it iteratates in the cross dispersion direction it will sum over the 12
    ; corresponding pixels for each column
    arc=arc+srow*mask       ;add them into extracted spectr
  endfor

  ; arc : vector e.g 4112   which is the boxcar extracted order
  ; yti and ybi get returned for  testing purposes


end
