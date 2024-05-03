;+
; :Description:
;               Hard-coded anomaly present in the Chiron CCD  (Blue orders - right hand side )
;               The image at this point in the software is as follows :
;               from left to right ( 4112 pixel - along the dispersion direction).
;               bottom (red orders ) to top (blue orders)
;               It also account for the upper-right section of the ccd where the signal of the orders
;               gets to the level of the noise
;
; : Input :
;          column : column number in the ccd.
;          order  : indexed order number in the ccd.
;-
function hasAnomaly, column, order
  anomalyColumns = [3387,3424]
  anomalyIndexedOrders = [60,75]; from redest to bluest

  noiseColumns = [3840,4111] ; These columns are at the noise level
  noiseOrders = [62,75]

  if order ge  anomalyIndexedOrders[0] then begin
    if  (column ge  anomalyColumns[0] and column le anomalyColumns[1])   or (column gt noiseColumns[0] )  then return, 1 else return, 0
  endif else return,0

end