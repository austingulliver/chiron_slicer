

data1= readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210208\achi210208.mstr_stellar.fits',h1)

data2= readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\raw\mir7\210208\chi210208.1003.fits',h2)


history1 = sxpar(h1,'HISTORY' ) 
history2= sxpar(h2,'HISTORY' )


;print, history1
;print, ''
;print, ''
;print, ''
;print, history2

; A = FLTARR(10)
; 
;
;hasContent= history1
;
;CATCH, Error_status
;
;;This statement begins the error handler:
;IF Error_status NE 0 THEN BEGIN
;  PRINT, '' ;, Error_status
;  PRINT, 'Error message: ' ;, !ERROR_STATE.MSG
;;  ; Handle the error by extending A:
;    hasContent= 1  ; Cause it has content 
;    CATCH, /CANCEL
;ENDIF
;
;
;if hasContent eq 0  then print, 'ther is NO content'  ELSE  PRINT,'it has content  need to read ' 
;


;print, size(history1)
sz1=size(history1) 


;print, size(history2)
sz2= size(history2)

if sz1 [0] eq 0 then print, 'passs' else begin
  
  has = history1.Contains('CR-CLEANED')
  
  
  dummy = where( has eq 1, count)
  print, fix(count) 
  
endelse




END