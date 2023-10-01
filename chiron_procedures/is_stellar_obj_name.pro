; Chech if object name corresponds to stellar exposure
; input: the object name 
; output: True => Object name corresponds to stellar exposure, Flase => Oject name corresponds to other type file (ThAr, flat ...)
function is_stellar_obj_name, objName 
  if objName eq 'iodine' or objName eq 'thar' $ 
  or objName eq 'focus' or objName eq 'junk' or objName eq 'dark' $
  or objName eq 'bias' or objName eq 'quartz' or objName eq 'master_stellar' then begin
    ; Not stellar file return false 
    return, BOOLEAN(0)
  endif else begin
    ; Stellar file return true 
      return, BOOLEAN(1)
    endelse
 end