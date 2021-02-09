function select,selector,item
;This function checks if the input ITEM is contained in the 
;array SELECTOR.  		ECW	10-15-93


;length= n_elements(selector)
;
;for index = 0L, length-1 do begin
;  if strtrim(selector[index],2) eq  strtrim(item,2) then begin
;    return, 1
;  endif
;
;endfor


; Old vERSION 
dum=where(strpos(selector,item),cnt)
found=cnt ne n_elements(selector)

return, found
;return, 0
end
