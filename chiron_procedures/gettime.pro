; Convert UT midpoint time to float hours
; input; string array in 'hh:mm:ss' format
; output: ut, >24h after midnight
function gettime, mdpt
  n = n_elements(mdpt)
  ut = fltarr(n)
  for i=0,n-1 do begin
    s = mdpt[i]
    yy = fix(strmid(s,0,4)) & mon = fix(strmid(s,5,2)) & dd = fix(strmid(s,8,2)) & hh = fix(strmid(s,11,2)) & mm = fix(strmid(s,14,2)) & sec = fix(strmid(s,17,2))
    if yy eq 0 then begin
       jdate = 0
    endif else begin
      jdate = JULDAY(mon, dd, yy, hh, mm, sec)
    endelse
    ut[i] = jdate
    PRINT, jdate, format='(f20.10)'
  endfor
  return, ut
end


