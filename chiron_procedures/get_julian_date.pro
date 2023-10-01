; Convert UT midpoint time to float hours
; input; string array in 'y-m-dTh:m:s' format
; output: ut, >24h after midnight
function getJulianDate, mdpt
  n = n_elements(mdpt)
  ut = dblarr(n)
  for i=0,n-1 do begin
    s = mdpt[i]
    date = stregex(s, '([0-9]+)-([0-9]+)-([0-9]+)T([0-9]+):([0-9]+):([0-9]+.[0-9]+)',/SUBEXPR,/EXTRACT)
    if n_elements(date) eq 7 then begin
      yy = fix(date[1]) & mon = fix(date[2]) & dd = fix(date[3]) & hh = fix(date[4]) & mm = fix(date[5]) & sec = fix(date[6])
      if yy eq 0 then begin
        jdate = 0
      endif else begin
        jdate = JULDAY(mon,dd,yy,hh,mm,sec)
        ;jdate = jdate([yy,mm,dd,hh,mm])
      endelse
      ut[i] = jdate
    endif else begin
      print,  s + " is not in the following format y-m-dTh:m:s. Defaulting to 0.0."
      ut[i] = 0.0
    endelse
  endfor
  return, ut
end


