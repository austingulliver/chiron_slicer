; convert month numbers to 3 character string names
;
function strMonth, mon
  nMonths = ['01','02','03','04','05','06','07','08','09','10','11','12']
  sMonths = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']

  n = where ( nMonths eq mon )
  if ( n lt 0 ) then m = '???' else m = sMonths[n]
  return, m
end