;+
; :Description:
;    Trying to find where within section, signal is. Where section is an array greater than signal 
;    
;    Returns an array with the same dimension as signal  where every point is the difference of signal with section 
;    The rem
;
;    OUTDATED 
;-
function corr_order, signal,  section 
  compile_opt idl2
  
  differences =list()
  
  ;I want to take the difference of signal- section (Only account for the section pixels that 'overlap index wise' with signal )
  ; We start with signal   at index 0 -11  and shift as 1-12  ->  2-13 -> 3-14  
  ; Thus we actually doing is considenring different sections of section 
  
  ; We iterate over  n times  where n is found by diff between len(section) - len(signal)
  n=  n_elements(section) - n_elements(signal)
  
  for index = 0L, n do begin
    
    low_section_bound = index
    upper_section_bound =low_section_bound+ n_elements(signal)-1
    
    shifted_section = section[low_section_bound: upper_section_bound]
    
    difference = abs(signal-shifted_section  )
    difference = total(difference) / n_elements(difference)
    
    differences.add, difference 

  endfor

  ; The index [0] differences is the one where signal is at the most left of section 
  return, differences.toarray()
end
