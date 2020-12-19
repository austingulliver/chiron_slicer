; Top-level script for CHIRON data reduction
; input example : allreduce, '111003'
; Oct 18, 2011 AT
pro allreduce, night

;modes=['slit','slicer', 'narrow', 'fiber']
;modes=['slit', 'narrow', 'fiber']
;modes=['slicer','fiber']
modes=['slicer']

for i=0, n_elements(modes)-1 do begin
    print, ' '
    print, ' '
    print, '               ... ALLREDUCE: Reducing specta for '+ string(modes[i]) + ' mode ...'  
    sorting_hat, night, mode=modes[i], /reduce, /getthid, /iod2fits, /combine_stellar ,thar_soln='wvc_slicer_181103.sav'; thar_soln was added
endfor
;for i=0, n_elements(modes)-1 do sorting_hat, night, mode=modes[i], /reduce,  /iod2fits

print, ' - - - - - - - - - - - - - - - - - - - - - - -   End of Script - - - - - - - - - - - - - - - - - - - - - - - '
end

