
function is_consecutive, prev, current
; SUMMARY :
;          Takes two numbers and returns true if current is a conscutive number wrt to prev.
   if (current eq prev+1)  then return, 1 else return, 0
end




function make_range_from_vector, file_numbers
;INPUT:   
;      file_number: vector with all files numbers (integers) to find the ranges from 
;OUTPUT:
;      ranges_str : string with all ranges from the files 
;SUMMARY :
;       It sorts number files and create a range (string) out of the file numbers
;       This is algorthim does not work if file _numbers values are repeated or are not integers 


   ; >> Sort all numbers in ascending  order
   file_numbers = file_numbers [ sort(file_numbers) ]
   
   ; >> treat first case separate
   final_string= ''
   range=list()
   range.Add, file_numbers[0]
   
   ;range_end=0L
   
   ; >> Iterate over every file number
   length=n_elements(file_numbers)
   for index = 1L, length-1 do begin
        
        if (is_consecutive( file_numbers[index-1], file_numbers[index]) eq 1 ) then begin  ; If true
            ;range_end = file_numbers[index]
            
            ; take care of last number
            if (index eq length-1 ) then begin
              range.Add, file_numbers[index]
              final_string = final_string + ', ' + strtrim(string(range[0]),1) + '-'+ strtrim(string(range[1]),1)
            endif

                    
        endif else begin
          
            range.Add, file_numbers[index-1]
            if range[0]  eq range[1] then final_string = final_string + ', ' + strtrim(string(range[0]),1) else  $
                                          final_string = final_string + ', ' + strtrim(string(range[0]),1) + '-'+ strtrim(string(range[1]),1)
            
            range.Remove,/ALL
            
            ; Set new beginning value
            range.Add, file_numbers[index]
            
             if (index eq length-1 ) then final_string = final_string + ', ' + strtrim(string(range[0]),1) 
                
        endelse

    

   endfor 


   return, final_string.Substring(1,-1)  
end




;UNCOMMENT THIS FOR TESTING SCENARIO
;x=[1041, 1042, 1043, 1044, 1046, 1048, 1049, 1050]
;st= mk_range_from_vector(x)
;print, 'output : ' +st
;
;END