function clean_key_dictionary, key
  alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
  numbers = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
  
  ; Cleaning up string 
  new_key=''
  key = strtrim(key)
  len_key = strlen(key)
  ;Only select characters that are alphabet or numbers.
  for i=0, len_key do begin
    curr_char = key.CharAt(i)
    if total(alphabet eq curr_char.toLower()) eq 1 then begin
      new_key+= curr_char
    endif else if total(numbers eq curr_char) eq 1 then begin
      if i eq 0 then begin 
        new_key+="x"
      endif 
      new_key+= curr_char  
    endif
  endfor
  return, new_key   
end