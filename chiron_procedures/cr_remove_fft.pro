

;+
;
; :Description:
;               Helper method used to find out if 'value' is within the list 'ls'
;-
function in_ls, ls, value

   arr = ls.toArray()  
  !null =  where(arr eq value, nns)  
  if nns gt 0 then return, 1 else return, 0
  
end





;+
;
; :Description:
;               Helper method used to find out if values in list 'ls' are monotonic
;-
function is_monotonic, ls
  compile_opt idl2
   
   n_eles = n_elements(ls)
   if n_eles eq 1 then return, 1   
   if   ls[1] - ls[0] gt 0 then  monotonicity = 'increasing' else  monotonicity = 'decreasing' 
   
   prev = ls[0]   
   for idx = 1L, n_eles-1 do begin      
      IF monotonicity EQ 'increasing' then begin
          if prev gt ls[idx] then  return, 0  ; it supposed to be increasing but is decreasing        
      ENDIF ELSE BEGIN
          ; DECREASE         
          if prev le ls[idx] then  return, 0  ; it supposed to be decreasing but is increasing 
      ENDELSE
      prev= ls[idx]
   endfor  
  return, 1
end



;
; :Description:
;               Helper method used to find out if values in list 'ls' represent a valley
;-
function is_valley, ls
    
    ; find out if pixel in the middle is less than the sides.
   
 
    
    middle_px = floor( ls[n_elements(ls)/2] )
    
    if middle_px lt ls[0] and middle_px lt ls[n_elements(ls)-1] then return, 1
    
    return, 0
    

end






;+
;
; :Input:
;         indices_ls :  Indices selected as CRs (includes the skirt of the crs)
;         intensities : All intensties in the order.
;
; :Description:
;            This add pixels to the selected crs based on :
;                 - a CR will always be at least 3 pixels wide.
;                 - If pixel width is an even number. we round up to the closest odd number. 
;
;                :Example:
;                         If the width of the cr is 1 then I make it 3 (I add1 pixel by the right and the 1 pixels by the left)
;                         If 3 I leave it as 3
;                         If 4 then I make it 5
;                         If 5 I leave it as 5
;                         If 6 then I make it 7
;-
function tweak_crs , indices_ls, intensities
  compile_opt idl2
  
 
  n_eles= indices_ls.count()
  
  if n_eles le 1 then return, indices_ls ; I could still give back 3 rather than 1 index if count - 1
  
  ;otherwise tweak the answer
  new_ls = list()
  sub_ls = list()
  sub_ls.add, indices_ls[ 0 ]
  prev = indices_ls[ 0 ]
  
  for idx = 1L, n_eles-1 do begin
      
      diff=   abs( intensities[prev] -intensities[idx] )      
      if diff gt 1 then begin
          new_ls. add, sub_ls        
          sub_ls = list()
          sub_ls.add,  indices_ls[ idx ]
          prev = indices_ls[ idx ]       
      endif else if diff eq 1 then begin
          sub_ls.add,  indices_ls[ idx ]
          prev = indices_ls[ idx ] 
      endif ; if is 0 then we  just ignore sing it repeated     
      
      ;if is the last iteration then I need to add sub_ls to new_ls regarless
      if idx eq n_eles-1 then new_ls. add, sub_ls

  endfor
  
  
  
  ; Now I have a list of lists where each list represent a cr
  ; We make sure of : 
  ;                   - The width of cr is an odd number of pixels.
  ;                   - If width of 1then  we select 3
  ;                   - For any other number of indices we round up to closest odd number
  
  
  foreach cr_ls, new_ls do begin
    
      n = cr_ls.count()       
      if n eq 1  then begin      
         ; have to add 2 indices. One before and 1 after 
         cr_ls.add, cr_ls[0]-1
         cr_ls.add, cr_ls[0]+1       
      endif else if (n mod 2) eq 0 then begin ; if it's even, then increase the number of selected indices.
         ; Notice I have to add 1 but not sure if left or right, so :      
          cr_arr = cr_ls.toarray()
          if  intensities[cr_arr[0]] ge intensities[cr_arr[-1]] then  added = cr_arr[0] -1  else  added = cr_arr[-1] +1
          cr_ls.add, added 
      endif ; else must be odd and is not 1 so there is nothing to do. 
  
  endforeach
  
  
  ; -----------------------
  ; Monotonicity Check
  ; -----------------------
  ; Iterate again to do the monotonicity check 
  final_ls = list()
   foreach cr_ls, new_ls do begin   
       
      cr_ls_indices=cr_ls.sort()
      cr_ls_indices_arr = cr_ls_indices.toarray()
      cr_ls= intensities[cr_ls_indices_arr]
      
      if is_monotonic(cr_ls) eq 0 and  is_valley(cr_ls) eq 0 then final_ls = final_ls+cr_ls_indices 
   endforeach

  return, final_ls
end























;+
;      ------------------
;      :   Main Methods  :
;      -------------------
;
; :Input :
;         - order : 1-D array  with the intensities of the order. The array can have any size. 
; :Description:
;               - Runs order by order
;-



function cr_remove_fft, order, sigma_multiplier, skirt_level, frac, total_crs
  compile_opt idl2
  

  ;-----------------------
  ; Intiial Variables
  ;-----------------------

  total_crs=0
  
  nfft = n_elements(order) ; Fortran code has this set to 8192 for some reason and changes as the number of observations increases ( Why number of observations ?)
  nf = (frac/100.0) *  nfft
  debug = 0 



  ; ---------------------------
  ; Creating Hanning Filter
  ; ---------------------------
  hwindow   = hanning( 2*nf, ALPHA=0.50, /double  ) ; The 0..5 is defined by the hannning filter.
  hwindow   = shift( hwindow,  round( n_elements( hwindow )/2.0 ) ) ; The filter gets modified such that there is a valley in the middle rather than a peak. 
  h_filter  = DBLARR(nfft)
  h_filter[0:round( n_elements( hwindow )/2.0 ) ]      = hwindow[0:round( n_elements( hwindow )/2.0 )]
  h_filter[nfft-round( n_elements( hwindow )/2.0 ) :*] = hwindow[round( n_elements( hwindow )/2.0 ) :*]


  ;; Uncomment for further  DEBUGGIN the  Hanning Filter
  ;tt='Hanning Filter with frac=' +strt(frac)
  ;p=plot(h_filter, title=tt )




  ;-----------------------
  ; Fast Fourier Step
  ;-----------------------

  cc=fft(order,1)
  ;; Uncomment for further Debugging 
  ; p=plot(cc, title='Comaprison in FFT space' )
  cc= cc* h_filter ; Apply hanning filter
  
  ;; Uncomment for further  Debugging 
  ;p=plot(cc, color='blue', thick=2, /overplot)
  ff=fft(cc,-1)
  ff=real_part(ff)  ; We only interested in the real part. 
  

  




  ;-----------------------
  ; Iterate
  ;----------------------- 
  ; Iterate based on the following conditions.
  ;  1. Max of ten iterations
  ;  2. If sdv increases
  ;  3. If no more CRs are found.
 

  prev_std = 0.0
  counter = 1 
  
  
  diff = order-ff   ; fft will give some smooth version of the spectra
  std  = stddev(diff) ; 1st STD

  while (std gt prev_std or counter lt 10 ) do begin

      ;-----------------------
      ; Find CRs
      ;-----------------------
      ; >> 1st Attempt to find crs. 
      cr_indices=where(diff gt sigma_multiplier*std  , ncrs)
      
     if ncrs le 0 then break ; If no CRs  Found

      ;Remove the first two and last 4 crs from cr_indices. We making the assumptions there will not be crs in these pixels
      selected = where(cr_indices gt 10 and cr_indices lt nfft-10 , nns)  ; Making sure no pixels within the first and last 4 have been selected.
      cr_indices= cr_indices[selected]
      
      if nns le 0 then break ; If no CRs  Found
    
    
    
    
    
    
      
    
    
      ;-----------------------
      ; Selecting the skirt of the CRs
      ;-----------------------
    
      ; Find all the skirt values for which I want to find the interpolation
      upper_bound = sigma_multiplier*std
      lower_bound = upper_bound * (1.0/skirt_level)
      flagged_indices = list()
    
      foreach cr_index, cr_indices do begin
          if ~in_ls(flagged_indices,cr_index) then flagged_indices.add, cr_index
          is_skirt_left  = 1
          is_skirt_right = 1
          tempo_idx=cr_index
          
          ; Backwards
          while (is_skirt_left and (tempo_idx GT 0) ) do begin
              tempo_idx = tempo_idx - 1
              idx_diff = abs(diff[tempo_idx])
              if (idx_diff gt lower_bound) AND  ~in_ls(flagged_indices,tempo_idx)  then flagged_indices.add, tempo_idx else  is_skirt_left=0
          endwhile
      
          tempo_idx=cr_index
          ;Forward
          while (is_skirt_right and tempo_idx LT nfft ) do begin
              tempo_idx = tempo_idx + 1
              idx_diff = abs(diff[tempo_idx])
              if (idx_diff gt lower_bound) AND   ~in_ls(flagged_indices,tempo_idx)  then flagged_indices.add, tempo_idx else  is_skirt_right=0
          endwhile
      endforeach
    
    
    
      ;-----------------------
      ; Tweak cr selection
      ;-----------------------
    
      flagged_indices = tweak_crs(flagged_indices, order) ; Add pixels depending on width of the cr.
      flagged_indices = flagged_indices.toArray()
      
      if n_elements(flagged_indices) le 0 then break ; If no CRs  Found
      
      selected = where(flagged_indices gt 10 and flagged_indices lt nfft-10 , nns)  ; Making sure no pixels within the first and last 4 have been selected.
      flagged_indices= flagged_indices[selected]

      
      
      if n_elements(flagged_indices) le 0 then break ; If no CRs  Found

      if debug eq 1 then begin
          print, ' -------------------------------------------------- '
          print, '                 Iteration ' + strt(counter)
          print, ' -------------------------------------------------- '
          print, ' Standard deviation : ' +strt(std)
      endif



      if debug eq 1 then begin
        title = 'Iteration '+strt(counter)
        p=plot(order, title=title)
        p=plot(ff ,color='blue', thick=2 , /overplot)
        p=scatterplot(cr_indices, order[cr_indices],  SYMBOL='square', /overplot) ; These are the crs INITIALLY selected 
        p=scatterplot(flagged_indices, order[flagged_indices],  SYMBOL='Star', /overplot) ; The star CRS are selected as part of further processing
      endif
     
      
    
    
      
      ;-----------------------
      ; Replacing & Fitting new values
      ;-----------------------
      all_indices     =  LINDGEN(nfft)
      all_indices[flagged_indices] = -1000 ; !null didn't work so I try with a large negative value. It's momentary anyways
      good_indices    = where(all_indices ne -1000, nns )
    
      f_intensities = order[good_indices]
      f_indices     = all_indices[good_indices]
      all_indices   = indgen(nfft-1)
      interpolated  = Hermite( f_indices, f_intensities,all_indices)
      
      
      tt= 'Iteration '+strt(counter) + ' : Spectrum in blue has no CRs '
      if debug eq 1 then p = plot(order, title=tt )
      
      order[flagged_indices] = interpolated[flagged_indices]  ; Raplacing values in new order
      
      if debug eq 1 then p = plot(  order , color='blue', thick=2 , /overplot )
      
      
      ; ------------------------
      ; used in next iteartion
      ; ------------------------
      total_crs = total_crs+n_elements(flagged_indices)
      
      if debug eq 1 then print, ' Number of Crs : ' +strt(n_elements(flagged_indices))
      
      diff = order-ff   ; fft will give some smooth version of the spectra
      std  = stddev(diff) ; 1st STD
      prev_std = std
      counter = counter +1
      
      
      
      ;-----------------------
      ; Fast Fourier Step
      ;-----------------------

      cc=fft(order,1)
      ;; Uncomment for further Debugging
      ; p=plot(cc, title='Comaprison in FFT space' )
      cc= cc* h_filter ; Apply hanning filter

      ;; Uncomment for further  Debugging
      ;p=plot(cc, color='blue', thick=2, /overplot)
      ff=fft(cc,-1)
      ff=real_part(ff)  ; We only interested in the real part.
      
      diff = order-ff   ; fft will give some smooth version of the spectra
      std  = stddev(diff) ; 1st STD
  
  endwhile
  
  print, ''
  print, 'Number of TOTAL crs : ' +strt(total_crs)
  print, ' '
  print, 'EndOf the Cr rountine ---------------------------'
  print, ''
  return, order
end
















;-----------------------
; Main Debugging  
;-----------------------
; This section only runs when running individually (Not part of the program) .



;; -----------------------
;; Example 1 : 
;; -----------------------
;restore, 'C:\Users\mrstu\idlworkspace_yalecalibration\Default\an_order_img.sav'
;row= reform(order_img[0:3199,2])
;yy= reform(total(order_img, 2, /double))
;;p=plot(yy[0:3199])
;order= yy[0:3199]




;; -----------------------
;; Example 2 :
;; -----------------------
y=readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210524\post_processed\wchi210524.1139.fits')
sec_w_cr=y[1, 1800:2250 ,5]   ; 6th blue order
sec_no_cr = y[1, 2200:2300 ,5]

order= reform( y[1,*  ,41] )
order = order - mean(order)

sigma_multiplier = 3.0
skirt_level=1.5
frac=28.0
total_crs=0

dummy = cr_remove_fft(order, sigma_multiplier, skirt_level, frac, total_crs  ) 




END