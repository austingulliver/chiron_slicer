
function orientiation, p1,p2,p3
  compile_opt idl2

  ;  # to find the orientation of
  ;  # an ordered triplet (p1,p2,p3)
  ;  # function returns the following values:
  ;  # 0 : Colinear points
  ;  # 1 : Clockwise points
  ;  # 2 : Counterclockwise

  val = (float(p2.y - p1.y) * (p3.x - p2.x)) -   (float(p2.x - p1.x) * (p3.y - p2.y))
  if val gt 0 then begin ; Clockwise direction
    return, 1
  endif else if val lt 0 then begin ;   then coounter clockwise
    return, 2
  endif else begin ; else colinear orientation
    return, 0
  endelse

end






;+
; :Convex Hull Algorithim:
; 
; Input :
; Intensities of  the spectrum. We assuming these intensities belong to pixels : 1,2,3,4 ......n_pixels
; 
;  Reuturn all ( Only the top section)  the indices of the points considered to create the hull 
;  ; 
;-
function convex_hull, intensities, x_values
  compile_opt idl2
  
  n_pixels =n_elements(intensities)
  M = make_array(n_pixels, value={x:0, y:0.0})
  
  
  
  
  M.x =  x_values
  M.y = intensities
  
  l= n_pixels-1
  hull= list()
  p=l
  q=0


  while (1) do begin

    hull.ADD, p
    q=(p+1)mod n_elements(M)
    for index = 0L, n_elements(M)-1 do begin
      if orientiation(M[p], M[index], M[q]) eq 2 then begin
        q=index
      endif
    endfor
    p=q

    if p eq 0 then break

  endwhile

  hull = hull.ToArray()  ; May have to delete the first and las element from here
  
  
  ; Add the first and last pixels in case they were not included.
  dummy=where(hull eq 0, nEnd)
  if nEnd eq 0 then hull= [hull,[0]]
  dummy=where(hull eq n_elements(intensities), nEnd)
  if nEnd eq 0 then hull= [hull,[n_elements(intensities)]]



  return, hull
end



;+
; :Description:
;
;-
function select_neighbors, all_intensities, x_values 
  compile_opt idl2

  pixel_diff =4
  groups = list()  
  prev   = x_values[0]
  
  n_group=list()
  n_group.add, prev
  
  ;group all of them together
  for idx = 1L, n_elements(x_values)-1 do begin
    
    val= x_values[idx]
    if ( val - prev ) le pixel_diff then begin
      n_group.add, val   
    endif else begin
      n_group =n_group.toarray()
      groups.add, n_group
      n_group=list()
      n_group.add, val
    endelse
    prev=val

  endfor
  
  ; In the case last part was not added to groups
  if n_group.count() gt 0 then begin
    n_group =n_group.toarray()
    groups.add, n_group
  endif
  
  ;choose one per group
  selected_indices= list() 
  
  foreach array_idxs, groups do begin
    
    corres_intensities = all_intensities[array_idxs]
    dummy              = max(corres_intensities,max_i)
    selected_indices.add, array_idxs[max_i]

  endforeach

  

  return, selected_indices.toarray()
end



;+
; :Description:
; Given an array (intenstities) this procesdure does the following
; 1. Applys a convex hull
; 2. Interpolates (Hermite Polynomials) to all points based on the convex hull choosen points.
; 3. Create bins every 100 pixels, for every bin, it selects the pixel closest to the convex hull values
; 4. Using 3, it intorpolates for all values.
;
;
;-
function flat_spectrum, intensities
  compile_opt idl2

  intensities =reform(intensities)
  n_pixels =n_elements(intensities) 
  all_indices =  indgen(n_pixels)
  
  ;-----------------------------
  ;  Convex Hull algorithim
  ;-----------------------------
  hull = convex_hull( intensities, all_indices)
  hull = hull[sort(hull)]
  
  p=plot(intensities, title='Convex Hull Algorithm : Hull Creation  ')
  p=scatterplot(hull , intensities[hull], SYMBOL='square',  /overplot)


; >>  Interpolate all hull pixels   
; res_hull= interpol(intensities[hull],  hull, all_indices) ;testing for hermite spline rather than linera interpolation
  res_hull =  Hermite( hull, intensities[hull], all_indices)

  p=plot(all_indices , res_hull, color= 'rd',  /overplot) ; Plotting all interpolated values


  ;-----------------------------
  ;  Further Processing
  ;-----------------------------
  ; every window_size pixels, pick a pixel which its difference is the lowest between the hull and spectrum
  ; IFF there is no point already recorderd by the hull 
  ;iterate over the windows if there are points in the hull that match with the window range then take them all 
  ; otherwise pick them from proximity to res_hull
  
  
  mfIndices =list()
  window_size=100
  n_windows  = round( n_elements(intensities)/window_size )
  n_window=0

  while (n_windows gt  n_window) do begin
    
    ref            = window_size *n_window    
    window_lower = ref
    window_upper = ref+window_size-1    
    
    idxs_in_range = where( hull ge window_lower and hull le window_upper, n_in_window )
    
    if n_in_window gt 0 then begin
      ;Simply add the indices that already exist in the hull    
      for index = 0L, n_in_window-1 do begin ;Adding all to the list 
        mfIndices.add, hull[idxs_in_range[index]]
      endfor      
    endif else begin
      ;Pick the indices by proximity
      spectralWindow = intensities[window_lower:window_upper]
      hullWindow     = res_hull[window_lower:window_upper]
      diff = hullWindow-spectralWindow
      dummy=min(diff, min_idx) ; finding the closest
      mfIndices.add, ref+min_idx
    endelse
    n_window=n_window+1
  endwhile
  
  
  ; Adding the last one in case was lost it
  mfIndices= mfIndices.toArray()
  mfIndices = mfIndices[sort(mfIndices)]
  dummy=where(mfIndices eq n_elements(intensities), nEnd)
  if nEnd eq 0 then mfIndices= [mfIndices,[n_elements(intensities)]]
  
   
  ;  ----- End of Further Processing ------





  ; >>Interpolate again but now with more points : Needed to get the concave part of the Spectrum
  ;final_hull= interpol(intensities[mfIndices], mfIndices,  all_indices) ; Final points to be used
   final_hull =  Hermite( mfIndices, intensities[mfIndices], all_indices)

  
  ;-----------------------------
  ;  Last Fix
  ;-----------------------------
  ;  In case there are any values for which the interpolation is lower
  diff= final_hull -intensities
  replace_idxs = where(diff < 0.0 )
  
  extra_indices= select_neighbors( intensities, replace_idxs )
  mfIndices = [mfIndices, extra_indices]
  mfIndices = mfIndices[sort(mfIndices)]
  
  ; and delete any values for which its intenstities form a valley 
  
  tempo_mfIndices =list()
  for idx_tempo = 0L, n_elements(mfIndices)-2 do begin
    
    if idx_tempo eq 0 or idx_tempo eq  n_elements(mfIndices)-2 then begin
      tempo_mfIndices.add, mfIndices[idx_tempo]
    endif else begin
      if  ~(intensities[ mfIndices[idx_tempo] ] lt intensities[ mfIndices[idx_tempo -1] ] and intensities[ mfIndices[idx_tempo+1] ] gt intensities[ mfIndices[idx_tempo ] ] ) then tempo_mfIndices.add, mfIndices[idx_tempo]
    endelse
    

  endfor
  
  tempo_mfIndices= tempo_mfIndices.toarray()
  mfIndices = tempo_mfIndices
  dummy=where(mfIndices eq n_elements(intensities), nEnd)
  if nEnd eq 0 then mfIndices= [mfIndices,[n_elements(intensities)]]
  

  
  
  
  
  

  
  
  ; >> Last interpolation
  final_hull = Hermite( mfIndices, intensities[mfIndices], all_indices) 
;  When using spline  : hermitina 1 and 2nd derivites match. Shows to be better than spline and interpol
  final_hull_spline = SPLINE( mfIndices, intensities[mfIndices], all_indices)
  ;final_hull_linear= interpol(intensities[mfIndices], mfIndices,  all_indices)

   
   


  ;-----------------------------
  ;  Plotting : Troubshooting
  ;-----------------------------


 ;   p=plot(idx, order/res) ; result
    tt= 'Interpolated Hull : 1 pixel every '+ strt(window_size) + ' using Hemitian Spline interpolation '
    p=plot(intensities, title=tt )
    p=plot(all_indices,final_hull, thick=2, color ='blue' ,/overplot)  
    p=plot(all_indices, final_hull_spline , LINESTYLE=3, color='red', /overplot)
    p=plot(mfIndices, intensities[mfIndices] , SYMBOL='square' ,  LINESTYLE=2,  /overplot)
    
    p=plot(intensities/final_hull, title ='Flattened Order : Linear interpolation')
   
   
  ;  Spline comparable PLOTs
;   p=plot(intensities, title='usign Spline' )
;   p=plot(all_indices,final_hull_spline,  color ='blue' ,/overplot)  
;   p=plot(mfIndices, intensities[mfIndices] , SYMBOL='square' , /overplot)
;   p=plot(intensities/final_hull_spline, title ='Flattened Order : Linear  interpolation')

  return, intensities/final_hull

end









;--------------------------------
; ----- MAIN
; -------------------------------
;

; Fiting the continium of the spectra

yprime=readfits('C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210415\post_processed\wmchi210415.1148_4.fits')

y=reform(yprime[1,*,*])
sz =size(y)
n_orders= sz[2]
n_pixels=sz[1]

 ;Uncomment for plotting 1 order
; One order : 
order= 6;  Hydrogen line problem - > 6;21

intensities = flat_spectrum(yprime[1,*,order])
wavelengths = yprime[0,*,order]

p=plot(intensities, /overplot  )


;;Uncomment for All Orders
;
;for order = 0L, n_orders-1 do begin
;
;  intensities = flat_spectrum(yprime[1,*,order])
;  wavelengths = yprime[0,*,order]
;;  ; -----
;;  ; Check for intersection for MERGING 
;;  ; -----
;;  if order gt 0  then begin
;;    if prev_wavalengths[-1] gt wavelengths[0]  then begin
;;      ;These orders intersect. We keep the prev_intensities. Thus, cut the intentisities at index target
;;      diffs =  wavelengths-prev_wavalengths[-1]
;;      diff_idxs =where(diffs ge 0 , ns)
;;      intensities = intensities[diff_idxs[0]: * ]
;;      wavelengths = wavelengths[diff_idxs[0]: * ]
;;
;;    endif
;;  endif
;;
;;  prev_wavalengths=wavelengths
;   p=plot(wavelengths,intensities, /overplot  ) ;, title=t)

;endfor






end