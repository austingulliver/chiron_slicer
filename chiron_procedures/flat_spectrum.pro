
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
; 2. Interpolates to all points based on the convex hull choosen points.
; 3. Create bins every 100 pixels, for every bin select pixel closest to the convex hull values
; 4. Used values found in 3 to interpolate for all values.
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
  
  

  p=plot(intensities, title='Convex Hull Algorithm : Hull Creation  ')
  p=scatterplot(hull , intensities[hull], SYMBOL='square',  /overplot)


  ; Interpolate all hull pixels
   hull = hull[sort(hull)]
   res_hull= interpol(intensities[hull],  hull, all_indices)

    p=plot(all_indices , res_hull, color= 'rd',  /overplot) ; Plotting all interpolated values


  ;-----------------------------
  ;  Further Processing
  ;-----------------------------
  ; every window_size pixels, pick a pixel which its difference is the lowest between the hull and spectrum



  mfIndices =list()
  window_size=100
  n_windows  = round( n_elements(intensities)/window_size )
  mfIndices.add, 0
  counter=0

  while (n_windows gt  counter) do begin

    ref = window_size *counter
    spectralWindow = intensities[ref:ref+window_size-1]
    hullWindow = res_hull[ref:ref+window_size-1]
    diff = hullWindow - spectralWindow  ; best case scenario equals 0
    dummy=min(diff, min_idx)

    mfIndices.add, ref+min_idx
    counter=counter+1

  endwhile


  mfIndices= mfIndices.toArray()
  final_hull= interpol(intensities[mfIndices], mfIndices,  all_indices) ; Final points to be used


  
  diff= final_hull -intensities
  replace_idxs = where(diff < 0.0 )
  
  extra_indices= select_neighbors( intensities, replace_idxs )
  mfIndices = [mfIndices, extra_indices]
  
  
  mfIndices = mfIndices[sort(mfIndices)]
  final_hull= interpol(intensities[mfIndices], mfIndices,  all_indices)
  
  ;Only for comparison purposes, when using spline 
  final_hull_spline = SPLINE( mfIndices, intensities[mfIndices], all_indices)
  ;-----------------------------
  ;  Extra:  asure all values over spectra
  ;-----------------------------
;  diff= final_hull -intensities 
;  replace_idxs = where(diff < 0.0 )
;  mfIndices=[mfIndices,replace_idxs]
;  
;  mfIndices=mfIndices[SORT(mfIndices)]
;  
;;  ; key part : Make all the other points
;;  mean_in_x =mean(mfIndices)
;;  mena_in_y =mean(intensities[mfIndices] )
;;  ;Make eveything tobe in the middle except for mfIndices
;  
;  smooth_intensities= interpol(intensities[mfIndices],  mfIndices, all_indices)
;  
;  mfIndices = convex_hull( smooth_intensities,   all_indices) ; applying convex hull with rectified indices
  
  
;  final_hull= interpol(intensities[mfIndices], mfIndices,  all_indices)
  

  ;-----------------------------
  ;  Plotting : Troubshooting
  ;-----------------------------


  ;  ;p=plot(idx, order/res) ; result
    tt= 'Interpolated Hull : 1 pixel every '+ strt(window_size) + ' using Linear interpolation '
    p=plot(intensities, title=tt )
    p=plot(all_indices,final_hull,  color ='blue' ,/overplot)  
    p=plot(mfIndices, intensities[mfIndices] , SYMBOL='square' , /overplot)
    p=plot(intensities/final_hull, title ='Flattened Order : Linear interpolation')
   
   
   ; Spline comparable PLOTs
   p=plot(intensities, title='usign Spline' )
   p=plot(all_indices,final_hull_spline,  color ='blue' ,/overplot)  
   p=plot(mfIndices, intensities[mfIndices] , SYMBOL='square' , /overplot)
   p=plot(intensities/final_hull_spline, title ='Flattened Order : Spline interpolation')

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


; One order : 
order=  4;  Hydrogen line problem - > 6;21

intensities = flat_spectrum(yprime[1,*,order])
wavelengths = yprime[0,*,order]

p=plot(intensities, /overplot  )


;Uncomment for All Orders
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
; ; p=plot(wavelengths,intensities, /overplot  ) ;, title=t)
;
;endfor
;





end