
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
  M = make_array(n_pixels, value={x:0, y:0.0})

  all_indices =  indgen(n_pixels)
  M.x =  all_indices
  M.y = intensities


  ;-----------------------------
  ;  Convex Hull algorithim
  ;-----------------------------
  ;l = 0
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



  ;-----------------------------
  ;  Plotting : Troubshooting
  ;-----------------------------


  ;  ;p=plot(idx, order/res) ; result
  ;  tt= 'Interpolated Hull : 1 pixel every '+ strt(window_size)
    p=plot(intensities, title=tt )
    p=plot(all_indices,final_hull,  color ='blue' ,/overplot)
  
    p=plot(mfIndices, intensities[mfIndices] , SYMBOL='square' , /overplot)
  
   p=plot(intensities/final_hull, title ='Flattened Order ')

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




for order = 0L, n_orders-1 do begin

  intensities = flat_spectrum(yprime[1,*,order])
  wavelengths = yprime[0,*,order]
;  ; -----
;  ; Check for intersection for MERGING 
;  ; -----
;  if order gt 0  then begin
;    if prev_wavalengths[-1] gt wavelengths[0]  then begin
;      ;These orders intersect. We keep the prev_intensities. Thus, cut the intentisities at index target
;      diffs =  wavelengths-prev_wavalengths[-1]
;      diff_idxs =where(diffs ge 0 , ns)
;      intensities = intensities[diff_idxs[0]: * ]
;      wavelengths = wavelengths[diff_idxs[0]: * ]
;
;    endif
;  endif
;
;  prev_wavalengths=wavelengths
 ; p=plot(wavelengths,intensities, /overplot  ) ;, title=t)

endfor






end