
;+
; :Description:
;  Used to multiply array wrt specific dimension
;-
function operate_cons, array, constants, multiply=multiply, divide=divide
  compile_opt idl2

  for order = 0L, n_elements(constants)-1 do begin
    
    if  keyword_set(multiply) then begin
        array[*, order] = reform(array[*, order]) * constants[order]
        
    endif else begin
      ; divides
      
        array[*, order] =reform( array[*, order]) / constants[order]
    endelse
    
  endfor

  return, array
end


;+
; :Description:
;      >>  DEPRICATED  <<   A VERSION THAT ACTS ON THE IMAGES THEMSELVES HAS BEEN IMPLEMENTED IN SORTING_HAT.PRO
;    THIS REMOVES COSMIC RAYS AFTER EXTRACTING AND NORMALIZES THE SPECTRUM BEFORE getting rid of crs
;    Gets rid of the cosmic rays present in the spectra stored in the files passed as PATHS.
;    It ONLY runs if the number of observations is 3 or greater.
;    * Altears the files themselves and adds a HISTORY card with they keyword CR-CLEANED
;
; Input:
;     paths : Array with the paths to get rid of cosmic rays.
;     sigma : Value of sigma to be used.
;
;
;-
function remove_cr_by_sigma, paths, sigma
  compile_opt idl2

  if ~keyword_set(sigma) then sigma = 3

  if n_elements(paths) lt 3 then begin
    print, ' '
    print, '                 WARNING      '
    print, ' Cosmic Rays have not been removed. Please change the variable remove_crs from the ctio.par file to use either La Cosmic or the Legacy code.  '
    print, ' '
    print, ' Please, type :  cont.      to continue the reduction wihtout removnig cosmic rays.  '
    print, ' '
    stop, ' '
    return, -1 ; No success
  endif

  ; 1) Read first to find out the dimensions
  ;--------------------------------
  ref = readfits(paths[0], /silent)
  n_pixels = (size(ref))[2]
  n_orders = (size(ref))[3]

  ;2)  Create data cube & find averages per order
  ;--------------------------------
  avgs =list()
  spectra= make_array(n_pixels, n_orders, n_elements(paths),/double )
 ; p=plot([0],[0], title=' Original ')

  for idx = 0L, n_elements(paths)-1 do begin
    spectrum = readfits(paths[idx], /silent) ; Will read [2,4112,74]
    spectrum = reform(spectrum[1,*,*])
    spectra [*,*,idx]= spectrum
   ; P=plot(spectrum, /overplot  )
    avgs.add,  mean(spectrum, dimension=1, /double) ; Transforms to [1, 73] pr [73]
  endfor

  ; each of the 74 represents 1 order :
  avgs = avgs.toarray() ; transforms from a list([74],[1 for each order ]....,[74] ) to ->  [ [74], [74], [74] . . .  [74] ] up to the normal of obersevarion that exist
  ; E .g. 12 x 73
  avg  = mean(avgs, dimension=1, /double) ;  VERIFY !!!!  thre should be 1 for each obervation -> [avg1, avg2 , ....  avg73]  E .g 4 x 73



  goldenFactors = list()

  ;3) normalize to the found mean
  ;--------------------------------
  for idx = 0L, n_elements(paths)-1 do begin
    spectrum         = spectra [*,*,idx] ; 4112 x 73
    goldenFactor   = avg  / mean(spectrum, dimension=1, /double)    ; For given spectra observation. I'm manipulating all orders at the same time.
    ; mean(spectrum, dimension=2) ; -> [avg1, avg2 , ....  avg74]
    ; avg -> already  [avg1, avg2 , ....  avg74]
    ;goldenFactors -> gives me back 74 golden Factor one for each order.
    goldenFactors.add,  goldenFactor
    spectra [*,*,idx] = operate_cons(spectrum,goldenFactor, /multiply ) ; Tricky manipulation of arrays cause   [4112,73] x [73]
  endfor
  goldenFactors = goldenFactors.toarray()  ; [4,73]
  master_median = median(spectra, /double, dimension=3) ; Yes, [4112, 73 ],  Used to do replacement of crs .


  ;4) Calculate absolute deviations
  ;----------------------------------
  mad = spectra ; median absolute deviation  as cube
  for idx = 0L, n_elements(paths)-1 do begin
    mad[*,*,idx]  = abs( spectra [*,*,idx] - master_median)
  endfor
  mad =median(mad, /double, dimension=3) ; 4112 x 73, Yes !
  zeroIndices = where(mad eq 0) ; Correcting in case any is zero
  mad[zeroIndices] =1


  ;5) Purge Spectra
  ;----------------------------------
  todayDate = SYSTIME()
  todayDate = strmid(todayDate,20,4) + strmid(todayDate,3,7)

  ;p=plot([0],[0] ,title='removed cosmic rays')
  for idx = 0L, n_elements(paths)-1 do begin

    path= paths[idx]
    
    originalY=   readfits(path, hd )
   
    y= spectra [*,*,idx]
    
    excludeIdxs = where( (abs(y - master_median) / mad) gt sigma , n_crs)
    print, 'CRs : ' +strt(n_crs)
    y[excludeIdxs] = master_median[excludeIdxs]
    constants = reform(goldenFactors[idx,*])
    y= operate_cons(y,constants, /divide) ; to return to original intensity
    ;p= plot( y*avgs[idx] ,/overplot)
    
    originalY[1,*,*] = y


    pos=stregex(path, 'chi([0-9]+)\.([0-9]+)\.fits$', length=len)
    print,"CR_ROMOVE:  Found  "  + strt(n_crs)+" CRs  in file : "+strmid(path, pos, len)
    history_str = 'CR-CLEANED : '+todayDate+' : ' + strt(n_crs)+ ' from : '  +strt(n_elements(paths))  +' files'
    ; The statement "CR-CLEANED" serves as reference to identify if the file has been cleaned previouly. Do not remove.
    sxaddpar, hd, 'HISTORY', history_str
    writefits,  path, originalY, hd


  endfor


  return, 1 ; Sucess
end



; Testing
;num_files= indgen(1170-1159 +1 ) + 1159
;paths = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\tous\mir7\fitspec\210526\wchi210526.'+strt(num_files)+'.fits'
;!null = remove_cr_by_sigma(paths)
;
;
;end
