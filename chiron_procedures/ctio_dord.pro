pro ctio_dord,  ordfname, redpar, orc,ome, image=image
;  Determines  the default order locations for a particular
;  spectrograph setting. 
;
;INPUTS:
;   ORDFNAME   (input string) Filename of FITS file to be used
;   REDPAR      parameter structure. Current mode is passed there.
;   IMAGE      optional image to use for order location, e.g. summed flat
;
;OUTPUTS:
;     ORC  (array (# coeffs , # orders))] coefficients from the
;          polynomial fits to the order peaks.
;     OME  (optional output vector (# orders))] each entry gives the mean of the
;           absolute value of the difference between order locations and the polynomial
;           fit to these locations.
; 6-Oct-11 AT  Added redpar as argument to use getimage.pro 


if n_params() lt 3 then begin
  print,'syntax: ctio_dord,ordfname, redpar,orc[,ome].'
  retall
end

  print,'CTIO_DORD: Entering routine.'

  if ~keyword_set(image) then begin ; read order-location image from the disk
     image = getimage(ordfname, redpar, header=head)  
     if (size(image))[0] lt 2 then begin
       print, 'CTIO_DORD: Image is not found. Returning from all'
       retall
     endif
  endif

  

;For SlICER mode we take a different approach. The other modes can be implemented in a similar Fashion but pkcoefs for each has to be found. 
dmode = strt(redpar.modes[redpar.mode])

if dmode eq 'slicer' then begin
       ;  >>If slicer then return 2-d arrray with order_ys[ # of orders , # of columns] E.g 74 x 4112
       orc = optimal_order_tracing(image, redpar )
       ; >> We save the n- structure to later extract the orders usign them
       
       
       ;2. Prefix added to FITS headers:
       prefix=redpar.prefix   ; e.g. 'chi111003'
       if strpos(prefix,'.') lt 0 then prefix=prefix+'.' ; add the point
       
       
endif else begin
  
      ; >> For any other order we give back  an array with the the polynomia fitted to each order 
      swid = 32    ;arbitrarr swath width, the number of columns desired in each swath. 
      fords,image,swid,orc, ome, redpar ;find order location coeffs
      ;orc (output)
      ;ome (output)
      ;swid (input)
      ;image (input)  image from which order location is found
      name = redpar.rootdir+redpar.orderdir+prefix+mode+'.orc'
      wdsk, orc, name, /new
      print, 'REDUCE_CTIO: Order Coefficients are stored as '+name
      
endelse



;  print,'CTIO_DORD: Saving order locations to ' + prefix + '.ord'
;  comment = 'Order location fit coefficients.'  
;  wdsk,orc,prefix + '.ord',comment,/new		;  save ORCs to disk
  
  return
end
