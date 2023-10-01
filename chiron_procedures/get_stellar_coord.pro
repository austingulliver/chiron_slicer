;+
; :Description:
;
; Finds the RA and DEC from either the header or making a SIMBAD request
;
; Output : ra and dec in  FOMRAT  RA(h:m:s) |  Dec(d:m:s)
;
; ra and dec are in string format ready for presetation in sheet
; coords server to pass to
;-
pro get_stellar_coord, hd, starname=starname, ra=ra, dec=dec, coords=coords
  compile_opt idl2

  ra=sxpar(hd,'RA', count=ra_match  )
  dec=sxpar(hd,'DEC', count=dec_match)
  coords= [0.0, 0.0] ; default value
  if  ra_match gt 0  and dec_match gt 0 then begin

    ; no need to do anything else
    ; ;  THEY COME IN FORMAT RA(hours:minute:second) |  Dec(degree :minute :ssecond)
    ; format to format acceptable for qbary


    coords_ra  = float(strsplit(ra,':',/EXTRACT))
    if coords_ra[0] gt 0.0 then begin
      coords_ra = coords_ra[0] + (coords_ra[1]/60.0) +  (coords_ra[2]/3600.0)
    endif else begin
      coords_ra = abs(coords_ra[0]) + (coords_ra[1]/60.0) +  (coords_ra[2]/3600.0)
      coords_ra =-1.0*coords_ra
    endelse



    coords_dec  =float(strsplit(dec,':',/EXTRACT))
    if coords_dec[0] gt 0.0 then begin
      coords_dec = coords_dec[0] + (coords_dec[1]/60.0) +  (coords_dec[2]/3600.0)
    endif else begin
      coords_dec = abs(coords_dec[0]) + (coords_dec[1]/60.0) +  (coords_dec[2]/3600.0)
      coords_dec =-1.0*coords_dec
    endelse


    ; qbary is expecting :  RA & DEC, in [hours,degrees] eg. [1.75,-15.9d]
    coords=[ coords_ra,   coords_dec]


  endif else begin
    ; else need to make request to SIMBAD  to obtain RA and DEC

    if ~keyword_Set (starname) then begin
      print, "get_stellar_coord: The programs is expected to make a request to SIMBAD but var 'starname' is not passed. "
      ra  = '00:0:00'
      dec = '00:00:00'
    endif else begin

      print, 'call_qbary:  Requesting RA and DEC information from SIMBAD ..... '
      QuerySimbad, starname , ra, dec, found=coord_found
      ;dec comes back as degrees
      ;ra comes back as degrees

      if  coord_found ne 0 then begin
        ; format in correct format
        ; RA(hours:minute:second) |  Dec(degree :minute :ssecond)

        ra= ra* (24.0/360.0)      ; Transformation from degrees to hours
        hours   =  floor(ra)
        minutes = floor( (ra - floor(ra) ) *60.0  ); 60 minutes /1 hour
        seconds  =  floor(    (  ((ra - floor(ra) ) *60.0 ) -   floor((ra - floor(ra) ) *60.0)  ) *60.0   )

        ra= strt(hours) +':' +strt(minutes) +':'+strt(seconds)

        degrees = floor(dec)
        minutes = floor(  (dec - floor(dec))*60  ) ; 1 degree/ 60 minutes
        seconds = floor(  (      ((dec - floor(dec))*60)      -floor(((dec - floor(dec))*60)    )          )*60.0)
        dec = strt(degrees) +':' +strt(minutes) +':'+strt(seconds)


        coords = [ra,dec]

      endif else begin
        print, 'call_qbary: Request failed '
        ra  = '00:0:00'
        dec = '00:00:00'
      endelse
    endelse
  endelse
  
end