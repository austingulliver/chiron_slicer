; Calcualte Julian Date and relativistic redshift ,RA (coords_ra)AND  DECLINATION (coords_dec)
; Input: header fits file 
; Output: JD, bary_correc, coords_ra, coords_dec

function logMakerCalc, hd, OBJName=objname

    if ~keyword_set(OBJName) then begin
      object = sxpar(hd, 'OBJECT')     ; Get object name
      OBJName = strcompress(object,/remove_all)
    endif
  
    return_vals = dictionary("date", '0000-00-00T00:00:00.000', 'JD', '0.0', 'bary_correc', '0.0', 'coords_ra', '00:00:00', 'coords_dec', '00:00:00', 'f_bary_correc', 0.0, 'is_stellar', boolean(0)) ; Default values 
    em_date_time = sxpar(hd,'EMMNWOB', count=mid_time_match) ; mean time without bckgrd subtraction 
    
    ; Check if EMMNWOB exists in header
    if mid_time_match le 0 then begin
      print, "EMMNWOB key not found in header. Defaulting to 0000-00-00T00:00:00.000"
      em_date_time = '0000-00-00T00:00:00.000'
    endif
    
    ;If em_date_time = 0000-00-00T00:00:00.000 then try to use EXPTIME and UTSHUT
    if is_stellar_obj_name(objName) then begin 
     if em_date_time eq '0000-00-00T00:00:00.000' then begin
       print, 'EMMNWOB is equal to 0000-00-00T00:00:00.000. Trying to use EXPTIME or UTSHUT instead.'
       expotime = sxpar(hd,'EXPTIME', count=c_exp_time)          ; float E.g. EXPTIME =  714. / Exposure time in secs
       date_and_time = sxpar(hd,'UTSHUT', count=c_sop_time)      ; string E.g. UTSHUT  = '2021-03-25T04:45:20.845' / UT of shutter open
       if c_exp_time eq 0 or c_sop_time eq 0 then begin
         print, "EXPTIME or UTSHUT do not exist in header. Leaving default 0000-00-00T00:00:00.000."; If EXPTIME or UTSHUT leave default.
       endif else begin
         half_exp = 0.5 * (expotime / 3600.) ; converting from seconds to hours
    
         ; Year, month and day extracted from UTSHUT
         ymd = stregex(date_and_time, '([0-9]+)-([0-9]+)-([0-9]+)',/SUBEXPR,/EXTRACT)
         hms = stregex(date_and_time, '([0-9]+):([0-9]+):([0-9]+.[0-9]+)',/SUBEXPR,/EXTRACT)
         hti  = ten(float(hms[1:*])) + half_exp
         mt = sixty(hti)
         hms_u = (cgNumber_Formatter(mt[0], DECIMALS=0)).extract('[0-9]+') + ":" + (cgNumber_Formatter(mt[1], DECIMALS=0)).extract('[0-9]+') + ":" + cgNumber_Formatter(mt[2], DECIMALS=3)
         
         ; Combining ymd with hti
         em_date_time =  ymd + "T" +  hms_u
         print, "New date extracted from EXPTIME or UTSHUT equals " + em_date_time + "."
       endelse
    endif
     
    if em_date_time eq '0000-00-00T00:00:00.000' then message, "This is a stellar exposure date cannot be 0000-00-00T00:00:00.000." ;For stellar em_date_time cannot be 0000-00-00T00:00:00.000
    
    jdUTC = getJulianDate([em_date_time])
    ; Define coords ra and dec
    get_stellar_coord, hd, starname=objName, ra=coords_ra, dec=coords_dec, coords=coords ; output are :  coords_ra and coords_dec
  
    ; Calculate-BCV
    call_qbary,  jd=jdUTC, barydir=barypath, czi=czi, coords=coords
    bary_correc = cgNumber_Formatter(czi,DECIMALS=6)
  
    ; Update dictionary with values
    return_vals["bary_correc"] = bary_correc
    return_vals["coords_ra"] = coords_ra
    return_vals["coords_dec"] = coords_dec
    return_vals["f_bary_correc"] = czi
    return_vals["is_stellar"] = boolean(1)
     
    endif else begin
       jdUTC = getJulianDate([em_date_time])
    endelse
    
    return_vals["date"] = em_date_time ; Set the date in return vals dictionary
    return_vals["JD"] = cgNumber_Formatter(jdUTC, DECIMALS=6);  Set JD in return dictionary
     
    return, return_vals
end


