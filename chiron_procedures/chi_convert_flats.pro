;+
;
;  NAME: 
;     chi_convert_flats
;
;  PURPOSE: 
;   To convert the "rdsk" flat files to FITS files.
;
;  CATEGORY:
;      CHIRON
;
;  CALLING SEQUENCE:
;
;      chi_convert_flats
;
;  INPUTS:
;
;		DATE: The date, in yymmdd format, of the flats to be converted.
;
;  OPTIONAL INPUTS:
;
;  OUTPUTS:
;
;  OPTIONAL OUTPUTS:
;
;  KEYWORD PARAMETERS:
;    
;  EXAMPLE:
;      chi_convert_flats, date='120302'
;
;		To run it in batch mode:
;		chi_create_many_logs, startdate='120302', enddate='121231', /flatfits
;
;  MODIFICATION HISTORY:
;        c. Matt Giguere 2015.02.24 18:10:40
;
;-
pro chi_convert_flats, $
date = date

;create the flat root name:
fltroot = '/tous/mir7/flats/chi'+date+'.'

;now make an array of all four observing modes
;these will be looped through to create all the
;.fits files:
modes = ['fiber', 'narrow', 'slit', 'slicer']

;if the slit.flat file from the night exists, then proceed with 
;converting all flats to FITS files:
if file_test(fltroot+modes[2]+'.flat') then begin
	;now loop through the modes:
	for i=0, n_elements(modes)-1 do begin
		;read in the old file format:
		rdsk, flt, fltroot+modes[i]+'.flat'
		;now write to FITS:
		writefits, fltroot+modes[i]+'flat.fits', flt
	endfor
endif;file_test

end;chi_convert_flats.pro 