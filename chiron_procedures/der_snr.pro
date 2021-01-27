;*****************************************************************************************
;+
;*NAME:
;     DER_SNR
;*CLASS:
;*CATEGORY:
;     spectra
;*AUTHOR:
;     Felix Stoehr [ST-ECF]
;*PURPOSE:
;     Estimate the derived signal to noise ratio from the flux.
;*CALLING SEQUENCE:
;     snr = der_snr(flux)
;*PARAMETERS:
;     flux      (REQ) (I) (N) (F)
;                 array containing the flux values (the computation is unit independent)
;*EXAMPLES:
;*SYSTEM VARIABLES USED:
;*INTERACTIVE INPUT:
;*SUBROUTINES CALLED:
;     medianvalue like the IDL median but for pair numbers of values in the array
;                   the average value of the closest values is returned. In the median
;                   function of IDL, the higher of the two values is returned.
;*FILES USED:
;*SIDE EFFECTS:
;*RESTRICTIONS:
;*NOTES:
;       The DER_SNR algorithm is an unbiased estimator describing the spectrum as a whole as long as
;       * the noise is uncorrelated in wavelength bins spaced two pixels apart
;       * the noise is Normal distributed
;       * for large wavelength regions, the signal over the scale of 5 or more pixels can
;         be approximated by a straight line
;
;       For most spectra, these conditions are met.
;
;*PROCEDURE:
;       This function computes the signal to noise ratio DER_SNR following the
;       definition set forth by the Spectral Container Working Group of ST-ECF,
;       MAST and CADC.
;
;       signal = median(flux)
;       noise  = 1.482602 / sqrt(6) median(abs(2 flux_i - flux_i-2 - flux_i+2))
;     snr           = signal / noise
;       values with padded zeros are skipped
;*REFERENCES:
;       * ST-ECF Newsletter, Issue #42:
;         www.spacetelescope.org/about/further_information/newsletters/html/newsletter_42.html
;       * Software:
;         www.stecf.org/software/ASTROsoft/DER_SNR/
;*I_HELP  nn:
;*MODIFICATION HISTORY:
;     24.05.2007, fst, Initial Import
;       01.01.2007, fst, added more help text, switched the functions, added a missing '
;-
;
;*****************************************************************************************

function medianvalue, xx

  ;*****************************************************************************************
  ;  Like the IDL median but for pair numbers of values in the array
  ;  the average value of the closest values is returned. In the median
  ;  function of IDL, the higher of the two values is returned.
  ;
  ;  Felix Stoehr, [ST-ECF]

  xxx = xx(sort(xx))
  n   = n_elements(xxx)

  if ((n mod 2) eq 0) then begin
    return, 0.5 * (    max( [0,xxx(n/2-1) ] )       +   xxx(n/2)   )
  endif else begin
    return, xxx((n-1)/2)
  endelse

end
;-----------------------------------------------------------------------------------------
;
;*****************************************************************************************

function DER_SNR, flux

  ;*****************************************************************************************

  if (n_params(0) eq 0) then begin
    print, 'usage: der_snr, flux'
    print, 'flux:         array containing the fluxes'
    print, 'return value: estimated signal to noise ratio'
    retall
  endif

  select     = where(flux ne 0.0)
  selectflux = flux(select)
  n          = n_elements(selectflux)-1

  if (n gt 4) then begin
    signal     = medianvalue(selectflux)
    noise      = 0.6052697 * medianvalue(abs(2.0 * selectflux(2:n-2) - selectflux(0:n-4) - selectflux(4:n)))
    return, signal/noise
  endif else begin
    return, 0
  endelse

end
;-----------------------------------------------------------------------------------------




;; testing 
;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_10_first.fits
;cubeachi_10_1=readfits(achiFileName)
;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_10_second.fits
;cubeachi_10_2=readfits(achiFileName)
;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_10_third.fits
;cubeachi_10_3=readfits(achiFileName)
;
;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_10_fourth.fits
;cubeachi_10_4=readfits(achiFileName)
;
;
;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_40.fits
;cubeachi_40=readfits(achiFileName)
;
;
;print, ' Results  SNR '
;print, ' SNR first_10 :' +string(DER_SNR(reform(cubeachi_10_1[1,*,28]  )))
;print, ' SNR second_10 :' +string(DER_SNR(reform(cubeachi_10_2[1,*,28]  )))
;print, ' SNR third_10 :' +string(DER_SNR(reform(cubeachi_10_3[1,*,28]  )))
;print, ' SNR fourth_10 :' +string(DER_SNR(reform(cubeachi_10_4[1,*,28]  )))
;
;print, ' SNR added_40 :' +string(DER_SNR(reform(cubeachi_40[1,*,28]  )))
;
;
;;value = find_ref_points (cubeachi )
;;p=plot(cubeachi[0,*,*],cubeachi[1,*,*] , NAME = 'PLOT_WIDGET_DOC', /CURRENT , TITLE = ' * 40 added Spectra' )
;
;
;
;END 