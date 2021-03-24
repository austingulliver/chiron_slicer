



;+
; :Description:
;             ; It finds the noise level assuming the noise is more-less constant at a given X value. Returns
;               a range (two x values) of where to expect that noise.
;               y_values given as intensity vs  pixel
;             ; For emmision spectra the noise level can be found by 
;             ; This makes the assumption that the background level is more less constant 
;             
; Input:
;       noise_width(float) : Is the read out noise comming from the fits header of the frame.  
;       noise_str(str)     : can 2 two values 
;                           constant_band : returns a flaot which represent the backgroun level 
;                           band_fragments : returns a vector (same size as y_values) which values
;                                            correspond to the fitted values of the background.
;       section_width(float) : the mode band_fragments uses this argument to divide in array y_values in section 
;                              of length section_width. Out of each section the minima is used to fit the background.
;       poly_order (int) : If option   band_fragments is passed then poly_order has be specified. Default = 4
;       
; Output:  returns and array with 2 elements where [0] is the lower bound of the noise and [1] is the upper bound of the noise.
;  
;
; History :
;          Create by Jorge Andres Lozano 2021_20_03
;-
function background_level, y_values, noise_width=noise_width, noise_str = noise_str , section_width=section_width, poly_order=poly_order, debug=debug
  compile_opt idl2
  
  if not keyword_set(poly_order) then poly_order = 4
  if not keyword_set(noise_str) then stop,  '>> Error << Procedure is missing argument : noise_str'
  
  
  if noise_str eq 'constant_band' then begin
        
        
        ; Find the lowest-most repeated values for the whole spectrum
        if not keyword_set(noise_width) then noise_width=10.0     ; Width of the band that represent the noise. This will become the bin size of histogram
    
    
    
        ; NOTICE the type of  noise_width has to be the same as y_values
        hdf= histogram(y_values,binsize=noise_width,locations = bin_locations) ; returns histogram density function : integer vector equal to the density function of the input Array
        ; Returns integer values since  a histogram by def takes care of the NUMBER OF elemnts within given bin.
        ; bin_locations(output) locations of the bins used for the histogram
   
        ; Since we only care about the most common low value,  we only considered the first 1/4 of the spectrum
        lowest_intensities = hdf [0: round( n_elements(hdf)/ 4.0 )   ]
        noise_value = float( max(lowest_intensities ,max_idx) );  is the max of the histogram :thus the most common low value
    
        RETURN, noise_value
        
    
    
  endif else if noise_str eq 'band_fragments' then begin
        
       
       ; Recommendation : Let section_width be big enough to have part of the background  
       ; Devided the y values in sections : We find 1 minima for each section. 
       ; The trick is how big or short the section is. 
       collected_minima = list()
       ref_indices = list()
       
       if not keyword_Set(section_width) then stop, '>> Error << If linear_fragments_band option selected then fill section_width'
       
       end_section = -1.0
       while (end_section lt n_elements(y_values)-1 ) do begin ; Iterating over each section to find the minima             
           
            
            beg_section = end_section + 1. < float( n_elements(y_values) )-1.0
            beg_section = beg_section > 0.0            
            end_section = beg_section+(section_width-1.0) < float( n_elements(y_values) ) -1.0    ; It is (section_width-1) just to account for indices that selected in the array
            
            ref_indices.add, round( beg_section + ((end_section-beg_section)/2.0 ) )  
            collected_minima.add, min( reform(y_values[round(beg_section) : round(end_section) ]) )

       endwhile
       
       ; > account for the very first and the very last points in the array 
       ; > For the very first 
       ref_indices.add, 0, 0
       collected_minima.add, collected_minima[0], 0
       ; > For the very last
       ref_indices.add, n_elements(y_values)-1 ; they will get added at the end. 
       collected_minima.add, collected_minima[-1]
       
       collected_minima = collected_minima.toarray()
       ref_indices =ref_indices.toArray()
       

       ; > Fit the values to a 4th order polynomial 
       coefficients = POLY_FIT( ref_indices, collected_minima, poly_order )       
       all_x_values=indgen(n_elements(y_values))       
       fitted_noise_values= poly(all_x_values,coefficients)
       
       return, fitted_noise_values
       
    
  endif
  
END