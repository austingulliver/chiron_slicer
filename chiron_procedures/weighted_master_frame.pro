;+
; :Description:
; 
; :Input:
;       typeStr (string) :  median :Statistical median
;                        :  mean   :Statistical mean
;                        :  sum   :The pure sum of all frame rather than the mean /median
;                        :  mean_clipping :  The mean sigma clipping version of the mean (trimmed mean). Clipping is done about median, but mean is returned. ;                        
;                        :  abs_error :  Weights assigned according to the absolute error.  The actual Value is assumed to be the median
;                        :  squared_error :  Weights assigned according to the squarred error. The actual Value is assumed to be the median
;                        :  symmetric_ape : Weights assigned according to the symmetric Absolute Percentage Error. The actual Value is assumed to be the median
;                        :  bayesian      : Assumimng a Normal Distribution and assign weights inversely proportional to Bayesian probability 
;-
function weighted_master_frame, data_cube, typeStr
  
  sz = size(data_cube)
  n_cols= sz[1]   ;# columns in image
  n_rows=  sz[2]  ;# rows in image
  
  columns = findgen(n_cols)          
  rows = findgen( n_rows)          
  n_frames = sz[3]        ; Number of frame 

  
  print, '>> Master Frame created by  ' +string(typeStr)
  switch (typeStr) of
    ;**************************************************
    ; *   Median
    ;**************************************************
    'median': begin      
      master_frame = median(data_cube, /double, dimension=3)
      break
    end
    ;**************************************************
    ; *   Mean
    ;**************************************************
    'mean': begin
      master_frame = mean(data_cube, /double, dimen=3)
      break
    end
    ;**************************************************
    ; *   Sum
    ;**************************************************   
    'sum': begin
      master_frame =total(data_cube, 3, /double) 
      break
    end
    ;**************************************************
    ; *   Sigma Clipping 
    ;**************************************************
    'mean_clipping': begin
      
      master_frame= dblarr(n_cols, n_rows) 
      foreach column, columns do begin          
          foreach row, rows do begin 
              values = reform(data_cube[column, row, *]) ; value to evaluate on                  
;              t_mean = mean(values)
              meanclip, values, out_mean, out_sigma, CLIPSIG=2, MAXITER=2
;              print, 'clippend mean: ' +strt(out_mean) + ' | mean: '+strt(t_mean)
;              print, ' '
              master_frame[column, row] =out_mean               
          endforeach 
       endforeach
        
      

      break
    end

    ;**************************************************
    ; *   Absolute Error
    ;**************************************************
    'abs_error': begin
      
      master_frame= dblarr(n_cols, n_rows)
      
      foreach column, columns do begin
        foreach row, rows do begin
          values = reform(data_cube[column, row, *]) ; value to evaluate on
          
          mean_val= mean(values)
          median_val = median(values, /double)
          
          weights = 1.0/(1+abs(values -median_val)) ; The greatest diff has the least weight
          weights_sum = total(weights)
          weights = weights/weights_sum
;          PROVE= TOTAL(weights)
;          STOP, 'This has to equal 1 : '+string(prove)
          values= values*weights; Every value has been weighted already          
          
          out_mean= mean(values)*100.0         
;          print, 'modified mean: ' +strt(out_mean) + ' | mean: '+strt(mean_val)
;          print, ' '
          master_frame[column, row] =out_mean
        endforeach
      endforeach
      
      

      break
    end
    ;**************************************************
    ; *   Squared Error
    ;**************************************************
    'squared_error': begin
      
      master_frame= dblarr(n_cols, n_rows)

      foreach column, columns do begin
        foreach row, rows do begin
          values = reform(data_cube[column, row, *]) ; value to evaluate on

          mean_val= mean(values)
          median_val = median(values)

          weights = 1.0/  ( ((abs(values -median_val))^2 )+1.0 ); The greatest diff has the least weight
          weights_sum = total(weights)

;          PROVE= TOTAL(weights/weights_sum)
;          STOP, 'This has to equal 1 : '+string(prove)
          values= (values*weights)/weights_sum ; Every value has been weighted already

          out_mean= mean(values) *100.0
;          print, 'modified mean: ' +strt(out_mean) + ' | mean: '+strt(mean_val)
;          print, ' '
          master_frame[column, row] =out_mean
        endforeach
      endforeach

      break
    end
    ;**************************************************
    ; *   Symmetric Absolute Percentage Error
    ;**************************************************
    'symmetric_ape': begin ; SEEM TO BE NOT WORKING 
      
      master_frame= dblarr(n_cols, n_rows)

      foreach column, columns do begin
        foreach row, rows do begin
          values = reform(data_cube[column, row, *]) ; value to evaluate on

          mean_val= mean(values)
          median_val = median(values)

          weights = 1.0/  ( (  ( abs(values -median_val) +1.0 )  / (((values -median_val)+1.0)/2.0)  )*100)  ; The greatest diff has the least weight
          ; +1 added as a hack  so 
          weights_sum = total(weights)

          PROVE= TOTAL(weights/weights_sum)
;          STOP, 'This has to equal 1 : '+string(prove)
          values= (values*weights)/weights_sum ; Every value has been weighted already

          out_mean= mean(values)
          print, 'modified mean: ' +strt(out_mean) + ' | mean: '+strt(mean_val)
          print, ' '
          master_frame[column, row] =out_mean
        endforeach
      endforeach

      break
    end
    ;**************************************************
    ; *   Bayesian
    ;**************************************************
    'bayesian': begin
      
      master_frame= dblarr(n_cols, n_rows)   
      foreach column, columns do begin
        foreach row, rows do begin
          values = reform(data_cube[column, row, *]) ; value to evaluate on



          mean_val= mean(values)
          std_val = stddev(values)
          params= [ 1, mean_val, std_val] ;Paramters for the gaussian 
          
          weights = gaussian(values, params)   ; y: probability evaluated at the values passed
          
          
          values= (values *weights ) /total(weights)
          
          out_mean= mean(values) *100.0  

;          print, 'modified mean: ' +strt(out_mean) + ' | mean: '+strt(mean_val)
;          print, ' '
          master_frame[column, row] =out_mean
        endforeach
      endforeach
     
      
       
      
      
               

      break
    end
    
    ;**************************************************
    ; *   Exception
    ;**************************************************    
    else: begin
         stop, 'weighted_master_frame: >> Error<< typeStr must be an string according to the comments of the function.'
    end
  endswitch




  return, master_frame
end
