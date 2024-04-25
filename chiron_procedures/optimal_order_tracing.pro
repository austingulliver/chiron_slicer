;Summary:
;      ** Main Function **
;       called from ctio_dord
;       Main method calls 
;       Only optimized for slicer but can be applied to other modes with minor adjustments
;       
;       In Generatl Keepp in mind that Chiron's ccd records 76 orders  ([0,75]) 
;       However the 1st [0] and last 2 orders [74,75] are incomplete and therefore are not considered 
;        
;       * This has only been tested for the slicer mode
;Input:
;      img:  2-D image of the master flat
;      initial_order_peaks:
;
;Output:
;      array with the  middles of all the orders for all the columns (4112 ) E.g.  74 x 4112
;      
;;Notes:
;      the polynomial "pkcoefs_slicer" was found from the 0th order to the nth order.
;      If extracting 74 order for example it would be from 0 to 74
; Written by Jorge Lozano 2021-14-03

function optimal_order_tracing, img, redpar

  ;------------------------------
  ; >> Constants
  ;------------------------------
  debug    = redpar.debug
  flat     = img
  img_size = size(flat)      ;size of image passed: e.g. Master Flat  4112 x 1366 for the slicer mode
  n_columns   = img_size[1]     ;number of columns in image
  n_rows      = img_size[2]     ;number of rows in image
 
  nord     = redpar.nords    ;total number of orders
  iord     = findgen(nord)   ;Generated Orders in range [0,nord]  
  orcdeg = 4.          ;polymial degree to fit order locations : Note increasing orcdeg initially decreases the fit residuals (ome)
  middle_column = round( n_columns/2.0 )
  scipy_signal= Python.Import('scipy.signal')
  slicer_width= round(redpar.xwids[redpar.mode] )

  ;------------------------------
  ; >> define n  templates (for each order)
  ;------------------------------  
  

  
  
  templates = define_templates(flat , redpar) ; n orders structure  with middle, up and down for every order (central column)
  

  if debug gt 0 then stop, " ** Templates recognizition  Sucessfull *** "
  
  ;------------------------------
  ; >> Cross-correlate every order 
  ;------------------------------
  

  orc= dblarr(orcdeg+1,nord) ; Initializing array to store polynomia of each order. +1 cause the contant 
  ; > Iterate over each order
  ; > cross correlate along the order,  such that I end up with 4112 points per order
  ; > Fit polynomial to 4112 points
  
  order_ys = MAKE_ARRAY(n_elements(templates.middle ), n_columns, /FLOAT, VALUE = 0.0) ; (#of Orders, # X Pixels (alond dispersion) )

debug_flag = 0
  
 print, 'OPTIMAL_ORDER_TRACING: Tracing the orders. Please wait ....'
 for ord_idx = 0L, n_elements(templates.middle )-1 do begin   
                     
       print, 'OPTIMAL_ORDER_TRACING: Tracing  indexed order: ' +strt(ord_idx)     
        i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                           ; Initial Template
        i_swath =     flat [ *,  templates.middle[ord_idx]  -slicer_width : templates.middle[ord_idx] + slicer_width-1 ]  ; Initial Swath        
        order_ys[ ord_idx,middle_column] = templates.middle[ord_idx] ; Insert value for  the middle
   

        prev_local_row =slicer_width
        
        ; >> Left side of Order
        ;----------------------.
       
        FOR x = middle_column, 1,-1  DO BEGIN
          back_x = x-1          
          
          ;  > Returns Y value to store in order_ys. This Y value is such that when add + 6 and -6 will give back the order perfectly       
          local_row= cross_correlate_op( i_swath,i_template , back_x, scipy_signal, prev_local_row = prev_local_row , idx_order=ord_idx,  debug =debug )   
          ; Returns local_row wrt to i_swath
          
          sz_swath= size(i_swath)   ; Make sure swath is even at all times
          order_ys[ord_idx,back_X] = ( order_ys[ord_idx,back_X+1] -  round(sz_swath[2]/2.0 ) )  + local_row
                                     ; Initial point of the swath used for the present iteration
  
          ;  > Define new i_swath for the next iteration
          i_swath = flat [ *,  order_ys[ord_idx,back_X] -slicer_width  : order_ys[ord_idx,back_X] +slicer_width-1  ]
          

          ;  > Tricky . Better to define with we already got cause if ther is a new swath then the peak had to shift along with the new swath
          ;  If section went up we want to substract if section went down we want to add
          prev_local_row = local_row  -(order_ys[ord_idx,back_X] - order_ys[ord_idx,back_X+1] ); Problem is that if I move swath ref then prev local will shouls be moved as well
          

        ENDFOR
       
        ; >> Right side of Order
        ;-----------------------

        i_template =  flat [ middle_column , templates.down[ord_idx]  : templates.up[ord_idx] ]                        ; Define Section
        i_swath =     flat [ *,  templates.middle[ord_idx] -slicer_width : templates.middle[ord_idx] + slicer_width-1   ]  ; Define Swath
        prev_local_row =  slicer_width       
        
        FOR x=middle_column, n_columns-2  DO BEGIN       
          ; Same Idea as for Left side
          forward_x = x+1
          local_row= cross_correlate_op( i_swath,i_template , forward_x, scipy_signal, prev_local_row=prev_local_row, idx_order=ord_idx , debug =debug )
          sz_swath= size(i_swath)
          
          order_ys[ord_idx,forward_x] = (order_ys[ord_idx,forward_x-1] - round(sz_swath[2]/2.0 ) ) + local_row     
          
          ;Seems to be giving issues for no apparent reason. We force the last values assuming that the 
          ; the problem does not happend often and also aware that this might screw the template depending how far along the
          ;the order is present.
          
          low = order_ys[ord_idx,forward_x] -slicer_width > 0
          high = order_ys[ord_idx,forward_x] +slicer_width-1   < n_rows
          if high eq 1366 then high -=1
          i_swath = flat [ *, low : high ]          
          prev_local_row = local_row  -(order_ys[ord_idx,forward_x] - order_ys[ord_idx,forward_x-1] )
        ENDFOR
        
        
        if  debug gt 3 then  begin
           
           if flag eq 0 then  p=image(flat  ) else debug_flag =1
           
           
           bellow = round(reform(order_ys[ord_idx,*])) -6
           above =round( reform( order_ys[ord_idx,*])) +6  ; +6 just for plotting since the plot will draw the line at the beginning of the plot
           
           p=plot( bellow, color='red' ,/overplot)
           p=plot( above, color='red' ,/overplot)
           
           ;stop, 'plotted order :  '+strt(ord_idx)
           
        endif
 endfor
 
 
;  debug = 1
if debug gt 0 then begin       
    p=plot(order_ys[0, *] )    
    for ord_idx = 1L, n_elements(templates.middle )-1 do begin
        p=plot( order_ys[ord_idx, *] , /overplot )        
    endfor
    stop, 'look at the plot '
endif

  return, order_ys ; Order Coefficients
end
