
;+
; :Description:
;
;-
;pro calculate_snr, wavelength_1, wavelength_2
;
;
;PRINT, ' GOT INSIDE THE CALCULATE SNR METHOD. THE 2 wavelengths are : '  
;print,   wavelength_1
;print,   wavelength_2
;
;
;end
;



FUNCTION RBBoxMouseDown2, oWin, x, y, iButton, KeyMods, nClicks
  
  state = oWin.UVALUE
  
   IF state.snr_status EQ 1 THEN BEGIN
         oWin_plot = oWin.UVALUE.plot
         snr_xy = oWin_plot.ConvertCoord(x, y, /DEVICE, /TO_DATA)
    
    
    
         state.snr_x1= snr_xy[0]    ; Saves only the x component to be used later one
    
         state.x0 = x
         state.y0 = y
         ;print ,  'The positions when down are  x: ' +string(xy[0]) +'  and in y :' +string(xy[1])
         state.buttonDown = 1
         state.poly.HIDE = 0
         state.poly.SetData, [0,0,0], [0,0,0]
         state.poly.LINESTYLE='--'
         oWin.UVALUE=state
         RETURN, 0 ; Skip default event handling
    
   ENDIF ELSE  begin
        oWin.SELECT
        RETURN, 1
   ENDELSE
  
  
  

END

FUNCTION RBBoxMouseMotion2, oWin, x, y, KeyMods
  
  state = oWin.UVALUE

  IF state.snr_status EQ 1 THEN BEGIN
      IF state.buttonDown then begin
        x0 = state.x0
        y0 = state.y0
        xVector=[x0,x0,x,x,x0]
        yVector=[y0,y,y,y0,y0]
        xy = state.poly.ConvertCoord(xVector, yVector, /DEVICE, /TO_NORMAL)
  
        state.poly.SetData, REFORM(xy[0,*]), REFORM(xy[1,*])
      ENDIF
      RETURN, 0 ; Skip default event handling
    
    
  ENDIF ELSE begin
        oWin.SELECT
        RETURN, 1
  ENDELSE
  
END



FUNCTION RBBoxMouseUp2, oWin, x, y, iButton
  state = oWin.uvalue
  
  IF state.snr_status EQ 1 THEN BEGIN
        bwindow  =  oWin
        IF (~state.buttonDown) THEN RETURN, 0
    
    
        ; User to fin the SNR
        oWin_plot = oWin.UVALUE.plot
        snr_xy = oWin_plot.ConvertCoord(x, y, /DEVICE, /TO_DATA)
        state.snr_x2= snr_xy[0]    ; Saves only the x component to be used later one
    
    
        ;calculate_snr,  state.snr_x1, state.snr_x2
    
    
    
        x0 = state.x0
        y0 = state.y0
        ;print ,  'The positions when up are  x: ' +string(x) + '  and in y :' +string(y)
        xVector=[x0,x0,x,x,x0]
        yVector=[y0,y,y,y0,y0]
        xy = state.poly.ConvertCoord(xVector, yVector, /DEVICE, /TO_NORMAL)
    
        state.poly.SetData, REFORM(xy[0,*]), REFORM(xy[1,*])
    
        state.poly.LINESTYLE='-'
        state.buttonDown=0
        state.snr_status = 0  ; NOT ALLOWING FOR SNR WITHIN  EVENT HANDLERS
        oWin.uvalue=state
    
        ; Clear the current selections
        oSelect = oWin.GetSelect()
    
        ;  print, 'what gets on oSelect'
        ;  print, oSelect
    
        
        
    
        FOREACH oVis, oSelect do oVis.Select, /UNSELECT
    
        ; Do a hit test and select new items.
        oVisList = oWin.HitTest(x0+(x-x0)/2, y0+(y-y0)/2,    DIMENSIONS=ABS([x-x0, y-y0]) > 10)
    
        ;  print, 'what gets returned as oVisList'
        ;  print, oVisList
        FOREACH vis, oVisList do begin
          ;print, vis
    
          if vis eq state.poly then  begin
            state.poly.HIDE = 1
            ;        bwindow.MOUSE_DOWN_HANDLER= !NULL
            ;        bwindow.MOUSE_UP_HANDLER=!NULL
            ;        bwindow.MOUSE_MOTION_HANDLER=!NULL
          endif
          if vis ne state.poly then vis.Select, /ADD ; Selects the current  plot to be on top
        ENDFOREACH
        
        
        snr= find_ref_points( state.data, state.snr_x1, state.snr_x2 ) 
        
        
        WIDGET_CONTROL, state.label, SET_VALUE=" SNR = "+STRING(snr)+ " : 1"
        
        RETURN, 1 ; Skip default event handling
        
   ENDIF ELSE begin
        oWin.SELECT
        RETURN, 1
  ENDELSE
  
END



;pro find_snr, p, wDraw 
;  ;This procedure finds the signal to noise ratio based on the next to click events
;  ;given by the user.
;  
;  PRINT, '   *********** Calculating Signal To Noise Ratio ***********'
;  
;  state = oWin.uvalue
;  state.snr_status = 1  ; allowing to register the mouse events
;  
;   
;  state.snr_status = 0  ; NOT allowing to run the mouse events 
;  
;  
;  
;  
;  ;p.color='red'
;;  counter=0
;;  state = oWin.UVALUE
;;  print, 'these are the x and y : '
;;  
;;  state.x0 = x
;;  state.y0 = y
;;  while (counter  lt 2) do begin
;;    cursor, xcur, ycur, /down
;;    print,'Selected  >>  X-coord : '+ string(xcur) + '     Y-coord :  ' +string( ycur)
;;    counter= counter+1
;;  endwhile
;
;  
;  print, 'jaja '
;
;  
;end


PRO PLOT_WIDGET_DOC_EVENT, event ; get called once a button is pressed 

  CASE TAG_NAMES(event, /STRUCTURE_NAME) OF
    'WIDGET_BUTTON': BEGIN
      WIDGET_CONTROL, event.id, GET_UVALUE = event_UV

      ; Retrieve the Widget Window
      wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME = 'DRAW')
      WIDGET_CONTROL, wDraw, GET_VALUE = graphicWin

      ; Retrieve the plot with the NAME
      ; provided on plot creation
      p = graphicWin['PLOT_WIDGET_DOC']
      graphicWin.SELECT
      
;      print, 'this is the value of p '
;      print, p
      CASE event_UV OF
        'DONE': WIDGET_CONTROL, event.top, /DESTROY
        'SNR': BEGIN 
               PRINT, 'GOT CLICK IN HERE'
               state = graphicWin.uvalue
               
;               PRINT, state
               
               
               
               p.window.MOUSE_DOWN_HANDLER='RBBoxMouseDown2'
               p.window.MOUSE_UP_HANDLER='RBBoxMouseUp2'
               p.window.MOUSE_MOTION_HANDLER='RBBoxMouseMotion2'
               state.snr_status = 1
               graphicWin.uvalue=state
               graphicWin.SELECT
          
               
               END  ; find_snr, plot, wDraw  ;p.color='red'
               
        ;'BLUE': p.color='blue'
        ELSE: ; do nothing
      ENDCASE
    END

    'WIDGET_BASE': begin
      ; Handle base resize events. Retrieve our cached padding,
      ; and our new size.
      WIDGET_CONTROL, event.id, GET_UVALUE=pad, TLB_GET_SIZE=newSize
      wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME='DRAW')
      ; Change the draw widget to match the new size, minus padding.
      xy = newSize - pad
      WIDGET_CONTROL, wDraw, DRAW_XSIZE=xy[0], DRAW_YSIZE=xy[1],   SCR_XSIZE=xy[0], SCR_YSIZE=xy[1]
    end

    ELSE: ; do nothing
  ENDCASE
END



function find_ref_points, data_cube, wavelength_a, wavelength_b

;  wavelength_a= 5198.0
;  wavelength_b= 5199.5
  
  
  sz  = size(data_cube)
  num_orders=sz[3]
  
  for order = 0L, num_orders-1 do begin
    
    
    
    if (  (data_cube[0,0,order]  lt wavelength_a )  AND (data_cube[0,-1,order] gt wavelength_b)  ) then begin
      print,  'This is the indexed order : ' +string(order)
       diff_wave_a = abs( reform( data_cube[0,*,order] ) - wavelength_a )
       diff_wave_b = abs( reform( data_cube[0,*,order] ) - wavelength_b )
       
       
       print, ' This size should be 4112 '
       print, sizE(diff_wave_a)
       min_wave_a = MIN( diff_wave_a, min_subscript_lower)
       min_wave_b = MIN( diff_wave_b, min_subscript_upper)
       found_order= order
       
       wa_index = min_subscript_lower
       wa=data_cube[0,min_subscript_lower,order]
       
       wb_index = min_subscript_upper
       wb= data_cube[0,min_subscript_upper,order]
       break 
       
       
       
       ;print, 'found in order: '+string(order)+ ' wavelength range : ['+string(data_cube[0,min_subscript_lower,order])+ ' , '+ string(data_cube[0,min_subscript_upper,order])
       
      

    endif 
    


  endfor
  
  
  snr= DER_SNR( data_cube[1,wa_index:wb_index,found_order ] )
  print,'The signal to noise ratio found is : ' +string(snr)
  
;  intentities  = data_cube[1,wa_index:wb_index,found_order ]
;  wavelengths =  data_cube[0,wa_index:wb_index,found_order ]
;  p0 =plot(wavelengths, intentities,color= 'blue', SYM_FILLED = 2 )

  

  return, snr
end


PRO PLOT_WIDGET_DOC
  base1 = WIDGET_BASE(/COLUMN, TITLE='Widget Window example',      /TLB_SIZE_EVENTS)

  wDraw = WIDGET_WINDOW(base1, UVALUE='draw', UNAME='DRAW')   ; This will contain the actual plot. Refer to this UNAME to change 

  ; Create the base for the button:
  base2 = WIDGET_BASE(base1, /ROW, /ALIGN_CENTER )

  ; Create the action buttons.
  snr = WIDGET_BUTTON(base2, VALUE='Find SNR', UVALUE = 'SNR') ; The UVALUE has to match with a case within the PLOT_WIDGET_DOC_EVENT
  ;blueline = WIDGET_BUTTON(base2, VALUE='Blue line', UVALUE='BLUE')
  done = WIDGET_BUTTON(base2, VALUE = 'Done', UVALUE = 'DONE')
  
  w1 = WIDGET_LABEL(base2, /ALIGN_LEFT, /DYNAMIC)

  ; Realize the widget (i.e., display it on screen).
  WIDGET_CONTROL, base1, /REALIZE

  ; Register the widget with the XMANAGER, leaving the IDL command
  ; line active.
  XMANAGER, 'PLOT_WIDGET_DOC', base1, /NO_BLOCK

  ; Cache the padding between the base and the draw
  WIDGET_CONTROL, base1, TLB_GET_SIZE=basesize
  xpad = basesize[0] - 640
  ypad = basesize[1] - 512
  WIDGET_CONTROL, base1, SET_UVALUE=[xpad,ypad]

  ; Retrieve the newly-created Window object.
  WIDGET_CONTROL, wDraw, GET_VALUE = graphicWin

  graphicWin.SELECT

;  PRECIP=[0.5,0.7,1.2,1.8,2.5,1.6,1.9,1.5,1.2,1.0,0.8,0.6]
;  TEMP=[30, 34, 38, 47, 57, 67, 73, 71, 63, 52, 39, 33]
;  DAY=FINDGEN(12) * 30 + 15

  ; Plot #1: In position #1 on the grid defined by LAYOUT
  
  
  ;achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\comparing master files\master_stellars_171218\achi171218.mstr_stellar_40.fits
  achiFileName=  'C:\Users\mrstu\idlworkspace_yalecalibration\chiron\debugging\weighting_schemas\wmchi210208.1121_100_mean.fits'
  cubeachi=readfits(achiFileName)
  
   ;value = find_ref_points (cubeachi )
  p=plot(cubeachi[0,*,*],cubeachi[1,*,*] , NAME = 'Mean', /CURRENT , TITLE = ' 210128' )
  
  
;  p=PLOT(DAY, PRECIP, NAME = 'PLOT_WIDGET_DOC', SYMBOL='o',  LINESTYLE='--', TITLE = 'Precipitation',   YTITLE = 'Inches', XTITLE= 'Day of Year', $
;    THICK=2, /CURRENT)
    
  poly = POLYGON([0,0,0],[0,0,0], /DEVICE,   LINESTYLE='--', /HIDE,  FILL_TRANSPARENCY=90, FILL_BACKGROUND = 1, FILL_COLOR='red')
  
;  c = p.CROSSHAIR
;  c.STYLE = 'Manual'
;  c.COLOR = 'Red'
;  c.LINESTYLE = '-'
  
  p.window.UVALUE={x0:0, y0:0, buttonDown:0L,   poly: poly, plot: p, snr_x1: 0d, snr_x2: 0d, snr_status: 0, label:w1, data :cubeachi}

 
  
  graphicWin.SELECT, /ADD
    
    
    
END