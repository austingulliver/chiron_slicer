FUNCTION RBBoxMouseDown, oWin, $
  x, y, iButton, KeyMods, nClicks

  state = oWin.UVALUE
  state.x0 = x
  state.y0 = y
  print ,  'The positions when down are  x: ' +string(x) +'  and in y :' +string(y)
  state.buttonDown = 1
  state.poly.HIDE = 0
  state.poly.SetData, [0,0,0], [0,0,0]
  state.poly.LINESTYLE='--'
  oWin.UVALUE=state
  RETURN, 0 ; Skip default event handling

END

FUNCTION RBBoxMouseMotion, oWin, x, y, KeyMods
  state = oWin.uvalue
  IF state.buttonDown then begin
    x0 = state.x0
    y0 = state.y0
    xVector=[x0,x0,x,x,x0]
    yVector=[y0,y,y,y0,y0]
    xy = state.poly.ConvertCoord(xVector, yVector, /DEVICE, /TO_NORMAL)
    
    state.poly.SetData, REFORM(xy[0,*]), REFORM(xy[1,*])
  ENDIF
  RETURN, 0 ; Skip default event handling
END

FUNCTION RBBoxMouseUp, oWin, x, y, iButton
  state = oWin.uvalue
  IF (~state.buttonDown) THEN RETURN, 0
  x0 = state.x0
  y0 = state.y0
  ;print ,  'The positions when up are  x: ' +string(x) + '  and in y :' +string(y)
  xVector=[x0,x0,x,x,x0]
  yVector=[y0,y,y,y0,y0]
  xy = state.poly.ConvertCoord(xVector, yVector, /DEVICE, /TO_NORMAL)
   print ,  'The positions when up are  x and y : '
   print, xy
   state.poly.SetData, REFORM(xy[0,*]), REFORM(xy[1,*])
  
  state.poly.LINESTYLE='-'
  state.buttonDown=0
  oWin.uvalue=state

  ; Clear the current selections
  oSelect = oWin.GetSelect()
  FOREACH oVis, oSelect do oVis.Select, /UNSELECT

  ; Do a hit test and select new items.
  oVisList = oWin.HitTest(x0+(x-x0)/2, y0+(y-y0)/2, $
    DIMENSIONS=ABS([x-x0, y-y0]) > 10)
  FOREACH vis, oVisList do begin
    if vis ne state.poly then vis.Select, /ADD
  ENDFOREACH

  RETURN, 1 ; Skip default event handling
END

PRO RBBoxEventsTest
  x = Findgen(200)
  y = Sin(x*2*!PI/25.0)*Exp(-0.01*x)
  p = PLOT(x, y, TITLE='Click, hold mouse down and drag, release to draw box')
  ; Add a hidden polygon for the rubber-band box.
  poly = POLYGON([0,0,0],[0,0,0], /DEVICE,   LINESTYLE='--', /HIDE,  FILL_TRANSPARENCY=90, FILL_BACKGROUND = 1, FILL_COLOR='red')
  
  p.window.UVALUE={x0:0, y0:0, buttonDown:0L,   poly: poly}
  
  p.window.MOUSE_DOWN_HANDLER='RBBoxMouseDown'
  p.window.MOUSE_UP_HANDLER='RBBoxMouseUp'
  p.window.MOUSE_MOTION_HANDLER='RBBoxMouseMotion'

END