FUNCTION ExWidWinMouseMotionHandler, oWin, x, y, KeyMods
  state = oWin.uValue
  IF (~ISA(state.plot)) then return, 1

  ; Convert from screen coordinates to data coordinates.
  xy = state.plot.ConvertCoord(x, y, /DEVICE, /TO_DATA)

  ; "Snap" the location to the nearest plot point.
  xy = state.plot.GetValueAtLocation(xy[0])

  ; Update the crosshair location and the label
  state.crosshair.LOCATION = xy
  probe = STRING(xy[0:1], FORMAT='(2F9.2)')
  WIDGET_CONTROL, state.label, SET_VALUE=probe

  return, 1 ; Perform default event handling
END



PRO ExWidgetWindowEvents_event, event
  ; Be sure to process the internal window event first.
  ; This handles selection, translation, rotation, etc.
  w = WIDGET_EVENT(/NOWAIT)

  CASE TAG_NAMES(event, /STRUCTURE_NAME) of
    'WIDGET_BASE': BEGIN

      ; Handle base resize events. Retrieve our cached padding,
      ; and our new size.
      WIDGET_CONTROL, event.id, GET_UVALUE=pad, TLB_GET_SIZE=newSize
      wDraw = WIDGET_INFO(event.top, FIND_BY_UNAME='ex_widwin_window')

      ; Change the draw widget to match the new size, minus padding.
      xy = newSize - pad

      WIDGET_CONTROL, wDraw, DRAW_XSIZE=xy[0], DRAW_YSIZE=xy[1], SCR_XSIZE=xy[0], SCR_YSIZE=xy[1]
    END
    ELSE: ; do nothing
  ENDCASE

END


PRO ExWidgetWindowEvents

  ; Create the widgets (start unmapped)
  wBase = WIDGET_BASE(/COLUMN, /TLB_SIZE_EVENTS, MAP=0)

  wDraw = WIDGET_WINDOW(wBase, UNAME='ex_widwin_window',   MOUSE_MOTION_HANDLER='ExWidWinMouseMotionHandler',  X_SCROLL_SIZE=640, Y_SCROLL_SIZE=512)

  w1 = WIDGET_LABEL(wBase, /ALIGN_LEFT, /DYNAMIC)

  WIDGET_CONTROL, wBase, /REALIZE

  ; Cache the padding between the base and the draw
  WIDGET_CONTROL, wBase, TLB_GET_SIZE=basesize
  padding = basesize - [640, 512]
  WIDGET_CONTROL, wBase, SET_UVALUE=padding

  ; Retrieve the newly-created Window object.
  WIDGET_CONTROL, wDraw, GET_VALUE=win

  ; Make sure this is the current window
  win.Select

  p = PLOT(/TEST, /CURRENT)

  ; Change crosshair to "manual" mode and set some properties.
  c = p.CROSSHAIR
  c.STYLE = 'Manual'
  c.COLOR = 'Red'
  c.LINESTYLE = '-'

  ; Cache the graphics references for the event handlers
  win.uValue = {plot:p, label:w1, crosshair: c}

  ; Draw the widgets and start the event processing
  WIDGET_CONTROL, wBase, /MAP
  XMANAGER, 'ExWidgetWindowEvents', wBase, /NO_BLOCK
END