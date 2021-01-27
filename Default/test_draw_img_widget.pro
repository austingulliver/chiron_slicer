

; Create the widgets.
;wBase = WIDGET_BASE(/COLUMN)  ; A container wiht the X OPTION AT TOP RIGHT CORner  
;
;wDraw = WIDGET_WINDOW(wBase,  X_SCROLL_SIZE=1000, Y_SCROLL_SIZE=400) ; actual space within the wBase where the plot is going to be 
;
;WIDGET_CONTROL, wBase, /REALIZE  ; At this point we already outputting the widget control 
;
;;; Retrieve the newly-created Window object.
;WIDGET_CONTROL, wDraw, GET_VALUE=oWin  ; retrieving a ref specifically to the 'within' widget 
;
;print, oWin
;;; Make sure this is the current window
;oWin.Select
;;
;p = PLOT(/TEST, /CURRENT, /FILL_BACKGROUND)


print, 0d

end