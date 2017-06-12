;+
; Internal function for handling resize events.
;
; @param x {in}{required}{type=integer} New x size.
; @param y {in}{required}{type=integer} New y size.
;
; @private_file
;
; @version $Id: handle_resize.pro,v 1.4 2005/06/14 17:28:47 bgarwood Exp $
;-
pro handle_resize, x, y
    compile_opt idl2
    common gbtplot_common,mystate,xarray

    mystate.xsize=x-mystate.xpad
    mystate.ysize=y-mystate.ypad
    widget_control,mystate.plot1,draw_xsize=mystate.xsize,draw_ysize=mystate.ysize
    wdelete, mystate.pix_id
    window,/free,/pixmap,xsize=mystate.xsize,ysize=mystate.ysize
    mystate.pix_id = !d.window
    reshow
end
