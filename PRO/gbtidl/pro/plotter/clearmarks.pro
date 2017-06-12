;+
; Clear all marks (+ plus associated text) from the plotter.
;
; @keyword noshow {in}{optional}{type=boolean} Don't immediately
; update the plotter..  This is useful if you are stringing several
; plotter calls together.  It keeps the plotter from updating after each
; call.
;
; @version $Id: clearmarks.pro,v 1.3 2005/10/28 20:33:19 bgarwood Exp $
;-
pro clearmarks, noshow=noshow
    common gbtplot_common,mystate,xarray
    compile_opt idl2

    mystate.nmarkers = 0
    if not keyword_set(noshow) then reshow
end
