;+
; Removes all annotations from the current plot.
;
; @keyword noshow {in}{optional}{type=boolean} If set, don't call reshow
; here.
;
; @version $Id: clearannotations.pro,v 1.2 2005/06/14 17:28:47 bgarwood Exp $
;-
pro clearannotations, noshow=noshow
    common gbtplot_common,mystate,xarray

    mystate.n_annotations = 0
    if (not keyword_set(noshow)) then reshow
    return
end
