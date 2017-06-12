;+
; Clear all overlays (gbtoplot and oshow results) from the plotter.
; This combines calls to clearoplots and clearoshows in to one
; function.  Baseline region boxes are also cleared with this call.
;
; @version $Id: clearovers.pro,v 1.3 2006/03/01 19:52:11 bgarwood Exp $
;-
pro clearovers
    clearoplotslist
    clearoshowslist
    showregion,/off
    reshow
end
