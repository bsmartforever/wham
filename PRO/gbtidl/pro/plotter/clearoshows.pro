;+
; Clear any overlain plots (data containers plotted with oshow).  This
; removes them from the plotters state and does a reshow.
;
; @version $Id: clearoshows.pro,v 1.3 2005/06/14 17:28:47 bgarwood Exp $
;-
pro clearoshows
    clearoshowslist
    reshow
end
