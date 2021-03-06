;+
; Clears all baseline regions by resetting !g.nregion to 0.  Any
; region boxes displayed on the plotter are also erased.
;
; @examples
; <pre>
;   setregion                 ; interactively set a region
;   clearregion               ; clear them all 
;   nregion,[100,200,500,700] ; set them by hand
; </pre>
;
; @version $Id: clearregion.pro,v 1.2 2006/05/15 20:01:46 bgarwood Exp $
;-
pro clearregion
    !g.nregion = 0
    ; this unplots the regions but doesn't change the 
    ; state of plotter toggle 
    clearoplots, idstring='__showregion'
end
