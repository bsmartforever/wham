;+
; This procedure toggles a flag to draw the plot in histogram
; mode. Use the /on or /off keywords to ensure that the histogram
; plotting is on or off (otherwise it simply toggles the state). 
;
; <p>If both /on and /off are used at the same time, an error message
; is printed and the state of the zero line is not changed.
;
; @examples
;   histogram
;
; @keyword on {in}{optional}{type=boolean} Turn histogram mode on.
; @keyword off {in}{optional}{type=boolean} Turn histogram mode off.
;
; @version $Id: histogram.pro,v 1.2 2006/05/13 04:33:26 bgarwood Exp $
;-

pro histogram, on=on, off=off
    compile_opt idl2
    
    newstate = !p.psym
    
    if keyword_set(on) and keyword_set(off) then begin
        message,'/on and /off can not be used at the same time',/info
        return
    endif

    newstate = !p.psym
    if keyword_set(on) then begin
        newstate = 10
    endif else begin
        if keyword_set(off) then begin
            newstate = 0
        endif else begin
            newstate = 10-!p.psym
        endelse
    endelse

    if newstate ne !p.psym then begin
        !p.psym = newstate
	reshow
    endif
end
