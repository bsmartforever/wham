;+
; Function to calculate the shift, in channels, necessary to align in 
; frequency the primary data container with the data container
; template in an ongoing accumulation.  
;
; <p>You can use an alternate data container by setting buffer.  You
; can use an alternate global accumulation buffer by setting accumnum.
;
; <p>If the frame is not set, the one implied by the data header is 
; used.  Use <a href="xshift.html">xshift</a> to align using the current settings of the 
; plotter's x-axis.
;
; @param accumnum {in}{type=integer}{default=0} accum buffer to use.
; Defaults to the primary buffer, 0.  There are 4 buffers total so
; this value must be between 0 and 3, inclusive.
;
; @keyword buffer {in}{optional}{type=integer}{default=0} The global 
; buffer that will eventually be shifted.  Defaults to the primary
; data container (buffer 0).
;
; @keyword frame {in}{optional}{type=string}  The reference frame to
; use.  If not supplied, the value implied by the last 4 characters of
; the velocity_definition in the ongoing accumulation will be
; used.  See <a href="../toolbox/frame_velocity.html">frame_velocity</a> for a
; full list of supported reference frames.
;
; @returns shift, in channels, to be used as argument to shift.
; Returns 0.0 on failure.
;
; @examples
; <pre>
; getps,30
; accum             ; accum first spectrum, no alignment needed yet
; getps,31
; fs = fshift()     ; determine the shift to align scan 31 with scan 30
; gshift,fs         ; apply the shift to scan 31
; accum             ; and add the result to the accumulator
; getps, 32
; gshift, fshift()  ; all in one line, shift scan 32 to align with scan 30
; accum
; ave
; </pre>
;
; @uses <a href="../toolbox/dcfshift.html">dcfshift</a>
;
; @version $Id: fshift.pro,v 1.9 2006/05/15 21:22:06 bgarwood Exp $
;-
function fshift, accumnum, buffer=buffer, frame=frame
    compile_opt idl2

    on_error, 2

    if not !g.line then begin
        message,'FSHIFT only works in line mode, sorry.',/info
        return,0.0
    endif

    if n_elements(accumnum) eq 0 then accumnum = 0

    if (accumnum lt 0 or accumnum gt 3) then begin
        message,'accumnum must be in the range 0 to 3',/info
        return,0.0
    endif

    if n_elements(buffer) eq 0 then buffer=0

    if (buffer lt 0 or buffer ge n_elements(!g.s)) then begin
        message,string(n_elements(!g.s),format='("buffer must be >= 0 and < ",i2)'),/info
        return,0.0
    endif

    return, dcfshift(!g.accumbuf[accumnum], !g.s[buffer], frame=frame)
    
end
