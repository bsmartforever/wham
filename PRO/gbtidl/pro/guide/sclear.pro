;+
; Clears one of the global accum buffers (frees the pointer, zeros
; values).
;
; @param accumnum {in}{optional}{type=integer}{default=0} the accum
; buffer to clear, in the range of 0 through 3.  Defaults to 0.
;
; @uses <a href="../toolbox/accumclear.html">accumclear</a>
;
; @version $Id: sclear.pro,v 1.5 2006/05/17 07:11:14 bgarwood Exp $
;-
PRO sclear, accumnum
    compile_opt idl2

    ; verify argument
    if n_elements(accumnum) eq 0 then accumnum = 0

    if (accumnum lt 0 or accumnum gt 3) then begin
        message,'accumnum must be in the range 0 to 3',/info
        return
    endif

    accumbuf = !g.accumbuf[accumnum]

    accumclear, accumbuf

    !g.accumbuf[accumnum] = accumbuf
END
