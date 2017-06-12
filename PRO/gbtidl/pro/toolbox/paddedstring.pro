;+
; Format a number as a string, ensuring that it is padded with a
; leading zero so that there are always 2 digits before the optional
; decimal point.
;
; This could be generalized for use with numbers larger than 2 digits
; before the decimal point, but that is all I needed for now.
;
; @param number {in}{required}{type=numeric} The number to convert.
; @keyword precision {in}{optional}{type=integer} The number of
; characters after the decimal point.  If precision is 0 (the
; default), no decimal point appears in the returned value.
;
; @returns string representation of number at given precision.
;
; @version $Id: paddedstring.pro,v 1.3 2005/06/21 17:26:01 bgarwood Exp $
;-
function paddedstring, number, precision=precision
    compile_opt idl2

    if (not keyword_set(precision)) then precision = 0
    if (precision eq 0) then begin
        tmp = long(number)
        if (tmp lt 10) then begin
            result = string(tmp,format='("0",i1)')
        endif else begin
            result = string(tmp,format='(i2)')
        endelse
    endif else begin
        if (number gt 10.0) then begin
            width = precision + 3
            fmtstring = string(width,precision,format='("(f",i1,".",i1,")")')
        endif else begin
            width = precision + 2
            fmtstring = string('"','"',width,precision,format='("(",a1,"0",a1,",f",i1,".",i1,")")')
        endelse
        result = string(number,format=fmtstring)
    endelse

    return, result

end
