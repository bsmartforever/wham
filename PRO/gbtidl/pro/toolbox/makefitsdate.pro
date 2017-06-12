;+
; Make a FITS date string from a modified Julian Date.
;
; @param mjd {in}{required}{type=double} Modified Julian Date, in
; days.
;
; @keyword precision {in}{optional}{type=integer} Number of digits
; after the decimal point in the seconds field of the output FITS
; date.  If precision is 0 (the default) no decimal point is used.
;
; @returns a FITS date of the form YYYY-MM-DDThh:mm:ss[.sss].
;
; @uses <a href="paddedstring.html">paddedstring</a>
; 
; @version $Id: makefitsdate.pro,v 1.3 2004/11/17 21:47:51 bgarwood Exp $
;-
function makefitsdate,mjd,precision=precision
    compile_opt idl2

    ; convert mjd to jd and then to individual fields

    caldat,(mjd+2400000.5),month,day,year,hour,min,second

    ystr = string(year,format='(i4)')
    monstr = paddedstring(month)
    dstr = paddedstring(day)
    hstr = paddedstring(hour)
    minstr = paddedstring(min)
    sstr = paddedstring(second,precision=precision)

    return, string(ystr,monstr,dstr,hstr,minstr,sstr,format='(a,"-",a,"-",a,"T",a,":",a,":",a)')

end