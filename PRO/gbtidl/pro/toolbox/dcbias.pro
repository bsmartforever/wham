;+
; This procedure adds a bias factor to a data container's data
;
; @param dc {in}{required}{type=data container} data container (spectrum or continuum)
; @param factor {in}{required}{type=float} bias factor
;
; @examples
; <pre>
;    get,index=1
;    a = data_new()
;    data_copy,!g.s[0],a
;    show
;    dcbias,a,25
;    show,a
; </pre>
;
; @version $Id: dcbias.pro,v 1.3 2005/05/29 22:44:28 bgarwood Exp $
;-

pro dcbias,dc,factor

    compile_opt idl2

    if (data_valid(dc) le 0) then begin
        message, "dcbias: invalid data structure",/info
        message, "Usage: dcbias, dc, factor",/info
        return
    endif

    *dc.data_ptr = *dc.data_ptr + factor

end
