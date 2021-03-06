;+
; Set the order of the polynomial to be fit as a baseline
;
; @param order {in}{required}{type=integer} The order of polynomial
; to fit. 
;
; @examples
; Get some data, set some regions, fit a 2nd order polynomial
; <pre>
; getrec,20
; nregion,[100,500,700,1000,1600,2000]
; nfit,2
; baseline
; </pre>
;
; @version $Id: nfit.pro,v 1.3 2005/05/30 04:03:43 bgarwood Exp $
;-

pro nfit, order
    compile_opt idl2
    if (n_params() ne 1 ) then begin
        message, 'Usage: nfit, n',/info
        return
    endif
    if (size(order, /type) ne 3 and size(order,/type) ne 2) then begin
        message, 'Polynomial order must be an integer',/info
        return
    endif
    !g.nfit = order
end

    

