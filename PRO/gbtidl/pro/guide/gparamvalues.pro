;+
; Procedure to put values into the !g.gauss.params container.
;
; @param gauss_index {in}{required}{type=long} Which gaussian (0 to
; ngauss-1) to set the parameters of.
; @param values {in}{required}{type=double array} Array of values to
; assign to the indicated gaussian (must have 3 elements).  Order is
; height, center, and full width at half maximum.  The units are
; always channels.
;
; @examples
; <pre>
;   ngauss, 2    ; set up for 2 gaussians
;   gparamvalues, 0, [1020.0, 1.24, 12.4]
; </pre>
;
; @version $Id: gparamvalues.pro,v 1.5 2006/05/17 07:11:14 bgarwood Exp $
;-
pro gparamvalues, gauss_index, values

    if n_params() ne 2 then begin
        message,'Usage: gparamvalues, gauss_index, values',/info
        return
    endif
    if (n_elements(values) ne 3) then begin
        message,'values must havce 3 elements',/info
        return
    endif
    !g.gauss.params[0,gauss_index].value = values[0]
    !g.gauss.params[1,gauss_index].value = values[1]
    !g.gauss.params[2,gauss_index].value = values[2]

end    

