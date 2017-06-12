;+
; function returns the sum of the set of orthogonal polynomials
; described by the polyfit argument evaluated at x.;
;
; <p>This code came from Tom Bania's GBT_IDL work.  Local
; modifications include:
; <UL>
; <LI> Documentation modified for use by idldoc
; <LI> Array syntax changed to [] and compile_opt idl2 used.
; <LI> Indententation used to improve readability.
; <LI> Unnecessary code removed.
; <LI> Some argument checks added.
; <LI> Changed name of cfit argument to polyfit to avoid confusion
; with cfit argument of ortho_fit, which is NOT the same thing.
; </UL>
;
; @param x {in}{required} The x-values to use in evaluating the polynomials.
; @param polyfit {in}{required}{type=2D array} The array describing
; the polynomials.  Typically this will be the return value from <a
; href="ortho_fit.html">ortho_fit</a>.  The
; dimensionality is [4,(nfit+1)]
; <UL>
; <LI> polyfit(3,m) gives the weighting coefficient for the m-th orthogonal polynomial
; <LI> polyfit(0:2,m) for m=2 are the recursion coefficients for 
; <pre> 
; p^(m) = x*p^(m-1)*polyfit(2,m) + p^(m-1)*polyfit(1,m) + p^(m-2)*polyfit(0,m) 
; </pre>
; <LI> polyfit(0,0) is the value of the constant term
; <pre>
; p^(1) = polyfit(1,1)*x + polyfit(0,1)
; </pre>
; </UL>
;
; @returns Array giving the evaluated polyfit at x.
;
; @examples
;   Fit a 7-th order polynomial to the data in !g.s[0] and subtract
; the result of the fit from that data, replacing the data in !g.s[0].
; <pre>
;    yy = *(!g.s[0].data_ptr)
;    xx = dindgen(n_elements(yy))
;    poly = ortho_fit(xx, yy, 7, cfit, rms)
;    thefit = ortho_poly(xx,poly)
;    *(!g.s[0].data_ptr) = *(!g.s[0].data_ptr) - thefit
; </pre>
; 
;
; @version $Id: ortho_poly.pro,v 1.2 2004/12/08 03:50:36 bgarwood Exp $
;-
function ortho_poly,x,polyfit
    compile_opt idl2

    n = n_elements(x)
    b = size(polyfit)
    if (b[0] ne 2) then begin
	if (b[0] eq 1 and b[1] eq 4) then begin
            ; most likely its nfit=0, recast
            polyfit = reform(polyfit, 4, 1)
            b = size(polyfit)
        endif else begin
            message, 'polyfit must be a 2-D array'
        endelse
    endif
    if (b[1] ne 4) then message, 'First dimension of polyfit must have 4 elements'

    nfit = b[2]-1
    if (nfit lt 0) then message, 'Second dimension of polyfit must have more than 1 element'

    fit = dblarr(n)
    pnm1 = dblarr(n)
    pn = pnm1
    pnp1 = pnm1
    pnm1[0:n-1] = polyfit[0,0]
    fit = polyfit[3,0]*pnm1
    if (nfit eq 0) then begin
        return, fit
    endif

    pn = x*polyfit[1,1] + polyfit[0,1]
    fit = fit + polyfit[3,1]*pn

    for m=2,nfit do begin
        pnp1 = pn*(x*polyfit[2,m] + polyfit[1,m]) + pnm1*polyfit[0,m]
        fit = fit + polyfit[3,m]*pnp1
        pnm1 = pn
        pn = pnp1
    endfor

    return,fit
end
