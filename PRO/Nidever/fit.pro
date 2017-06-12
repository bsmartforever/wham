function fit,array,ord,x,coef,sigma,scut=scut,gd=gd

;+
; This program fits a polynomial to a set of points
; 
; INPUT
;  array     Array of points to fit
;  ord       The order of the polynomial (ord=1 is a line)
;  x         Optional array of x-values
;  scut=scut Optional sigma cut.
;
; OUTPUT
;  fit       The fit to the data points
;  coef      The coefficients of the best fit polynomial
;  sigma     The error in the coefficients
;  gd=gd     The good points if sigma cut set.
;
; Written by D.Nidever 2002?
;-

n=n_elements(array)
if not keyword_set(x) then x=dindgen(n)
coef=poly_fit(x,array,ord,yfit,yband,sigma)
fit=poly(x,coef)

if keyword_set(cut) then begin
  resid = array-fit
  sig = stdev(resid)
  mn = mean(resid)
  gd = where(resid ge mn-scut*sig or resid le mn+scut*sig,ngd)

  ; Cutting some bad points
  if (ngd lt n) then begin
    coef = poly_fit(x(gd),array(gd),ord,yfit,yband,sigma)
    fit = poly(x,coef)
  endif
endif

return,fit
end
