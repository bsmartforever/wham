;; Determine a significance level for a line given EW, wavelength, 
;; b-value, and S/N using Equations 4,7,9-11 of Keeney et al. (2012).
;;
;; INPUTS
;;   W      - observed equivalent width of line (mA)
;;   lambda - observed wavelength of line (A)
;;   b      - b-value (km/s)
;;   snpix  - S/N per pixel
;;   snx    - S/N measured at optimal resolution
;;
;; OPTIONAL PARAMETERS
;;   disp   - dispersion in mA/pix (defaults to G130M value for 
;;            lambda <= 1425 A and G160M value otherwise)
;;   xopt   - optimal integration width, in pixels
;;
;; CDanforth 12/30/11
;; B. Keeney 02/16/12

function cos_siglevel, w, lambda, b, snpix=snpix, snx=snx, disp=disp, xopt=xopt

if ((n_elements(snpix) eq 0) and (n_elements(snx) eq 0)) then begin
  print, 'SIGLEVEL: either SNPIX or SNX must be specified!!!'
  return, -1
endif

if (n_elements(disp) eq 0) then disp = (lambda le 1425) ? 9.97d : 12.23d

dx = 1d3*b*lambda / (2.998d5*disp)

xopt = 1.605*dx + 5.1d*dx^(-0.25d)
eta  = 0.15d + xopt^(0.37d)
fcx  = 0.743d - 0.185d*exp(-dx/11.6d)

siglevel = (n_elements(snx) gt 0) ? snx   * (W/disp) * fcx/xopt : $
                                    snpix * (W/disp) * eta*fcx/xopt

return, siglevel

end
