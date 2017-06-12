;+
;; COS_LSF.pro - return COS LSF at nearest tabulated wavelength value
;;               based on data at 
;; http://www.stsci.edu/hst/cos/performance/spectral_resolution/ 
;; 
;; inputs:
;;   lambda - central wavelength for LSF 
;; 
;; outputs: 
;;   xarray - output vector of wavelengths (in angstroms or pixels)
;;
;; options:
;;   /pixel - dispersion units in pixels, else in angstroms (default) 
;;   chan= - specify data channel (g130m,g160m,g140l).
;;
;; Example:
;;	y_ip=COS_LSF(1550.,x_ip)
;;  plot,x_ip,y_ip
;;
;; C. Danforth 10/12/09 --> 12/2/09
;-

function cos_lsf,lambda,xarray,chan=chan,pixel=pixel

;; restore IDL file
restore,'$HOME/PRO/STools/COS/cos_lsf.idl'

;; assume medium resolution grating and pick according to lambda
if not keyword_set(chan) then begin
  if lambda lt 1800. then begin ; assume FUV
    if lambda gt 1450. then chan='g160m' else chan='g130m'
  endif else chan='g225m' ; NUV LSF is the same for each grating, so pick middle one 
endif 
chan=where(strtrim(strlowcase(chan),2) eq lsfchan)
chan=chan[0]

;; pick nearest wavelength point (LSF varies slowly enough with lambda
;; that this should be good enough)
junk=min(abs(lsfwave[chan,*]-lambda),lamind)

lsf=transpose(lsf[chan,lamind,*])

if keyword_set(pixel) then xarray=lsfpix else xarray=lambda+lsfpix*0.001*lsfpixscale[chan]
  
return,lsf
end
