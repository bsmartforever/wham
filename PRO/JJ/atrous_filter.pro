;+
; NAME:
;       ATROUS_FILTER
;
;
; PURPOSE:
;       Smooth a 1D spectrum using an a trous wavlet transform
;       See Starck et al. (1997, ApJ 482:1011-1020)
;           Starck et al. (2002, PASP 114:1051S)
;
; CALLING SEQUENCE:
;       new = atrous_filter(spec, noisemodel [,n_scale=, nsig=])
;  
;
; INPUTS:
;       SPEC: Spectrum to be smoothed
;
;       NOISEMODEL: Vector containing uncertainty in each pixel in
;            SPEC. If measurment uncertainties are Poisson, then 
;            NOISEMODEL = sqrt(SPEC)
;
; KEYWORD PARAMETERS:
;       N_SCALE: Number of scales in discrete wavelet transform. 
;                Cannot exceed alog(n_elements(spec))/alog(2) - 2
;
;       NSIG: Sigma threashold below which features are set to zero
;             Lower NSIG means more smoothing.
;
; OUTPUTS:
;       NEW: Smoothed spectrum
;
; DEPENDANCIES:
;       Uses FAN.pro:
;           http://astron.berkeley.edu/~johnjohn/idl.html#FAN
;       
; MODIFICATION HISTORY:
;       Written 25 April 2005 by JohnJohn
;
;-

function atrous_filter, spec, noiseModelin, m_support=Msupport $
  , n_scales=Nscales, Nsig=Nsig, ds=ds, test=test, smear=smear $
  , noise=sigjl

npix = n_elements(spec)
nnm = n_elements(noiseModelin)
if nnm ne npix and nnm ne 1 then begin
    message,'SPEC and NOISEMODEL must have same number of elements.',/io 
endif else begin
    if nnm eq 1 then $
      noiseModel = replicate(noiseModelin, npix) else $
      noiseModel = noiseModelin
endelse
whiteNoise = randomn(seed, npix)
if n_elements(Nscales) eq 0 then Nscales = 7
dn = atrous(whiteNoise, n_scales=Nscales)
sigj = cmapply('user:stdev', dn, 1)
sigjl = fan(sigj, npix, /transpose) * fan(noiseModel, Nscales+1)

ds = atrous(spec, n_scales=Nscales)
Msupport = ds * 0 + 1
if n_elements(Nsig) eq 0 then Nsig = 5
bad = where(abs(ds) lt Nsig*sigjl, nbad)
if nbad gt 0 then Msupport[bad] = 0
;;; Extend supported regions by onvolving with WID wide tophat, ceil
;;; values 
if keyword_set(smear) then begin
    if smear eq 1 then wid = 4 else wid = smear
    nth = (wid+10) < (npix-10)
    tophat = fltarr(nth)
    tophat[nth/2-wid/2:nth/2+wid/2] = 1
    for i = 0, Nscales do $
      Msupport[0,i] = convol(Msupport[*,i], tophat, /edge_trunc)
    notz = where(Msupport ne 0, nn)
    if nn gt 0 then Msupport[notz] = 1
endif

new = total(ds * Msupport, 2)
if keyword_set(test) then stop
return, new
end
