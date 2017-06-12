;+
; NAME:
;   long_sensfunc
;
; PURPOSE:
;   Use a standard star spectrum to determine the spectroscopic
;   response function. 
;
; CALLING SEQUENCE:
;
; INPUTS:
;   scifile         - file containing object structure which has spectrum 
;                     for standard star
;
;   standard_name   - name of the standard star
;   
;   sensfuncfile    - File to write the sensitivity function out to
;
;
; OPTIONAL INPUTS:
;   OBJID           - object id of standar star in the object structure. Default
;                     is to the first object. 
;   nresln          - Break point spacing in resolution elemnets
;                     (default=20)
;   /MSK_BALM       - Mask Balmer lines (recommended but not default)
;
; OUTPUTS:
;   mag_set        - structure containing b-spline info for sensitivity function
;
; OPTIONAL OUTPUTS:
;  sensfunc         - sensitivity function evaluated
;
; COMMENTS:
;                   See README file in /apps2/iraf211/iraf/noao/lib/onedstds/
;                   for list of standard stars and the names of the
;                   associated files
;
; EXAMPLES:
;
; BUGS:
;                   Does not take into account atmospheric extinction!!!
;                   Leaves out first and last wavelength bins of
;                   sensitivity function
;
; PROCEDURES CALLED:
;   traceset2xy (idlutils)
;   xy2traceset (idlutils)
;   splog       (idlutils)
;
; INTERNAL SUPPORT ROUTINES:
;
; REVISION HISTORY:
;   01-Oct-2005  Written by J. Hennawi UC Berkeley
;------------------------------------------------------------------------------
FUNCTION long_sensfunc, scifile, sensfuncfile $
                        , SCIHDR = SCIHDR1, STANDARD_STRUCT = STANDARD_STRUCT1 $
                        , std_name = std_name $
                        , sensfunc = sensfunc, sensfit = sensfit $
                        , wave = wave, nresln = nresln $
                        , chk = chk, MSK_BALM = msk_balm $
                        , LINE_BALM = LINE_BALM $
                        , BALM_MASK_WID = BALM_MASK_WID $
                        , sciind = sciind1, flux_std = flux_std_int $
                        , STDFILE = STDFILE,  INMASK = INMASK $
                        , FINALMASK = FINALMASK, starflux = flux

  if  N_params() LT 2  then begin 
      print,'Syntax - ' + $
        'sfunc = long_sensfunc(scifil, outfil, STD_NAME=) [v1.0]'
      return, -1
  endif 


; stay five pixels away from edge
IF NOT KEYWORD_SET(NRESLN) THEN NRESLN = 20  ;; JXP -- Avoid 'narrow' features
IF NOT KEYWORD_SET(LO_BUFFER) THEN LO_BUFFER = 8L
IF NOT KEYWORD_SET(HI_BUFFER) THEN HI_BUFFER = 5L
IF NOT KEYWORD_SET(BALM_MASK_WID) THEN BALM_MASK_WID = 5.0D

IF KEYWORD_SET(scihdr1) AND KEYWORD_SET(STANDARD_STRUCT1) THEN BEGIN
   scihdr = scihdr1
   standard_struct = standard_struct1
ENDIF ELSE BEGIN
   scihdr = xheadfits(scifile)
   temp = xmrdfits(scifile, 5)
   IF NOT KEYWORD_SET(SCIIND1) THEN BEGIN maxf = max(temp.PEAKFLUX, sciind)
   ENDIF ELSE sciind = sciind1 
   standard_struct = temp[SCIIND]
ENDELSE

IF KEYWORD_SET(STDFILE) THEN BEGIN
   readcol, stdfile, wave_std, flux_std1, format = 'D,D'
   flux_std = 10.0d*flux_std1 
;; this factor of converts 1.0e-16 erg/cm^2/s/A to units of 1.0e-17
;; erg/cm^2/s/A. Since the xidl std's have that normalization
ENDIF ELSE BEGIN
   longslit_dir = getenv('LONGSLIT_DIR')
   std_file = longslit_dir+ '/calib/standards/calspec/' + std_name + '.fits.gz'
; read in standar star spectrum
   std = xmrdfits(std_file, 1)
   wave_std = std.WAVELENGTH
   flux_std = 1.0d17*std.FLUX   ; fluxes are in units of 1.0e-17 erg/cm^2/s/A
ENDELSE
   
; uncalibrated observed spectrum
wave = standard_struct.wave_box
flux = standard_struct.flux_box
ivar = standard_struct.ivar_box
IF KEYWORD_SET(INMASK) THEN BEGIN
   IF n_elements(INMASK) NE n_elements(ivar) THEN $
      message, 'Your mask must be aligned with the std spectrum'
   badpix = WHERE(INMASK EQ 0, nbad)
   IF nbad GT 0 THEN ivar[badpix] = 0.0
ENDIF
; parse headers and read in extinctino file
ext = long_extinct(wave, scihdr, AIRMASS = AIRMASS, EXPTIME = EXPTIME)
; extinction correct data and divide by exposure time
flux = flux*ext
ivar = ivar/ext^2

; find the min and max of the calibration spectrum
wave_min_std = min(wave_std)
wave_max_std = max(wave_std)
; find the min and max of the standard
ind_sort = sort(wave)
wave_min_obs = min(wave)        ;wave[ind_sort[LO_BUFFER-1]]
wave_max_obs = max(wave)        ;wave[ind_sort[nwave-1L-HI_BUFFER]]

wave_min = wave_min_std > wave_min_obs
wave_max = wave_max_std < wave_max_obs 

calib_inds = WHERE(wave GE wave_min AND wave LE wave_max)
wave = wave[calib_inds]
flux = flux[calib_inds]
ivar = ivar[calib_inds]
sort_ind = sort(wave)
wave = wave[sort_ind]
flux = flux[sort_ind]
ivar = ivar[sort_ind]
nwave = n_elements(wave)
;; Don't use the edges in the fit
;IF KEYWORD_SET(LO_BUFFER) THEN ivar[0:LO_BUFFER] = 0
;IF KEYWORD_SET(HI_BUFFER) THEN ivar[nwave-1L-HI_BUFFER:nwave-1L] = 0

;interpolate calbiration spectrum onto observed wavelengths
flux_std_int = interpol(flux_std, wave_std, wave)

; Compute an effective resolution for the standard. This could be improved
; to setup an array of breakpoints based on the resolution. At the 
; moment we are using only one number
std_res = 2.0*djs_median(abs(wave_std - shift(wave_std, 1)))
IF TAG_EXIST(standard_struct, 'PIX_RES') THEN BEGIN
    pix_res = djs_median(standard_struct.PIX_RES)
    disp = djs_median(abs(wave-shift(wave, 1)))
    resln = pix_res*disp >  std_res
ENDIF ELSE resln = std_res


if keyword_set(MSK_BALM) then begin
   IF NOT KEYWORD_SET(LINE_BALM) THEN $
      line_balm = [3836.4 $
                   , 3969.6 $
                   , 3890.1 $
                   , 4102.8 $                        ;; (H9,CaII H, Hf,H-delta)
                   , 4102.8 $
                   , 4341.6 $
;;           , 4687.3 $
                   , 4862.7 $ ;; (H-gamma,H-beta,H-alpha,Fe)
                   , 5407.0 $
                   , 6564.6 $
                   , 8224.8 $
                   , 8239.2] 
   nbalm = n_elements(line_balm)
   for qq = 0L, nbalm-1 do begin
      mskwv = where(abs(wave-line_balm[qq]) LE BALM_MASK_WID*resln, nbad)
      if nbad NE 0 then ivar[mskwv] = 0.
   endfor
endif
;; Mask telluric absorption
tell = (wave GE 7580.0D AND wave LE 7750.0D) OR $
       (wave GE 7160.0D AND wave LE 7340.0D) OR $
       (wave GE 6860.0  AND wave LE 6930.0D)
tell_ind = where(tell, ntell)
IF ntell GT 0 THEN ivar[tell_ind] = 0.0
finalmask = ivar GT 0
mag_set = bspline_magfit(wave, flux, ivar, flux_std_int $
                         , bkspace = resln*nresln $
                         , maxiter = 10, maxrej = 5, upper = 3, lower = 3 $
                         , sensfit = sensfit, sensfunc = sensfunc $
                         , wave_min = wave_min, wave_max = wave_max $
                         , outmask = outmask) 

if keyword_set(CHK) then x_splot, wave, sensfit, /blo 
IF KEYWORD_SET(sensfuncfile) THEN mwrfits, mag_set, sensfuncfile, /create



RETURN, mag_set
END
