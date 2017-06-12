;+ 
; NAME:
; sdss_ewciv
;  Version 1.1
;
; PURPOSE:
;  Measures the EW of the CIV lines (rest values) in the SDSS
;
; CALLING SEQUENCE:
;  sdss_ewciv, wave, flux, sig, strct, ZABS=
;
; INPUTS:
;  wave  -- Wavelength array
;  flux  -- Flux array (normalized)
;  sig   -- Sigma array
;
; RETURNS:
;  strct -- MgII structure with MgII EW filled up
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;  ZABS=  -- Absorption redshift (required)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   27-Feb-2004 Written by JXP
;-
;------------------------------------------------------------------------------

pro sdss_ewciv, wave, flux, sig, conti, strct, ZABS=zabs, PLOT=plot

  if  N_params() LT 4  then begin 
    print,'Syntax - ' + $
             'sdss_ewciv, wave, flux, sig, strct, ZABS= [v1.1]'
    return
  endif 

  if not keyword_set( ZABS ) then stop
  if keyword_set(ONLYTI2) then istrt = 3L else istrt = 0L

  ;; Structure
  strct={sdssmgiistrct}

  ;; Wavelengths
  rwave= [1548.195d, 1550.770]
  nlin = n_elements(rwave)
  obswv = (1.+zabs)*rwave

  ;; dwv
  dwv = wave - shift(wave,1)
  npix = n_elements(wave)

  ;; Set box size for MgII
  boxw = (obswv[1]-obswv[0])/2.

  oconti = conti

  if keyword_set(PLOT) then clr = getcolor(/load)
;  flux = flux < 1.1

  ;; Loop
  for ii=istrt,nlin-1 do begin
      strct.wrest[ii] = rwave[ii]

      ;; Reset continuum
      if ii EQ 0 then begin
          ;; MgII
          mn = min(abs(wave-(obswv[ii]-boxw)),ilhs)
          mn = min(abs(wave-(obswv[ii+1]+boxw)),irhs)

          i1a = (ilhs - 15) > 0L
          i1b = (ilhs - 20) > 0L
          i2a = (irhs + 15) < (npix - 1)
          i2b = (irhs + 20) < (npix - 1)

;         mnc_lhs = mean(conti[ilhs-15:ilhs])
;         mnc_rhs = mean(conti[irhs:irhs+15])
          mnc_lhs = median(flux[i1a:ilhs])
          mnc_rhs = median(flux[irhs:i2a])
          mnw_lhs = mean(wave[i1a:ilhs])
          mnw_rhs = mean(wave[irhs:i2b])

          conti[i1b:i2b]  = interpol([mnc_lhs,mnc_rhs], [mnw_lhs,mnw_rhs], $
                                     wave[i1b:i2b]) 

          if keyword_set(PLOT) then begin
              plot, wave, flux, xrange=[mnw_lhs-10,mnw_rhs+10], $
                    yrange = [0., max(flux[ilhs:irhs])*1.1], backgroun=clr.white, $
                    color=clr.black, psym=10
              oplot, wave, oconti, color=clr.red, linest=1, thick=3
              oplot, wave, conti, color=clr.blue, linest=2, thick=3
          endif
  
      endif

      ;; EW
      if ii LE 2 then begin
          ;; Edges
          mn = min(abs(wave-(obswv[ii]-boxw)),ilhs)
          mn = min(abs(wave-(obswv[ii]+boxw)),irhs)
          ;; EW
          strct.ew[ii] = total( (1.-flux[ilhs:irhs]/conti[ilhs:irhs]) $
                                *dwv[ilhs:irhs]) / (1.+zabs)
          strct.sigew[ii] = sqrt(total( (sig[ilhs:irhs]/conti[ilhs:irhs] $
                                         *dwv[ilhs:irhs])^2)) $
            / (1.+zabs)

          ;; Plot
          if keyword_set(PLOT) then begin
              oplot, [wave[ilhs],wave[ilhs]], [-1e9,1e9], color=clr.green, $
                     linest=2, thick=4
              oplot, [wave[irhs],wave[irhs]], [-1e9,1e9], color=clr.green, $
                     linest=2, thick=4
              print, rwave[ii], strct.ew[ii]
              if ii EQ 2 then stop
          endif
      endif else begin
          ;; Edges
          mn = min(abs(wave-obswv[ii]),icen)
          idx = icen - 2 + lindgen(5)
          ;; EW
          strct.ew[ii] = total( (1.-flux[idx])*dwv[idx]) / (1.+zabs)
          strct.sigew[ii] = sqrt(total( (sig[idx]*dwv[idx])^2)) / (1.+zabs)
      endelse
  endfor


  return


end 


