;+ 
; NAME:
; manuakea_sky
;    Version 1.1
;
; PURPOSE:
;    Gives an estimate of the sky brightness for Mauna Kea
;
; CALLING SEQUENCE:
;  mag = maunakea_sky( wave, phase )
;
; INPUTS:
;  wave=  -- Wavelength array
;  phase= -- Phase of the moon [days]
;
; RETURNS:
;  mag=  -- Sky brightness in AB mags
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   27-Oct-2005 Written by JXP based on HIRES S2N code
;-
;------------------------------------------------------------------------------
function maunakea_sky, wave, iphase

;     Sky brightness at Mauna Kea. 
;..............................................................................
;
;  NWAVE=NPHASE 
;  6=5

  ;; Max phase
  phase = iphase < 13.9
;
  xwave = [3500, 4200, 5500, 6700, 7800, 22000.]
  xphase = [0., 3., 7., 10., 14.]     
  xsky = [ [22.4, 23.0, 21.9, 21.2, 19.9, 12.0], $
           [21.5, 22.4, 21.7, 20.8, 19.9, 12.0], $ 
           [19.9, 21.6, 21.4, 20.6, 19.7, 12.0], $
           [18.5, 20.7, 20.7, 20.3, 19.5, 12.0], $
           [17.0, 19.5, 20.0, 19.9, 19.2, 12.0] ]   ;; Vega values

;
  ;; Convert to AB
  ;; AB  U_AB = U + 0.71;  B_AB = B-0.11; V_AB = V, R_AB = R+0.199,
   ;; I_AB=I+0.454  [Probably optimal for galaxies]
  xsky[*,0] = xsky[*,0] + 0.71
  xsky[*,1] = xsky[*,1] - 0.11
  xsky[*,3] = xsky[*,3] + 0.199
  xsky[*,4] = xsky[*,4] + 0.454


  nwv = n_elements(wave)
  msky = fltarr(nwv)
  for qq=0L,nwv-1 do begin
      gd = where(xwave LE wave[qq], ngd)
      if ngd LE 1 then j = 0 else begin
          mn = min(wave[qq] - xwave[gd], mnj)
          j = gd[mnj]
      endelse

      gd = where(xphase LE phase)
      if ngd LE 1 then k = 0 else begin
          mn = min(phase - xphase[gd], mnk)
          k = gd[mnk]
      endelse 
;
;
      t = (wave[qq] - xwave[j])/(xwave[j+1]-xwave[j])
      u = (phase - xphase[k])/(xphase[k+1]-xphase[k])
;       
      msky[qq] = (1.-t)*(1.-u)*xsky[j,k] + t*(1-u)*xsky[j+1,k] $
        + t*u*xsky[j+1,k+1] +(1-t)*u*xsky[j,k+1]
  endfor
;        
  return, msky
end
