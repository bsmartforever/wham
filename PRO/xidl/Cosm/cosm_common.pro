;+ 
; NAME:
; cosm_common
;
; PURPOSE:
;    Routine to initialize and set values in the Cosmology common
;    block named 'cosmolgy_cmmn'
;
; CALLING SEQUENCE:
;   cosm_common
;
; INPUTS:
;   H0 =   Hubbles constant in km/s/Mpc
;   Omegavac = Lambda value   [Default: 0.7]
;   OmegaDM = Omega for Dark Matter  [Default: 0.3]
;
; RETURNS:
;
; OUTPUTS:
;   
; OPTIONAL KEYWORDS:
;  /W06MAP -- Applies the WMAP cosmolgy from 2006
;  /SILENT -- No screen output
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
;   22-Nov-2003 Written by JXP
;-
;------------------------------------------------------------------------------

pro cosm_common, H0=h0, Omegavac=omegavac, OmegaDM=omegaDM, W06MAP=W06MAP, $
                 SILENT=silent

common cosmolgy_cmmn, cosm_dm, cosm_K, cosm_h, cosm_Ob, cosm_L, cosm_r

;; Original defaults are 0.3, 0.7 and 75km/s/Mpc
  if not keyword_set( OmegaDM ) then cosm_dm = 0.3 else cosm_dm = omegaDM
  if size( H0 , /type) EQ 0 then cosm_h = 75. else cosm_h = H0
  if size( Omegavac, /type) EQ 0 then cosm_L = 0.7 else cosm_L = Omegavac

  if keyword_set(W06MAP) then begin
      cosm_h = 73.
      cosm_dm = 0.28
      cosm_L = 0.72
  endif

  cosm_K = 1.d - cosm_dm - cosm_L
  cosm_r = 2.4725753d-05

  if not keyword_set(SILENT) then begin
      print, 'cosm_common: Using this cosmology --'
      print, 'Omega_m = ', cosm_dm
      print, 'H (km/s/Mpc) = ', cosm_h
      print, 'Omega_L  = ', cosm_L
  endif
  return

end

