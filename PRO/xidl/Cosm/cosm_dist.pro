;+ 
; NAME:
; cosm_dist
;
; PURPOSE:
;  Calculate the cosmological comoving distance (Mpc) given a 
;     cosmology and redshift   
;
; CALLING SEQUENCE:
;    dist = cosm_dist(z)
;
; INPUTS:
;   z -  Redshift
;   [cosomlogy] -  By default the cosmology is set prior to 
;     calling this using cosm_common.  You can have this done by
;     keying 
;
; RETURNS:
;   dist  -- Distance in Mpc
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;  H0=    -- Hubble constant (km/s/Mpc)
;  /INIT  -- Initializes the cosmology to the default values
;  /LUM   -- Luminosity distance (Mpc)
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   
;   dist = cosm_dist(2., /INIT)
;
; PROCEDURES CALLED:
;   x_constants
;   cosm_common
;   cosm_intdist
;   qromb
;
; REVISION HISTORY:
;   22-Nov-2003 Written by JXP
;-
;------------------------------------------------------------------------------

function cosm_intdist, z
common cosmolgy_cmmn, cosm_dm, cosm_K, cosm_h, cosm_Ob, cosm_L, cosm_r

  distintg = 1./ cosm_hubble(z)
  return, distintg
end
  

function cosm_dist, z, H0=h0, INIT=init, LUM=lum, _EXTRA=extra, SILENT=silent

  common cosmolgy_cmmn

  if (N_params() LT 1) then begin 
    print,'Syntax - ' + $
             'dist = cosm_dist(z, H0=, /INIT, /LUM) [Mpc; v1.1]'
    return, -1
  endif 

  if keyword_set( INIT ) or keyword_set(EXTRA) then $
    cosm_common, H0=h0, _EXTRA=extra, SILENT=silent

;  resolve_routine, 'cosm_hubble', /no_recompile, /is_function
  dist = qromb('cosm_intdist', 0., z, /double)
  c = x_constants()

  if keyword_set(LUM) then fact = (1+z) else fact = 1.

  if not keyword_set(SILENT) then print, 'cosm_dist: Units are in Mpc'
  return, fact*dist*c.c/1d5   ; Units are Mpc

end

