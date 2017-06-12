function scale_velocity, vel, phdr,line, silent=silent, geocentric=geocentric, $
	obs=obs, line=obsline

;+
;NAME: scale_velocity
;PURPOSE: convert velocity scale of an axis into LSR frame
;SYNTAX: scale_velocity() can be invoked in two forms:
;vel_lsr = scale_velocity(vel_raw, pheader, line [, /silent] [, /geocentric])
;vel_lsr = scale_velocity(obs=obs [, /silent] [, /geocentric]
;INPUTS:
;	vel_raw - velocity axis in raw frame
;	pheader - pheader from FITS file (used to determine pressures and VLSR)
;	line - string specifying the line: hb, oiii, blue_nii, hei, ha, nii, sii, oii, oi
; OR
;	obs=obs - structure containing observational data (produced by READOBS)
;	line=line - string specifying the line. Defaults to 'ha'
;-

IF keyword_set(obs) THEN BEGIN
	p = [mean(obs.pamon), mean(obs.pbmon)]/10.
	vlsr = obs[0].vlsr
	vel = obs[0].vel
	line = keyword_set(obsline) ? obsline : 'ha'
ENDIF ELSE BEGIN
	p = [readfitsheader(phdr, 'PAMON'), readfitsheader(phdr, 'PBMON')]/10.
	vlsr = keyword_set(geocentric) ? 0. : readfitsheader(phdr, 'VLSR')
ENDELSE

geo_zero = predict_vel_calib3(p, line, silent=keyword_set(silent))
IF NOT keyword_set(silent) THEN $
	print,'Geocentric zero = '+strtrim(geo_zero, 2)+'; VLSR = '+strtrim(vlsr)
return, vel - (geo_zero + vlsr)

END
