FUNCTION read_ccd_pressure_log, file, keepunzip=keepunzip

;+
;NOTE: read_icslog must be compiled before read_ccd_pressure log to get
;	search_and_unzip function
;-

readcol_multi, search_and_unzip(file, gzip, unzippedfile), delimiter='|', $
	format='A,L,F,F,X', silent=1, prestr, sysclock_s, ccd_pres, ig_pres

IF n_elements(sysclock_s) EQ 0 THEN return, -1
	
IF NOT keyword_set(keepunzip) THEN rmzipped, gzip, unzippedfile

ccd_pressure_log = { ccd_pressure_log, tz:-3, sysclock_s:0L, sysclock:0D, pressure:0.0e+0, ig_pres:0.0e+0 }
ccd_pressure_log = replicate(ccd_pressure_log, n_elements(sysclock_s))

ccd_pressure_log.tz = gettz(prestr)
ccd_pressure_log.sysclock_s = sysclock_s
ccd_pressure_log.sysclock = sysclock(sysclock_s, ccd_pressure_log.tz)
ccd_pressure_log.pressure = ccd_pres
ccd_pressure_log.ig_pres = ig_pres

return, ccd_pressure_log[sort(ccd_pressure_log.sysclock)]

END
