FUNCTION read_pcslog, file, keepunzip=keepunzip

;+
;NAME: read_pcslog
;PURPOSE: read a PCS log file in the format output by wham_pcs_read and
;	return an IDL structure with the data
;CALLING SEQUENCE: pcslog = read_pcslog(file)
;INPUT:
;	file: path to PCS log file(s) (string scalar or array)
;NOTE: read_icslog must be compiled before read_pcslog to get
;	search_and_unzip function
;-

format='A,X,F,L,L,I,  F,F,F,F,  X,I,F,F,F,F,  I,I,F,  I,I,F,  X,I,F,F,F,F,  I,I,F,  I,I,F'
readcol_multi, search_and_unzip(file, gzip, unzippedfile), $
	delimiter='|', format=format, /silent, $
	prestr, version, sysclock_s, uptime,sysstatus, $
	fillpres_v,ventpres_v,fillpres,ventpres, $
	cha_status,cha_setpoint,cha_pres,cha_pres_error,cha_temp, $
	cha_upvalve_control,cha_upvalve_status,cha_upvalve_voltage, $
	cha_downvalve_control,cha_downvalve_status,cha_downvalve_voltage, $
	chb_status,chb_setpoint,chb_pres,chb_pres_error,chb_temp, $
	chb_upvalve_control,chb_upvalve_status,chb_upvalve_voltage, $
	chb_downvalve_control,chb_downvalve_status,chb_downvalve_voltage

IF NOT keyword_set(keepunzip) THEN rmzipped, gzip, unzippedfile

IF n_elements(sysclock_s) EQ 0 THEN return, -1

pcslog = { pcslog, sysclock_s:0L, sysclock:0D, tz:-3., sysstatus:0D, $
	cha_setpoint: 0., cha_pres:0., cha_pres_error:0., cha_temp:0., $
	chb_setpoint: 0., chb_pres:0., chb_pres_error:0., chb_temp:0. }

pcslog = replicate(pcslog, n_elements(sysclock_s) )

pcslog.tz = gettz(prestr)
pcslog.sysclock_s = sysclock_s
pcslog.sysclock = sysclock(sysclock_s, pcslog.tz)
pcslog.sysstatus = sysstatus

pcslog.cha_setpoint = cha_setpoint
pcslog.cha_pres = cha_pres
pcslog.cha_pres_error = cha_pres_error
pcslog.cha_temp = cha_temp

pcslog.chb_setpoint = chb_setpoint
pcslog.chb_pres = chb_pres
pcslog.chb_pres_error = chb_pres_error
pcslog.chb_temp = chb_temp

return, pcslog

END
