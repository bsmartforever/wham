FUNCTION trim_icslog, log, period, ignorecheck=ignorecheck

;+
;NAME: trim_icslog
;SYNTAX: log = trim_icslog(log [, period])
; or log = trim_icslog( read_icslog(logfile) [, period])
;PURPOSE: trim an icslog or pcslog to extract only every nth entry
;	by default, reads every 60th entry, appropriate for extracting an entry
;	every 60 s from a log file that is updated every second
;INPUT:
;	logfile: logfile
;OPTIONAL INPUT:
;	period: folding period in number of lines (default: 60)
;		NOTE that if the log file is not written every second, this may
;		not give the expected time interval between lines
;		The function does simple checking to warn the user if
;		the first two entries are not separated by one second
;OPTIONAL KEYWORD INPUT:
;	/ignorecheck: trim the log file even if the time difference check fails
;MODIFICATION HISTORY:
;	2009 March 9: written by Alex Hill
;-

period = n_params() GE 2 ? period : 60
IF log[1].sysclock_s - log[0].sysclock_s NE 1 THEN BEGIN
	print, 'READ_ICSLOG_TRIM WARNING: difference in times 1 and 0 is ' + $
		strtrim(log[1].sysclock_s - log[0].sysclock_s, 2) + ' s, not one second'
	IF NOT keyword_set(ignorecheck) THEN BEGIN
		print, 'Returning full log file'
		return, log
	ENDIF ELSE print, 'Extracting trimmed log file anyway'
ENDIF

idx = indgen(n_elements(log))
return, log[where(idx MOD period EQ 0)]

END
