;+
;NAME: get_dark_times
;PURPOSE: get beginning and end of dark time for a given night
;CALLING SEQUENCE: [begin_time, end_time] = get_dark_times( date [, /human] )
;INPUT:
;	date: date to find dark times for (JD)
;OPTIONAL KEYWORD INPUT:
;	/human: return human readable output in CLT
;NOTE:
;	get_dark_times returns a 2-element array with the beginning and end of
;		the dark time for the given NIGHT. The end time will usually be
;		the following morning (after midnight). Late in the cycle (after
;		new moon), the beginning time will be the following morning.
;MODIFICATION HISTORY:
;	2009-6-26: Written by Alex Hill, pulling most functionality out of
;		darktime.pro
; 2016-1-12: Chile is now UTC-3 = CLT all the time. Began in 2015.
;-

FUNCTION get_dark_times, dateinp, human=human

observatory, 'ctio', obs
;; Chile is now UTC -3 all the time. May need updating if OBSERVATORY gets changed.
obs.tz = 3.0

nd_lines = file_lines('/d/wham/lib/darkhours.txt')
dlines = strarr(nd_lines)
  
openr, unit, '/d/wham/lib/darkhours.txt', /get_lun
readf, unit, dlines
free_lun, unit

date = string(long(dateinp - obs.tz/24.0), $
	format = '(c(cmoi2.2,"/",cdi2.2,"/",cyi4.2))')
idx = where(strmatch(dlines, date+"*") eq 1)
IF NOT array_equal(idx, -1) THEN BEGIN
	dline = dlines[idx]
	dark_times = stregex(dline, "Start: +([0-9:]+) \| End: +([0-9:]+)", $
		/extract, /subexpr)
	dark_times[1] = (stregex(dline, "Start: +([0-9:]+)", /extract, /subexpr))[1]
	dark_times[2] = (stregex(dline, "End: +([0-9:]+)", /extract, /subexpr))[1]
;
;; Old DST adjustment
;	tzstr = (strsplit(dline, ' |', /extract))[6]
;	IF (tzstr EQ 'CLST') THEN obs.tz -= 1
	
	darktimetemp = long(strsplit(dark_times[1], ':', /extract))
	darktime = long(dateinp - obs.tz/24. + 12./24.) + $
		double(darktimetemp[0] + darktimetemp[1]/60.)/24. - 12./24.
	IF darktimetemp[0] LT 12 THEN darktime += 1.
	starttime = darktime + obs.tz/24.
	
	lighttimetemp = long(strsplit(dark_times[2], ':', /extract))
	lighttime = long(dateinp - obs.tz/24. + 12./24.) + $
		double(lighttimetemp[0] + lighttimetemp[1]/60.)/24. - 12./24.
	IF lighttimetemp[0] LT 12 THEN lighttime += 1.
	endtime = lighttime + obs.tz/24.
	
	IF keyword_set(human) THEN BEGIN
	  starttime = string(starttime - obs.tz/24.0, $
		format='("Start time: ",C(CHI2.2,":", CMI2.2, ":", CSI2.2), " ' + tzstr + '", /)')
	  endtime = string(endtime - obs.tz/24.0, $
		format='("End time: ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " ' + tzstr + '", /)')
	ENDIF
		
	return, [starttime, endtime]
ENDIF

IF keyword_set(human) THEN return, ["No dark time", ""]
return, [0, 0]

END
