FUNCTION getymdstr, JD
caldat, JD, month, day, year
return, string(year-2000, month, day, format='(3I02)')
END

FUNCTION getymstr, JD
caldat, JD, month, day, year
return, string(year-2000, month, format='(2I02)')
END

FUNCTION searchfiles, today, enddate, nfiles, logdir, prefix
	searchfiles = strarr(enddate - today + nfiles)
	FOR i=0, n_elements(searchfiles) - 1 DO $
		searchfiles[i] = logdir + prefix+'.' + getymdstr(today-nfiles+1+i) + $
			' ' + logdir + 'archive.' + getymstr(today-nfiles+1+i) + '/' + $
			prefix + '.' + getymdstr(today-nfiles+1+i)+'.gz'
	searchfiles = strjoin(searchfiles + ' ')
	spawn, '/bin/ls ' + searchfiles, files, err
	return, files
END

FUNCTION searchfilesmonth, today, enddate, nfiles, logdir, prefix
	searchfiles = strarr(enddate - today + nfiles)
	FOR i=0, n_elements(searchfiles) - 1 DO $
		searchfiles[i] = logdir + prefix+'.' + getymstr(today-nfiles+1+i) + $
			' ' + logdir + 'archive.' + getymstr(today-nfiles+1+i) + '/' + $
			prefix + '.' + getymstr(today-nfiles+1+i)+'.gz'
	searchfiles = strjoin(searchfiles + ' ')
	spawn, '/bin/ls ' + searchfiles, files, err
	return, files
END

PRO icsday, year, month, day, endyear, endmonth, endday, x=x, smooth=smooth, $
	yesterday=yesterday, logdir=logdir, _extra=extra

;+
;NAME: icsday
;PURPOSE: Produce summary plots of a time interval of ICS readings
;SYNTAX: icsday [, year, month, day [, endyear, endmonth, endday] [, /x] $
;	[, smooth=smooth] [, logdir=logdir]
;INPUTS: year, month, day: numerical year, month, and day.
;	year can be in format YYYY or YY. For YY, year must be between 1990
;		and 2089. If no date specified, will plot today.
;	Will plot 24 hours, midnight to midnight, unless end date is also
;		specified
;OUTPUT: output a postscript file to /d/wham/log/plots/icsday.ps
;	(unless /x is set, in which case output to the current graphics device)
;OPTIONAL KEYWORD INPUTS:
;	/x: output to X in a new window
;	/yesterday: plot the ICS summary plot for yesterday
;		(ignored if a date is specified)
;	smooth = smooth (smoothing kernel in minutes; default: 1)
;	logdir = logdir (default: '/d/wham/log/')
;-

logdir = keyword_set(logdir) ? logdir : '/d/wham/log/'

IF NOT (n_params() EQ 0 OR n_params() EQ 3 OR n_params() EQ 6) THEN $
	message, 'SYNTAX: icsday [,yyyy, mm, dd [,yyyy_end, mm_end, dd_end]]'	

IF n_params() EQ 0 THEN	BEGIN
	today = long(systime(/julian)+0.5)-0.5
	IF keyword_set(yesterday) THEN today -= 1
ENDIF ELSE BEGIN
	year = year LT 90 ? year+2000 : year LT 100 ? year+1900 : year
	today = julday(month, day, year, 0, 0, 0)
ENDELSE
enddate = today + 1.0
IF n_params() EQ 6 THEN BEGIN
		endyear = endyear LT 90 ? endyear+2000 : endyear LT 100 ? endyear+1900 : endyear
		enddate = julday(endmonth, endday, endyear, 0, 0, 0) + 1.0
ENDIF

daystosearch = 1

files = searchfiles(today, enddate, daystosearch, logdir, 'icslog')
IF array_equal(files,'') THEN BEGIN
	print, 'found nothing on first pass; expanding search range'
	files = searchfiles(today, enddate, 15, logdir, 'icslog')
ENDIF
IF array_equal(files, '') THEN message, 'FATAL ERROR: searched for files and found nothing'
print, 'found ICS files ', files

pcsfiles = searchfiles(today, enddate, daystosearch, logdir, 'pcslog')
IF array_equal(pcsfiles,'') THEN pcsfiles = searchfiles(today, enddate, 15, logdir, 'pcslog')
IF array_equal(pcsfiles, '') THEN message, 'FATAL ERROR: searched for pcsfiles and found nothing'
print, 'found PCS files ', pcsfiles

ccdpresfiles = searchfilesmonth(today, enddate, daystosearch, logdir, 'ccd_pressure_log')
IF array_equal(ccdpresfiles, '') THEN message, 'FATAL ERROR: searched for ccd_pressure files and found nothing'
print, 'found CCD pressure files ', ccdpresfiles

IF keyword_set(x) THEN window, /free, xs=1100, ys=800 ELSE BEGIN
	set_plot, 'ps'
	device, filename=logdir + 'plots/icsday.ps', $
		xs=8.0, ys=10.0, xoff=0.25, yoff=0.5, /inches, landscape=0, /color
ENDELSE

!x.range = [today, enddate]

title = keyword_set(yesterday) ? $
	'WHAM summary plot for ' + string(today, format='(C(CDwA, X, CMoA, X, CDI, X, CYI5))') : $
	string(today, format='(C())') + ' to ' + string(enddate, format='(C())')

plotICSday, files, pcsfiles, ccdpresfiles, $
	title = title, smooth=keyword_set(smooth) ? smooth : 1, $
	begintime=today, endtime=enddate, onepage=keyword_set(x), _extra=extra

!x.range = 0

IF NOT keyword_set(x) THEN BEGIN
	device, /close
	set_plot, 'x'
ENDIF

END
