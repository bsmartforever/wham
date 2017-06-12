function date2mjd, datetime, silent=silent
;+
; NAME:
;	DATE2MJD
;
; PURPOSE:
;	This function converts from a date string to Modified Julian Date.
;
; CATEGORY:
;	Time.
;
; CALLING SEQUENCE:
;	Result = DATE2MJD(Date)
;
; INPUTS:
;	Date:	A date and timestring. Any form accepted by the APL routine
;		DATE2JD is acceptable.
;
; KEYWORD PARAMETERS:
;	SILENT:IF included, don't print any informational messages.
;
; OUTPUTS:
;	This function returns the Modified Julian Day.
;
;
; PROCEDURE:
;	Straightforward. Uses APL routines.
;
; EXAMPLE:
;
; NOTE: 
;	The date must contain the month as a name of 3 or more letters!
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 29 September 2000.
;	7 June 2001 Now prints input date in a standard form so there is no
;	 ambiguity on what date is being converted.
;	28 January 2002 Added SILENT keyword.
;-

	dt_tm_brk,datetime,date,time

	jd1 = double(date2jd(date))
	hh = double(strmid(time,0,2))
	mm = double(strmid(time,3,2))
	ss = double(strmid(time,6,10))
	jd2 = hh/24.0D + mm/1440.0D + ss/86400.0D

	jd = jd1 + jd2
	mjd = jd - 2400000.0d0 - 1.0
			;I don't understand why this is 1.0 instead of 0.5,
			; but it works.

	if not keyword_set(silent) then begin
		print,'Converting ',mjd2date(mjd),' to MJD'
		print
	endif

	return, mjd

end
