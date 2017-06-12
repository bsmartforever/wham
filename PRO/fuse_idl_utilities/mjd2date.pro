function mjd2date, mjd, format=form
;+
; NAME:
;	MJD2DATE
;
; PURPOSE:
;	This function converts from Modified Julian Date to a date string.
;
; CATEGORY:
;	Time.
;
; CALLING SEQUENCE:
;	Result = MJD2DATE(MJD)
;
; INPUTS:
;	MJD:	Modified Julian Date. This should be a double. If it isn't,
;		a warning is printed and truncation may occur.
;
; KEYWORD PARAMETERS:
;	FORMAT:	Determines format of output:
;		1 (default): Wednesday 2000 May 17 02:35:22
;		2:           2000-05-17 02:35:22
;		3:           Wednesday 2000 May 17 02:35:22.3	
;
; OUTPUTS:
;	This function returns a standard time string.
;
; PROCEDURE:
;	Straightforward. Uses APL routines.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 6 December 1999.
;	16 May 2000 Added 0.5 to the value of JD1 in order to round to the
;	 nearest second rather than the previous second.
;	29 September 2000 Added warning if the input is not a double.
;	21 July 2003 Modified to skip warning if input is a double array
;	2 December 2003 Added FORMAT=3
;-

	sval = size(mjd)
	if (sval[0] eq 0 and sval[1] eq 5) or (sval[0] eq 1 and sval[2] eq 5) $
		then begin		;if a double
	endif else begin		;if not a double
;	if ((size(mjd))[1] ne 5) then begin
		print,'Input is not a double!!'
		print,mjd,format='("Converted to float as: ",f16.8)'
		print,'Type .con to continue'
		stop
	endelse
	jd = double(mjd) + 2400000.5d0
	jd1 = jd + 0.5
	jd1 = jd1 + 5.787037037d-6	;Add 0.5 seconds for rounding purposes
					; since dt_tm_mak appears to truncate.
	d1 = floor(jd1)
	d2 = (jd1-floor(jd1))*86400.0d0

	if not keyword_set(form) then form = 1
	case form of
		2:	date = dt_tm_mak(d1,d2,format='Y$-0n$-d$ h$:m$:s$')
		3:	date = dt_tm_mak(d1,d2,format='W$ Y$ n$ d$ h$:m$:s$f$')
		else:	date = dt_tm_mak(d1,d2,format='W$ Y$ n$ d$ h$:m$:s$')
	endcase
	return, date

end
