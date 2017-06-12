function readfitsheader, header, headstr, comment=comment

;+
;NAME: readfitsheader()
;PURPOSE: read any header from FITS file
;CALLING SEQUENCE: headval = getfitsheader(header, headstr [,/comment])
;  'header' can be either the file name of a FITS file (in which case
;    readfitsheader will read the FITS file) or the header itself as an array
;    of strings
;
;OPTIONAL KEYWORD INPUTS: /comment - extract the comment string, not the value
;
;MODIFICATION HISTORY:
;	Written by ASH 2005-8-9 (based on Oberlin getdynheader)
;	Modified by ASH 2009-6-19: bug fix in dealing with string values
;   Modified by ASH 2009-11-21: add /comment option
;-

IF n_elements(header) LE 1 THEN hdr = headfits(header) ELSE hdr = header

IF n_params() NE 2 THEN $
	message,'USAGE: headerval = readfitsheader ( header, headstr )'

sub = where(strpos(hdr, strupcase(headstr) ) NE -1)

IF NOT array_equal(sub, -1) THEN BEGIN
	val = hdr[sub]
	val = val[0]
	sub=strpos(val, 'M/S')
	val = strpos(val, 'M/S') EQ -1 ? strsplit(val, '=/', /extract) : $
		strsplit(val, '=', /extract)
	IF keyword_set(comment) THEN return, strtrim(val[2], 2)
	val = strtrim(val[1], 2)
	IF ( strmid(strtrim(val, 2), 0, 1) EQ "'" ) THEN $
		return, strtrim( strmid(val, 1, strlen(strtrim(val, 2)) - 2), 2)
	return, float(strtrim(val, 2))
ENDIF

return,-1	; we shouldn't still be here unless nothing was found!

END
