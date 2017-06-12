function wlscalef, channel, slit, inputvalue, pix2wave=pix2wave, $
	wave2pix=wave2pix, message=message, chaninfo=chaninfo, $
	silent=silent
;+
; NAME:
;	WLSCALEF
;
; PURPOSE:
;	This function returns the pixel number for a given wavelength,
;	or the wavelength for a given pixel for flight FUSE data. The
;	direction of the conversion depends on the keywords. It reads the
;	flight wavelength calibration files. Note that it currently reads
;	only the point source data.
;
; CATEGORY:
;	Wavelength scale.
;
; CALLING SEQUENCE:
;	Result = WLSCALEF(Channel,Slit,Inputvalue)
;
; INPUTS:
;	Channel:A string describing the channel, e.g. 'LiF1A'
;	Slit:	Slit name, e.g. 'MDRS'.
;	Inputvalue:
;		Input wavelength or pixel number, depending on direction
;		of conversion. This may be a single number, or a vector
;		of values.
;
; RESTRICTIONS:
;	The CF_CALDIR environment variable must point to the directory
;	containing the wavelength calibration file, e.g. 
;	/usr/local/fusesw/calfuse/v2.4/calfiles.
;
; KEYWORD PARAMETERS:
;	WAVE2PIX:
;		If set, the function assumes that wavelength(s) were
;		input, and it returns pixels.
;	PIX2WAVE:
;		If set, the function assumes that pixels were input,
;		and it returns wavelengths. This is the default, so
;		this keyword is not required.
;	MESSAGE:A text message describing where the data came from, etc.
;		NOT YET WORKING.
;	CHANINFO:	NOT YET WORKING
;		A text string containing a description of the
;		calculated segment. Useful as a plot title.
;	SILENT:	If included, suppress diagnostic messages.
;
; OUTPUTS:
;	Returns the wavelength for Inputvalue (or the pixel number, if
;	WAVE2PIX is set). A -1 is returned if the wavelength is not present
;	on the chosen Channel
;
; PROCEDURE:
;	Reads the wavelength calibration file from the $CF_CALDIR directory.
;
; EXAMPLE:
;	To calculate the wavelength for all pixels in the LiF1A MDRS slit:
;		IDL> pixel = indgen(16384)
;		IDL> wavelength = wlscalef('LiF1A','MDRS',pixel)
;
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 30 January 2000.
;	17 May 200 Modified to print out the wavelength file used. Now
;	 uses 008 (did use 002) by default.
;	13 June 2000 Now uses file 009.
;	29 June 2000 Modified to use the latest wavelength file.
;	2 July 2000 Now uses CHANNEXT.PRO to determine which extension to read.
;	18 September 2000 Added WAVE2PIX keyword.
;	11 October 2000 Added SILENT keyword.
;	7 September 2001 Modified error message for no match.
;	17 September 2001 Changed call to rstrpos to use strpos with 
;	 /reverse_search keyword
;       2006-02-15     wvd     Replaced all calls to findfile with file_search.
;-
	wavedir = '$CF_CALDIR'		;directory containing calibration files.

	chan = strupcase(strmid(channel,0,3))
	seg = strlowcase(strmid(channel,3,2))
	exnum = channext(chan,strupcase(slit),'POINT')
	print,'Reading extension ',exnum


	wavefilelist = file_search(wavedir + '/wave' + seg + '*.fit')
	numfiles = n_elements(wavefilelist)
	filenum = fix(strmid(wavefilelist,$
		(strpos(wavefilelist[0],'wave',/reverse_search)+6),3))
	filemax = where(filenum eq max(filenum))
				;find the file with the largest number
	wavefile = wavefilelist[filemax]
	wavefile = wavefile[0]

	print,'Using wavelength file ',wavefile
	wl = mrdfits(wavefile,exnum,header,/silent)

	if keyword_set(wave2pix) then begin		;input is wavelength
		outputvalue = inputvalue
		for i=0,n_elements(inputvalue)-1 do begin
			temp = abs(inputvalue[i]-wl.wavelength)
			if (min(temp) gt 0.05) then begin	;no close match
				outputvalue[i] = -1
				if not keyword_set(silent) then print,$
					'No match for wavelength ',$
					strcompress(inputvalue[i],/rem),$
					' A in this wavelength file'
			endif else begin
				outputvalue[i] = $
					wl(where(temp eq min(temp))).pixel
			endelse
		endfor
		return,outputvalue
	endif else begin
		return,wl(inputvalue).wavelength	;input is pixel number
	endelse
end
