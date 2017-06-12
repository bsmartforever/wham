pro wfusettag, ttag, outfile, fheader, bheader, gti, gtiheader, header, $
	addkey=addkey, saddkey=saddkey, faddkey=faddkey, iaddkey=iaddkey, $
	remkey=remkey, new=new
;+
; NAME:
;	WFUSETTAG
;
; PURPOSE:
;	This procedure creates a FUSE raw TTAG file and writes TTAG data
;	to it. The header can include information from a pre-existing header 
;	which is passed, or a brand new header can be generated.
;
; CATEGORY:
;	FITS.
;
; CALLING SEQUENCE:
;	WFUSETTAG, Data, Filename, Fheader [,Header]
;
; INPUTS:
;	Data:	A structure containing the TTAG data to write to the file.
;		The structure should contain standard FUSE TTAG data, i.e.
;		TIME (float), X (INT), Y (INT), and PHA (BYTE). 
;	Fheader:The existing FITS primary header.
;	Bheader:The existing FITS binary table header for the first extension.
;	Filename:
;		Name of output FITS file.
;
; OPTIONAL INPUTS:
;	Gti:	Good Time intervals. If not present, it will be set to include
;		all data.
;	Gtiheader:
;		Header for GTI extension.
;
; KEYWORD PARAMETERS:
;	ADDKEY:	Structure containing keywords to add to or modify in the
;		primary header. The format is a Nx3 string array, but integers 
;		or floats are written as floats. In general, it is better to
;		use a combination of the SADDKEY, FADDKEY, and IADDKEY keywords
;		for more control of the type.
;	SADDKEY:Same as ADDKEY, but all values are written as strings.
;	FADDKEY:Same as ADDKEY, but all values are written as floats, even
;		though the array is a string array.
;	IADDKEY:Same as ADDKEY, but all values are written as ints, even
;		though the array is a string array.
;	REMKEY:	String containing a list of FITS keywords to remove from the
;		primary header.
;	NEW:	If set, ignores fheader and creates a new one from scratch.
;
; OPTIONAL OUTPUTS:
;	Header:	Header for new file.
;
;REDO THIS EXAMPLE
; EXAMPLE:
;	To read in exposure M9980301004 1A, shift the entire spectrum by
;	25 pixels in x, and write it back out as a valid TTAG file
;	called 'modified.fit', do the following:
;
;	dummy = mrdfits('M99803010041attagfraw.fit',0,fheader)
;	data =  mrdfits('M99803010041attagfraw.fit',1,bheader)
;	gti = mrdfits('M99803010041attagfraw.fit',2,gtiheader)
;	data.x = data.x + 25
;	wfusettag, data, 'modified.fit', fheader, bheader, gti, gtiheader
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow based on WFUSEHIST.PRO, 11 January 2001.
;	4 April 2001 Added call to EXPAND_TILDE for outfile.
;	23 October 2002 Added message noting that if NEW keyword is used,
;	 segment is assumed to be 1A. Changed check for a floating point
;	 number to use valid_num().
;	24 November 2004 Added SADDKEY, FADDKEY, IADDKEY keywords. Also
;	 added a space before the comment field.
;-
;
	outfile = expand_tilde(outfile)		;expand tilde if needed

	if keyword_set(new) then begin		;Create a new header 
		print,'Creating a new primary header'
		mkhdr,header	;make a minimal header
		fxaddpar,header,'TELESCOP','FUSE'
		fxaddpar,header,'INSTRUMENT','FUV'
		fxaddpar,header,'FILETYPE','RAW HISTOGRAM'
		fxaddpar,header,'DATEOBS',0
		fxaddpar,header,'NEVENTS',0
		fxaddpar,header,'EXPTIME',0
		fxaddpar,header,'INSTMODE','HIST'
		fxaddpar,header,'DETECTOR','1A'
		fxaddpar,header,'APERTURE','LWRS'
	endif else begin	;Make sure header shows file is a histogram
		fxaddpar,fheader,'FILETYPE','RAW PHOTON ADDRESS'
		fxaddpar,fheader,'INSTMODE','TTAG              '
		header = fheader
	endelse

;	modify some of the keywords in the primary header:
	;date and time:
	get_date,dte
	fxaddpar,header,'DATE',dte,'Date of file creation'
	fxaddpar,header,'SPECBINX',1,'Binsize in detector X coordinate'
	fxaddpar,header,'SPECBINY',1,'Binsize in detector Y coordinate'

	if keyword_set(addkey) then begin
			;add new/modified keywords (convert strings to floats)
		temp = size(addkey)
		if (temp(0) eq 2) then begin
				;if a 2D array (i.e. more than 1 new keyword)
			numwrds = temp(2)
		endif else begin
				;probably a 1D array (only 1 new keyword)
			if (temp(0) ne 1) then begin
				print,'WFUSETTAG: error in ADDKEY'
				stop
			endif
			numwrds = 1
		endelse
		for i=0,numwrds-1 do begin
			;check to see if the parameter value is a number:
			if (valid_num(addkey(1,i),temp) eq 0) then $
				goto, nofloat
			;if a valid number, output the floating point result:
			fxaddpar,header,addkey(0,i),temp,' '+addkey(2,i)
			goto, endcheck
nofloat:
			;if there was an error:
			fxaddpar,header,addkey(0,i),addkey(1,i),' '+addkey(2,i)
endcheck:
		endfor
	endif
	if keyword_set(saddkey) then begin	
				;add new/modified keywords as strings
		temp = size(saddkey)
		if (temp[0] eq 2) then begin
				;if a 2D array (i.e. more than 1 new keyword)
			numwrds = temp[2]
		endif else begin
				;probably a 1D array (only 1 new keyword)
			if (temp[0] ne 1) then begin
				print,'WFUSETTAG: error in SADDKEY'
				stop
			endif
			numwrds = 1
		endelse
		for i=0,numwrds-1 do begin
			fxaddpar,header,saddkey[0,i],$
				strcompress(saddkey[1,i],/rem),' '+saddkey[2,i]
		endfor
	endif
	if keyword_set(faddkey) then begin	
				;add new/modified keywords as floats
		temp = size(faddkey)
		if (temp[0] eq 2) then begin
				;if a 2D array (i.e. more than 1 new keyword)
			numwrds = temp[2]
		endif else begin
				;probably a 1D array (only 1 new keyword)
			if (temp[0] ne 1) then begin
				print,'WFUSETTAG: error in FADDKEY'
				stop
			endif
			numwrds = 1
		endelse
		for i=0,numwrds-1 do begin
			if (valid_num(faddkey[1,i],temp) ne 1) then begin
				print,faddkey[1,i],' is not a valid float'
				print,'Inserting value INVALID'
				temp = 'INVALID' 
			endif
			fxaddpar,header,faddkey[0,i],temp,' '+faddkey[2,i]
		endfor
	endif
	if keyword_set(iaddkey) then begin	
				;add new/modified keywords as ints
		temp = size(iaddkey)
		if (temp[0] eq 2) then begin
				;if a 2D array (i.e. more than 1 new keyword)
			numwrds = temp[2]
		endif else begin
				;probably a 1D array (only 1 new keyword)
			if (temp[0] ne 1) then begin
				print,'WFUSETTAG: error in IADDKEY'
				stop
			endif
			numwrds = 1
		endelse
		for i=0,numwrds-1 do begin
			if (valid_num(iaddkey[1,i],temp,/int) ne 1) then begin
				print,iaddkey[1,i],' is not a valid integer'
				print,'Inserting value INVALID'
				temp = 'INVALID' 
			endif
			fxaddpar,header,iaddkey[0,i],temp,' '+iaddkey[2,i]
		endfor
	endif

	if keyword_set(remkey) then begin	;remove selected keywords
		numwrds = n_elements(remkey)
		for i=0,numwrds-1 do begin
			sxdelpar,header,remkey(i)
		endfor
	endif
	fxaddpar,header,'COMMENT','This file was written by WFUSETTAG.PRO'
	mwrfits,dummy,outfile,header,/create

;Create a binary extension for the data:
	fxbhmake,bheader,n_elements(ttag)
	fxaddpar,bheader,'COMMENT','This file was written by WFUSETTAG.PRO'
	mwrfits,ttag,outfile,bheader		;write the extension

;Create a binary extension for the Good Time Intervals:
	if not keyword_set(gti) then begin		;create GTIs from data
		mm = double(minmax(ttag.time))
		gti = create_struct('START',mm[0],'STOP',mm[1])
	endif
	if not keyword_set(gthiheader) then begin
				;create a binary extension header if necessary
;SHOULD CREATE A minimal header here
	endif
	fxbhmake,gtiheader,n_elements(gti)
	fxaddpar,gtiheader,'COMMENT','This file was written by WFUSETTAG.PRO'
	mwrfits,gti,outfile,gtiheader		;write the extension


return
end
