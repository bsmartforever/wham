pro wfusehist, image, outfile, fheader, header, addkey=addkey, remkey=remkey, $
	new=new, ignoretype=ignoretype
;+
; NAME:
;	WFUSEHIST
;
; PURPOSE:
;	This procedure creates a FUSE raw histogram file and writes a histogram
;	to it. The header can include information from a pre-existing header 
;	which is passed, or a brand new header can be generated. It creates
;	a dummy of SIA table which includes the entire detector.
;
; CATEGORY:
;	FITS.
;
; CALLING SEQUENCE:
;	WFUSEHIST, Histogram, Filename, Fheader [,Header]
;
; INPUTS:
;	Histogram:
;		The histogram to write to the file.
;	Fheader:The existing FITS primary header.
;	Filename:
;		Name of output FITS file.
;
; KEYWORD PARAMETERS:
;	ADDKEY:	Structure containing keywords to add to the primary header.
;		The format is a Nx3 string array. This procedure converts
;		numbers to strings before writing the file.
;	REMKEY:	String containing a list of FITS keywords to remove from the
;		primary header.
;	NEW:	If set, ignores fheader and creates a new one from scratch.
;	IGNORETYPE:
;		By default, if the data type being written is not a BYTE,
;		INT, or LONG, the program stops and waits for user input
;		before writing the file. This keyword allows the file to
;		be written without waiting for this acknowledgement.
;
; OPTIONAL OUTPUTS:
;	Header:	Primary header for new file.
;
; EXAMPLE:
;	To read in exposure I818020701 1A, shift the entire spectrum by
;	25 pixels in x, and write it back out as a valid histogram file
;	called 'modified.fit', do the following:
;
;	data = readit('I81802070011ahistfraw.fit',fheader=fheader)
;	data = shift(data,25,0)
;	wfusehist, data, 'modified.fit', fheader
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow based on WHISTFITS.PRO, 4 February 2000.
;	7 February 2000 Added check that input data is a BYTE, INT, or LONG.
;	3 May 2000 Corrected documentation.
;	24 May 2000 Replaced call to WRITEFITS with MWRFITS when creating
;	 the FITS file.
;	6 September 2000 Now forces FILETYPE and INSTMODE keywords to be
;	 those appropriate for a histogram.
;	11 January 2001 Fixed a bug which caused a crash if ADDKEY contained
;	 only one keyword.
;	4 April 2001 Added call to EXPAND_TILDE for outfile.
;	23 October 2002 Added message noting that if NEW keyword is used,
;	 segment is assumed to be 1A. Changed check for a floating point
;	 number to use valid_num().
;	5 September 2003 Added time to DATE keyword value.
;	19 April 2004 Added IGNORETYPE keyword. Added a space before the
;	 comment text when using the ADDKEY keyword.
;-
;
	siatable = bytarr(8,64) + byte(1)		;dummy SIA table

	outfile = expand_tilde(outfile)		;expand tilde if needed

	temp = size(image)
	specbinx = 16384 / temp[1]
	specbiny = 1024 / temp[2]
	if ((temp[3] lt 1) or (temp[3] gt 3)) then begin
			;if not a byte, int, or long array
		print,'Invalid data type for creating a FUSE histogram file.'
		print,'Must be a BYTE, INT, or LONG array.'
		if keyword_set(ignoretype) then begin
			print,'Writing data anyway'
		endif else begin
			print,'Type ".con" to continue anyway.'
			stop
		endelse
	endif
	if keyword_set(new) then begin		;Create a new header 
		print,'Creating a new primary header'
		mkhdr,header,siatable	;make a minimal header using the data
		fxaddpar,header,'TELESCOP','FUSE'
		fxaddpar,header,'INSTRUMENT','FUV'
		fxaddpar,header,'FILETYPE','RAW HISTOGRAM'
		fxaddpar,header,'DATEOBS',0
		fxaddpar,header,'NEVENTS',0
		fxaddpar,header,'EXPTIME',0
		fxaddpar,header,'INSTMODE','HIST'
		fxaddpar,header,'DETECTOR','1A'
		print,'Assuming Segment 1A'
		fxaddpar,header,'APERTURE','LWRS'
	endif else begin	;Make sure header shows file is a histogram
		fxaddpar,fheader,'FILETYPE','RAW HISTOGRAM     '
		fxaddpar,fheader,'INSTMODE','HIST              '
		header = fheader
	endelse

	;make sure NAXISi and BITPIX keywords are correct, and modify if needed:
	dscheck_fits,siatable,header,/update

;	modify some of the keywords in the primary header:
	;date and time:
	get_date,dte,/time
	fxaddpar,header,'DATE',dte,'Date of file creation'
	fxaddpar,header,'SPECBINX',specbinx
	fxaddpar,header,'SPECBINY',specbiny

	if keyword_set(addkey) then begin	;add new/modified keywords
		temp = size(addkey)
;		numwrds = temp(2)
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
;		on_ioerror, nofloat		;if the value is not a number
		for i=0,numwrds-1 do begin
			;check to see if the parameter value is a number:
			if (valid_num(addkey(1,i),temp) eq 0) then $
				goto, nofloat
;			temp = float(addkey(1,i))
			;if no error, output the floating point result:
;print,'float ',addkey(0,i),' ',addkey(1,i)
			fxaddpar,header,addkey(0,i),temp,' '+addkey(2,i)
			goto, endcheck
nofloat:
;print,'nofloat ',addkey(0,i),' ',addkey(1,i)
			;if there was an error:
			fxaddpar,header,addkey(0,i),addkey(1,i),' '+addkey(2,i)
endcheck:
		endfor
;		on_ioerror,null
	endif

	if keyword_set(remkey) then begin	;remove selected keywords
		numwrds = n_elements(remkey)
		for i=0,numwrds-1 do begin
			sxdelpar,header,remkey(i)
		endfor
	endif
	fxaddpar,header,'COMMENT','This file was written by WFUSEHIST.PRO'
;stop
;	write the SIA table and header:
;	writefits,outfile,siatable,header
	mwrfits,siatable,outfile,header,/create

;Create a binary extension:
	comm1 = 'Created by ...'
	fxbhmake,bheader,n_elements(wavelength),$
		'HISTOGRAM','name of this extension',/initialize									
;SHOULD ADD all the world coordinate stuff here

	fxaddpar,bheader,'XORIGIN',0,'offset of this region in the x dimension'
	fxaddpar,bheader,'YORIGIN',0,'offset of this region in the y dimension'
	fxaddpar,bheader,'RCOUNT',fix(total(siatable)),$
		'count of rectangles in this region'
	minval = min(image)
	maxval = max(image)
	fxaddpar,bheader,'MINVAL',minval,'minimum value within rectangles'
	fxaddpar,bheader,'MAXVAL',maxval,'maximum value within rectangles'
	fxaddpar,bheader,'COMMENT','This file was written by WFUSEHIST.PRO'

;	maxint = 32767
;	if (maxval gt maxint) then begin
;		print,'MAXVAL > ',maxint,'. Truncating. Data may not be valid!'
;		image(where(image gt maxint)) = maxint
;	endif

	mwrfits,image,outfile,bheader		;write the extension
		;can't get /iscale or /lscale flags to work correctly,
		; so I added the truncation above.


return
end
