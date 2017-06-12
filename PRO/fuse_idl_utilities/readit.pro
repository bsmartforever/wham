function readit, infileraw,trange=trange, xrange=xrange, yrange=yrange,$
	prange=prange, scale=scale, phd=phd, fheader=fheader, $
	bheader=bheader,ftype=ftype,photons=photons,error=error,$
	quality=quality, extension=extension, goodtimes=goodtimes,$
	expand=expand,xscaled=xscaled,yscaled=yscaled,silent=silent, $
	float=float,farf=farf,raw=raw,pha=pha
;+
; NAME:
;	READIT
;
; PURPOSE:
;	This function reads FUSE FITS data files, such as
;	the *fraw.fit files created by OPUS, the intermediate files
;	created by the CalFUSE pipeline, and the calibrated 1D data.
;
; CATEGORY:
;	FITS.
;
; CALLING SEQUENCE:
;	Result = READIT(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS filename. It can be in any standard FUSE
;		FITS type.
;
; KEYWORD PARAMETERS:
;	TRANGE:	An [N,2] element matrix containing the start and ending
;		times of N intervals to include in the array. If N = 1,
;		a simple two element vector, [tstart,tend] may be used.
;	XRANGE:	A two element vector containing the range of x to include
;		in the array.
;	YRANGE:	A two element vector containing the range of y to include
;		in the array.
;	PRANGE:	A two element vector containing the range of pulse height
;		 to include in the array. Not relevant for histograms.
;	SCALE:	Scale factor for array. Can be set to make the resultant
;		array a reasonable size. A scale size of n bins the
;		image n x n.
;	PHD:	The returned pulse height distribution. Empty for histograms.
;	FHEADER:The returned FITS header.
;	BHEADER:The returned FITS header from the binary table extension.
;		Equal to FHEADER for histograms
;	PHOTONS:Reads only this number of photons from the data file. This
;		works only for photon lists files.
;	FTYPE:	Set this to a type of image to override file type returned
;		by filetype(). Valid types include:
;			TTAG		OPUS time tag
;			HIST		OPUS spectral image mode
;			PIPELINE	Science pipeline image
;			FES		FES file
;			TTGD		TTGD file from pipeline (before v3.0)
;			IDF		IDF file from pipeline (v3.0 and later)
;			COUNT		FUSE accumulated count map file (v3.0) 
;			GAIN		FUSE accumulated gain map file (v3.0) 
;			TCNT
;			PHAMEAN
;			XYPT_LIST
;			FLAT		CalFUSE pipeline calibration file
;			BKG		CalFUSE pipeline calibration file
;			GEOM		CalFUSE pipeline calibration file
;			ONED		Calibrated one-dimensional data		
;			XYP_LIST	UCB GSE photon list
;			TXYP_LIST	UCB GSE photon list with time
;			COUNT_IMAGE	UCB GSE histogram
;			GAIN_IMAGE	UCB GSE gain histogram
;			COS_NUV_TT	COS NUV time tag
;			COS_NUV_ACCUM	COS NUV ACCUM
;			COS_FUV_TT	COS FUV time tag
;			COS_FUV_ACCUM	COS FUV ACCUM
;			
;	ERROR:	If included, and the file is a pipeline calibration file,
;		load the error array (2nd extension) rather than the data
;		array (1st extension).
;	QUALITY:If included, and the file is a pipeline calibration file,
;		load the quality array (3rd extension) rather than the data
;		array (1st extension).
;	EXTENSION:If set, this extension will be read as a two dimensional
;		image. Note that this will only worked for unrecognized
;		file types.
;	GOODTIMES:If set, and the file is an OPUS TTAG file, data that is not
;		in a good time interval (as listed in the second binary
;		extension) is not included in the returned structure.
;	EXPAND:	If present, scale the number of x and y pixels in a histogram
;		by SPECBINX and SPECBINY. Flux in each pixel is spread evenly 
;		over the new smaller pixels.
;	XSCALED,YSCALED:
;		If these keywords are present, on return they will contain
;		the value of x and y scaling that was applied to the data.
;		They are probably the values of SPECBINX and SPECBINY or 1,1,
;		and are returned by OPUS2HIST or HIST2HIST.
;	SILENT:	If included, suppress informative messages.
;	FLOAT:	If included, return a floating point array rather than an
;		integer array (for OPUS2HIST only).
;	FARF:	For IDF file format only. If included, read X and Y in the
;		FARF coordinate frame rather than the final X and Y.
;	RAW:	For IDF file format only. If included, read X and Y in the
;		raw coordinate frame rather than the final X and Y.
;	PHA:	For GAIN file format only. Read this pulse height value.
;		If not specified, a pulse height of 8 will be assumed.
;		Note that to get a PHA of 0, a small but nonzero value must
;		be used, e.g. 0.1.
;
; OUTPUTS:
;	Returns a two dimensional array containing the data.
;
; COMMON BLOCKS:
;	LAMSPACE:for IMAGE.PRO.
;
; PROCEDURE:
;	Calls FILETYPE to determine the file type, then calls LIST2HIST,
;	HIST2HIST, or OPUS2HIST, etc. as appropriate.
;	
;
; EXAMPLE:
;	To read the data from a 'raw' data file into the two dimensional
;	array data[]:
;
;	IDL> data = readit(infile)
;
;	To read a time tag file and return its FITS headers and pulse height
;	distribution for plotting:
;
;	IDL> data = readit(infile,fheader=fheader,bheader=bheader,phd=phd)
;	IDL> plot, phd, psym=10		;to plot the pulse height distribution
;	IDL> print, fheader, bheader	;to display the FITS headers.
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 12 August 1997.
;	14 November 1997 Added 'COUNT_IMAGE' option to ftype
;	4 December 1997 Now assumes ftype = UNDEFINED is a histogram
;	3 March 1998 Added FTYPE keyword to override default.
;	30 September 1998 Added TTAG ftype (from OPUS output).
;	1 October 1998 Added HIST ftype (from OPUS output).
;	3 November 1998 Added list=2 type, which calls OPUS2HIST.
;	6 November 1998 Added ftype = PIPELINE.
;	13 November 1998 Added PHOTON keyword.
;	8 April 1999 list=4 and list=5 type for pipeline calibration files.
;	11 June 1999 Added support for FES files. Added EXTENSION keyword.
;	12 September 1999 Added GOODTIMES keyword.
;	18 September 1999 Modified keyword parameter description for TRANGE.
;	29 November 1999 Modified to trap ftype of UNDEFINED.
;	1 November 1999 Added ftype of TCNT (total count rate file).
;	30 January 2000 Added EXPAND keyword, which gets passed to OPUS2HIST
;	 and HIST2HIST.
;	17 February 2000 Now prints out the FILETYPE in the error message
;	 noting that file can't be read.
;	30 May 2000 Added SILENT keyword.
;	20 June 2000 Added TTGD ftype.
;	6 July 2000 Now prints out filename if file not found
;	6 October 2000 Added ONED ftype.
;	1 November 2000 Set xscaled, yscaled = 1 for list = 3,4,5,6.
;	4 November 2000 Now uses EXPAND_TILDE to allow the use of the
;	 tilde in the input filename.
;	29 December 2000 Added FLOAT keyword.
;	14 June 2001 Added PHAMEAN ftype.
;	6 April 2003 Added IDF ftype
;	14 July 2003 Removed stop if file not found.
;	15 July 2003 Added four COS file types. Added list=9 for files with
;	 TTAG data and no GTI data. COS_FUV_ACCUM type has not been tested yet.
;	29 July 2003 Now exits gracefully if file not found.
;	7 September 2003 Set xscaled=1 for list=0. Added COUNT file type.
;	12 January 2004 Added GAIN file type. Added PHA keyword to support it.
;	15 Added rounding to PHA keyword to permit plotting of PH=0.
;-
 
	common lamspace,lambda0,lamslope,space0,spaslope

	infile = expand_tilde(infileraw)

	if (n_elements(silent) eq 0) then silent = 0

;First determine what type of file we want to read, unless FTYPE is set:
	if (keyword_set(ftype) eq 0) then ftype = filetype(infile,silent=silent)

;Now use either LIST2HIST, HIST2HIST or OPUS2HIST directly depending on the
;	file type:
	;list = 0: image in primary data unit
	;list = 1: photon list in first binary extension, good time intervals
	; 		in second extension
	;list = 2: image in multiple binary extensions
	;list = 3: pipeline file
	;list = 4: pipeline calibration image file with three extensions
	;		(data, errors, quality)
	;list = 5: pipeline calibration image file with two extensions
	;		(x shift, y shift)
	;list = 6: image in first binary extension
	;list = 7: calibrated data in first binary extension
	;list = 8: CalFUSE 3.0 Intermediate Data File format
	;list = 9: photon list in first and only binary extension
	;list = 10; 32 extensions for pulse heights 0 - 31.
print,ftype
	case ftype of
		'GAIN_IMAGE':	list = 0
		'COUNT_IMAGE':	list = 0
		'TCNT':		list = 0
		'PHAMEAN':	list = 0
		'COUNT':	list = 0
		'TXYP_LIST':	list = 1
		'TTAG':		list = 1
		'HIST':		list = 2
		'PIPELINE':	list = 3
		'FLAT':		list = 4
		'BKG':		list = 4
		'TTGD':		list = 4
		'GEOM':		list = 5
		'FES':		list = 6
		'COS_FUV_ACCUM':list = 6
		'COS_NUV_ACCUM':list = 6
		'ONED':		list = 7
		'IDF':		list = 8
		'XYP_LIST':	list = 9
		'XYPT_LIST':	list = 9
		'COS_FUV_TT':	list = 9
		'COS_NUV_TT':	list = 9
		'GAIN':		list = 10
		'UNDEFINED':	list = 0
		'FILE_NOT_FOUND': begin
			print,'File ',infile,' not found! Exiting.'
			goto, endnow
		end
	ELSE: begin
		list = -1
		end
	endcase

;Populate keywords which were not used (can't use _extra since they
; are only passed into, and not out of routines):
	if not keyword_set(trange) then trange = [-1e10,1e10]
	if not keyword_set(xrange) then xrange = [-1e10,1e10]
	if not keyword_set(yrange) then yrange = [-1e10,1e10]
	if not keyword_set(prange) then prange = [-1e10,1e10]
	if not keyword_set(scale) then scale = 1.0
	if not keyword_set(extension) then extension = 0
				;read primary data by default
	if not keyword_set(goodtimes) then goodtimes = 0
	if not keyword_set(expand) then expand = 0

;Read the file:
	case list of
		0: begin		;histogram in primary data unit
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=extension,expand=expand,$
				silent=silent)
			if (n_elements(result) eq 1) then begin
						;likely an error
				print,'Filetype is ',fxpar(fheader,'FILETYPE')
				print,'READIT probably cannot read this file!'
				print,'READIT only reads detector and FES files!'
				ans = ''
				read,'Continue anyway (y or n)? ',ans
				if (ans ne 'y' or ans ne 'Y') then goto, endnow
			endif
			xscaled = 1
			yscaled = 1
		end
		1: begin		;photon list in first binary extension
			if (keyword_set(photons)) then begin
				result = list2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					photons=photons,goodtimes=goodtimes,$
					silent=silent,xscaled=xscaled,$
					yscaled=yscaled)
			endif else begin
				result = list2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					goodtimes=goodtimes,silent=silent,$
					xscaled=xscaled,yscaled=yscaled)
			endelse
		end
		2: begin		;OPUS histogram file
			result = opus2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				expand=expand,xscaled=xscaled,$
				yscaled=yscaled,silent=silent,float=float)
		end
		3: begin		;histogram in primary data unit
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=1,expand=expand,silent=silent)
			xscaled = 1
			yscaled = 1
		end
		4: begin		;histograms in first 3 extensions
			ext = 1
			if (keyword_set(error)) then ext = 2
			if (keyword_set(quality)) then ext = 3
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=ext,expand=expand,$
				silent=silent)
			xscaled = 1
			yscaled = 1
		end
		5: begin		;histogram in first 2 extensions
			ext = 1
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=ext,expand=expand,$
				silent=silent)
			xscaled = 1
			yscaled = 1
		end
		6: begin		;histogram in first extension
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=1,expand=expand,$
				silent=silent)
			xscaled = 1
			yscaled = 1
		end
		7: begin		;calibrated 1D data in first extension
			result = mrdfits(infile,1,silent=silent)
		end
		8: begin		;IDF files from pipeline v3.0 and later
			if not keyword_set(farf) then farf = 0
			if not keyword_set(raw) then raw = 0
			if (keyword_set(photons)) then begin
				result = idf2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					photons=photons,goodtimes=goodtimes,$
					silent=silent,xscaled=xscaled,$
					yscaled=yscaled,farf=farf,raw=raw)
			endif else begin
				result = idf2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					goodtimes=goodtimes,silent=silent,$
					xscaled=xscaled,yscaled=yscaled,$
					farf=farf,raw=raw)
			endelse
		end
		9: begin		;photon list in first binary extension
			if (keyword_set(photons)) then begin
				result = list2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					photons=photons,goodtimes=goodtimes,$
					silent=silent,xscaled=xscaled,$
					yscaled=yscaled,$
					/alldata)
			endif else begin
				result = list2hist(infile,trange=trange,$
					xrange=xrange, yrange=yrange,$
					prange=prange, scale=scale, phd=phd, $
					fheader=fheader,bheader=bheader,$
					goodtimes=goodtimes,silent=silent,$
					xscaled=xscaled,yscaled=yscaled,$
					/alldata)
			endelse
		end
		10: begin		;32 extensions, one for each PHD bin
			if not keyword_set(pha) then pha = 8.
			pha = round(pha)
			print,'Pulse Height = ',pha
			result = hist2hist(infile,trange=trange,$
				xrange=xrange,yrange=yrange,$
				prange=prange,scale=scale,phd=phd, $
				fheader=fheader,bheader=bheader,$
				extension=pha+1,expand=expand,$
				silent=silent)
		end
	ELSE: begin
		print,'Cannot recognize the file type.'
		print,'If you are reading a valid FUSE or COS file type,'
		print,'contact David Sahnow for an updated version.'
		end
	endcase

	return,result

endnow:				;if user aborts 
	return,0

end
