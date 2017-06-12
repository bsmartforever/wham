function list2hist, infile,goodtimes=goodtimes,scale=scale,phd=phd, $
	fheader=fheader, bheader=bheader,photons=photons,$
	silent=silent,xscaled=xscaled,yscaled=yscaled,_extra=e

;+
; NAME:
;	LIST2HIST
;
; PURPOSE:
;	Reads FITS file with binary table extensions (as saved by
;	CREATELIST or created by detector EGSE). Can read either
;	XYP or TXYP files. Usually called from READIT rather than
;	run directly.
;
; CATEGORY:
;	FITS
;
; CALLING SEQUENCE:
;	result = LIST2HIST(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS file with photon list data in a binary table
;		extension.
;
; KEYWORD PARAMETERS:
;	NOTE: DO NOT change these without also modifying LIST2LIST,
;	 HIST2HIST, IDF2LIST, IDF2HIST and OPUS2HIST.
;	NOTE: THE RANGE values are passed via the _EXTRA keyword to
;	 LIST2LIST. 
;	TRANGE:	An [N,2] element matrix containing the start and ending
;		times of N intervals to include in the array. If N = 1,
;		a simple two element vector, [tstart,tend] may be used.
;	XRANGE:	A two element vector containing the range of x to include
;		in the array.
;	YRANGE:	A two element vector containing the range of y to include
;		in the array.
;	PRANGE:	A two element vector containing the range of pulse height
;		 to include in the array.
;	SCALE:	Scale factor for array. Can be set to make the resultant
;		array a reasonable size. A scale size of n bins the
;		image n x n.
;	PHD:	The returned pulse height distribution.
;	FHEADER:The returned FITS header.
;	BHEADER:The returned FITS header from the binary table extension.
;	NOTE: The following keyword is only in this routine.
;	PHOTONS:Read only this number of photons from the file. If it is
;		greater than the number of events, read all of the data.
;	GOODTIMES:If included, and nonzero, and there is a second binary
;		extension containing good time intervals (such as those
;		created by OPUS), then use only the good times listed.
;	SILENT:	If included, suppress informative messages.
;	XSCALED,YSCALED:
;		If these keywords are present, on return they will contain
;		the value of x and y scaling that was applied to the data.
;		They are probably the values of SPECBINX and SPECBINY or 1,1.
;		
; COMMON BLOCKS:
;	LAMSPACE:for IMAGE.PRO.
;		
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 4 October 1996.
;	10 October 1996 Changed to a function which returns image.
;	27 November 1996 Added keywords, and common block for IMAGE.PRO.
;	6 February 1997 Replaced image(0) = q with REFORM command for turning
;	 1D histogram into 2D image.
;	28 March 1997 Modified to read UCB XYP files in addition to JHU TXYP.
;	 Added PHD keyword
;	8 April 1997 Modified to be backward compatible with old TXYP format.
;	16 May 1997 Forced image to be an integer rather than the long
;	 that HISTOGRAM returns.
;	22 May 1997 Added keywords BHEADER, FHEADER
;	13 August 1997 Modified to always calculate PHD. Previously only
;	 calculated it if KEYWORD_SET(PHD), which was zero if called
;	 with an empty array. Modified xsegsize, ysegsize if scale != 1.
;	3 October 1997 Added verbose variable. Modified logic used in checking
;	 xrange so that rangekeys > 0 only if at least one of the range
;	 keywords requires removing some photons. Commented out the second
;	 call to mrdfits because it doesn't seem to be necessary.
;	11 November 1997 Added the ability to read UCB XYPT files.
;	14 November 1997 Added oldstyle = 2 option for very old XYP files.
;	2 December 1997 Modified to work with files containing zero photons.
;	10 January 1998 Fixed bug in xrange,yrange,prange checks.
;	30 Septempber 1998 Added ability to read OPUS TTAG files. This has
;	 oldstyle=3.
;	4 October 1998 Added check that data_fmt is an integer before 
;	 concluding that the DATA_FMT keyword doesn't exist.
;	13 November 1998 Added PHOTONS keyword.
;	8 April 1999 Added FSCALE keyword to MRDFITS calls.
;	21 August 1999 Now doesn't bomb if no good photons found.
;	12 September 1999 Added GOODTIMES keyword.
;	18 September 1999 Modified TRANGE keyword to allow multiple ranges.
;	8 December 1999 Simplified substantially by using LIST2LIST for
;	 reading the file, and screening for xrange, etc.
;	30 May 2000 Added SILENT keyword.
;	3 July 2000 Added XSCALED, YSCALED keywords.
;	24 February 2003 Removed scaling, since it has now been added to
;	 LIST2LIST.
;	15 July 2003 Added oldstyle=4 for data files with no pulse height, such
;	 as those from the COS MAMA.
;-

	common lamspace,lambda0,lamslope,space0,spaslope
	
	verbose = 0		;set = 1 to print memory usage

	if (n_elements(silent) eq 0) then silent = 0

;set scale factor:
	if keyword_set(scale) then begin
		scale = float(fix(scale))
	endif else begin
		scale = 1.0
	endelse

;	first find out the maximum number of rows & columns in any table:
	tfmax = -1
	namax = -1

	i=1
	if (verbose) then memtime,1
	result = list2list(infile,bheader=bheader,fheader=fheader,$
		oldstyle=oldstyle,xsegsize=xsegsize,ysegsize=ysegsize, $
		scale=scale,silent=silent,xscaled=xscaled,yscaled=yscaled, $
		_extra=e)

	if (n_elements(result) gt 1) then begin 	;only if there is data
	;goodtimes:
	if (not keyword_set(goodtimes)) then goodtimes = 0
	if (goodtimes ne 0) then begin
		gti = mrdfits(infile,2,gheader,/fscale)
		numgti = n_elements(gti)	;number of GTIs
		print,'Using good time intervals:'
		good = where( (result.time ge gti[0].start) and $
			(result.time le gti[0].stop) )
		print,gti[0].start,' to ',gti[0].stop
		if (numgti gt 1) then begin
			for i=1,numgti-1 do begin
				good = [good,where( (result.time ge $
					gti[i].start) and $
					(result.time le gti[i].stop) )]
					print,gti[i].start,' to ',gti[i].stop
			endfor
		endif
		if (n_elements(good) ne 1) then begin
			if (good[0] ne -1) then result = result(good)
		endif
		print
	endif

;removed 2/24/03
;;	modify segment sizes, as appropriate:
;	xsegsize = xsegsize / scale
;	ysegsize = ysegsize / scale

;	turn it into a histogram:
	if (verbose) then memtime,10
	if (silent eq 0) then print,'Creating 1D histogram'
;	image = fix(histogram(fix(temporary(result.x/scale))+fix(result.y/scale)* $
;		ceil(xsegsize/scale), min=0,$
;		max=xsegsize*ysegsize-1L))	;, binsize=scale)
	image = fix(histogram(fix(temporary(result.x))+fix(result.y)* $
		ceil(xsegsize), min=0,$
		max=xsegsize*ysegsize-1L))	;, binsize=scale)
	if (verbose) then memtime,11
;SHOULD the previous line use hist_2d instead?

	if (silent eq 0) then begin
		print,'Turning into a 2D histogram'
		print,' Size is ',xsegsize,' by ',ysegsize,' pixels.'
	endif
	image = reform(image,xsegsize,ysegsize)
	if (verbose) then memtime,12

;	pulse height histogram:
;;	if (keyword_set(phd)) then begin
		if (silent eq 0) then print,'Creating pulse height histogram'
		if (oldstyle eq 3 or oldstyle eq 4) then begin
			if (oldstyle eq 3) then begin
				phd = histogram(result.pha,min=0)
			endif
			if (oldstyle eq 4) then begin
				phd = fltarr(32)	;empty PHD
			endif
		endif else begin
			phd = histogram(result.p,min=0)
		endelse
;;	endif
       endif else begin		;end of if main loop
	image = fltarr(4,4)
	phd = 0
	scale = findgen(4)
       endelse

;	create some variables for use by IMAGE.PRO
		lambda0 = 0
		lamslope = scale
		space0 = 0
		spaslope = scale

	return, image

end
