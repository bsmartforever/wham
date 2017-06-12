function idf2hist, infile,goodtimes=goodtimes,scale=scale,phd=phd, $
	fheader=fheader, bheader=bheader,photons=photons,$
	silent=silent,xscaled=xscaled,yscaled=yscaled,_extra=e

;+
; NAME:
;	IDF2HIST
;
; PURPOSE:
;	Reads CalFUSE pipeline Intermediate Data File. Usually called from 
;	READIT rather than run directly.
;
; CATEGORY:
;	FITS
;
; CALLING SEQUENCE:
;	result = IDF2HIST(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS file with photon list data in a binary table
;		extension.
;
; KEYWORD PARAMETERS:
;	NOTE: DO NOT change these without also modifying LIST2LIST,
;	 HIST2HIST, IDF2LIST, IDF2HIST and OPUS2HIST.
;	NOTE: THE RANGE, FARF and RAW values are passed via the _EXTRA keyword 
;	 to IDF2LIST. 
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
; 	Written by David Sahnow based on LIST2HIST, 5 April 2003.
;	26 August 2004 Now uses the weight of each photon. The weights
;	 were not returned until IDF2LIST was modified earlier today. Result
;	 is now a float.
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
	result = idf2list(infile,bheader=bheader,fheader=fheader,$
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

;	turn it into a histogram:
	if (verbose) then memtime,10
	if (silent eq 0) then print,'Creating 1D histogram'
	image = hist1dk(fix(temporary(result.x))+fix(result.y)* $
		ceil(xsegsize), result.weight,min=0,$
		max=xsegsize*ysegsize-1L)
	if (verbose) then memtime,11

	if (silent eq 0) then begin
		print,'Turning into a 2D histogram'
		print,' Size is ',xsegsize,' by ',ysegsize,' pixels.'
	endif
	image = reform(image,xsegsize,ysegsize)
	if (verbose) then memtime,12

;	pulse height histogram:
;;	if (keyword_set(phd)) then begin
		if (silent eq 0) then print,'Creating pulse height histogram'
		if (oldstyle eq 3) then begin
			phd = histogram(result.pha,min=0)
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
