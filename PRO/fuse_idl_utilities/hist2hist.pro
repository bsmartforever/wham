function hist2hist, infile, trange=trange, xrange=xrange, yrange=yrange,$
	prange=prange, scale=scale, phd=phd, fheader=fheader, $
	bheader=bheader,extension=extension,expand=expand,silent=silent
;+
; NAME:
;	HIST2HIST
;
; PURPOSE:
;	Reads FITS file with histogram data in the primary data unit
;	(or any extension in the EXTENSION keyword).
;	Differs from mrdfits only in the ability to handle the same
;	keywords used by LIST2HIST. Usually called from
;	READIT rather then run directly.
;
; CATEGORY:
;	FITS
;
; CALLING SEQUENCE:
;	result = HIST2HIST(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS file with histogram data in the primary data
;		unit.
;
; KEYWORD PARAMETERS:
;	NOTE: DO NOT change these without also modifying LIST2HIST.
;	TRANGE:	A two element vector containing the start and ending
;		times to include in the array.
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
;	BHEADER:A null string. Included for compatibility with LIST2HIST.
;	EXTENSION:
;		Extension where the data is found. If not present, assumes
;		the data is in the primardy data unit.
;	EXPAND:	If present, scale the number of x and y pixels by SPECBINX
;		and SPECBINY. Flux in each pixel is spread evenly over the
;		new smaller pixels.
;	SILENT:	If included, suppress informative messages.
;		
; COMMON BLOCKS:
;	LAMSPACE:for IMAGE.PRO.
;		
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 12 August 1997.
;	14 November 1997 Changed to return an empty string for BHEADER.
;	6 November 1998 Added EXTENSION keyword.
;	8 April 1999 Added FSCALE keyword to MRDFITS call.
;	30 January 2000 Added EXPAND keyword to scale input array by
;	 SPECBINX and SPECBINY and restore original pixel ratio.
;	19 April 2000 Added defaults values of SPECBINX, SPECBINY.
;	30 May 2000 Added SILENT keyword.
;	20 June 2000 Modified fheader to return the primary header, even
;	 if another extension is being read, and bheader to return the
;	 header of the extension being read.
;	20 March 2001 Replaced temp(*) with temp[*] in xsegsize,ysegsize
;	 assignment.
;-

	common lamspace,lambda0,lamslope,space0,spaslope

	oldstyle = 0

	if (n_elements(silent) eq 0) then silent = 0

;these keywords which return values don't apply for histograms:
	phd = 0
	bheader = ''

;set scale factor:
	if keyword_set(scale) then begin
		scale = float(fix(scale))
	endif else begin
		scale = 1.0
	endelse

;check for the extension keyword:
	if keyword_set(extension) then begin
		extension = fix(extension)
	endif else begin
		extension = 0
	endelse

;Read the data:
	if (extension ne 0) then fheader = headfits(infile)
		;read the primary header
	if (silent eq 0) then print,'Reading extension ',extension
	image = mrdfits(infile,extension,header,/fscale,silent=silent)
	if (extension eq 0) then fheader = header
	bheader = header	

	xsegsize = fxpar(header,'NAXIS1')
	ysegsize = fxpar(header,'NAXIS2')
	
;If the EXPAND keyword is set, rescale the data:
	if keyword_set(expand) then begin
		theader = headfits(infile)
		specbinx = fxpar(theader,'SPECBINX')
		specbiny = fxpar(theader,'SPECBINY')
		if (specbinx eq 0) then specbinx = 1	;added 4/19/00
		if (specbiny eq 0) then specbiny = 1	;added 4/19/00
		xsegsize = xsegsize * specbinx
		ysegsize = ysegsize * specbiny
		print,'Expanding input image by ',strcompress(specbinx),$
			' x ',strcompress(specbiny)
		image = rebin(float(image),xsegsize,ysegsize)/$
			float(specbinx*specbiny)
	endif

	if (silent eq 0) then print

;	set windows in x and y:
	xr = [0,xsegsize-1]
	yr = [0,ysegsize-1]
	if (keyword_set(xrange)) then xr = xrange
	if (keyword_set(yrange)) then yr = yrange
	xr(0) = xr(0) > 0
	xr(1) = xr(1) < (xsegsize-1)
	yr(0) = yr(0) > 0
	yr(1) = yr(1) < (ysegsize-1)

;	clear data outside chosen region:
	if (xr(0) ne 0) then image(0:xr(0)-1,*) = 0
	if (xr(1) ne (xsegsize-1)) then image(xr(1)+1:xsegsize-1,*) = 0
	if (yr(0) ne 0) then image(*,0:yr(0)-1) = 0
	if (yr(1) ne (ysegsize-1)) then image(*,yr(1)+1:ysegsize-1) = 0
	
;rescale if necessary:
	if (scale ne 1.0) then begin
		print,'Scaling image'
		image = rebin(image*(scale*scale),xsegsize/scale,ysegsize/scale)
	endif
	temp = size(image)
	xsegsize = temp[1]
	ysegsize = temp[2]

	if (silent eq 0) then $
		print,' Size is ',strcompress(xsegsize),$
		' by ',strcompress(ysegsize),$
		' pixels.'
;	fheader = header
;	bheader = ''		

;	create some variables for use by IMAGE.PRO
		lambda0 = 0
		lamslope = scale
		space0 = 0
		spaslope = scale

	return, image

end
