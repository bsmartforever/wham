function opus2hist, infile, trange=trange, xrange=xrange, yrange=yrange,$
	prange=prange, scale=scale, phd=phd, fheader=fheader, $
	bheader=bheader,expand=expand,xscaled=xscaled,yscaled=yscaled,$
	silent=silent,float=float
;+
; NAME:
;	OPUS2HIST
;
; PURPOSE:
;	Reads FITS file with histogram data in binary extensions.
;	Differs from mrdfits only in the ability to handle the same
;	keywords used by LIST2HIST. This is a modified version of
;	HIST2HIST which reads files output by OPUS. Usually run from
;	READIT rather than run directly.
;
; CATEGORY:
;	FITS
;
; CALLING SEQUENCE:
;	result = OPUS2HIST(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS file with histogram data as output by OPUS.
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
;	FHEADER:The returned primary HDU FITS header.
;	BHEADER:A string containing the FITS header of the extension containing
;		the data.
;	EXPAND:	If present, scale the number of x and y pixels by SPECBINX
;		and SPECBINY. Flux in each pixel is spread evenly over the
;		new smaller pixels.
;	XSCALED,YSCALED:
;		If these keywords are present, on return they will contain
;		the value of x and y scaling that was applied to the data.
;		They are probably the values of SPECBINX and SPECBINY or 1,1.
;	SILENT:	If included, suppress informative messages.
;	FLOAT:	If included, return a floating point array rather than an
;		integer array.
;	
; COMMON BLOCKS:
;	LAMSPACE:for IMAGE.PRO.
;		
; MODIFICATION HISTORY:
; 	HIST2HIST Written by David Sahnow, 12 August 1997.
;	14 November 1997 Changed to return an empty string for BHEADER.
;	3 November 1998 Modified to OPUS2HIST.
;	4 November 1998 Changed from reading all extensions to just the first.
;	15 November 1998 Changed call to NUMEXT to NUM_EXT.
;	26 March 1999 Now reads all extensions with data and inserts it into
;	 the array. Previously read only the first data extension.
;	24 May 1999 Changed to returned headers so that FHEADER is now the
;	 primary header, and BHEADER is the header of the extension containing
;	 the image.
;	30 January 2000 Added EXPAND keyword to scale input array by
;	 SPECBINX and SPECBINY and restore original pixel ratio. Also
;	 added XSCALED and YSCALED keywords in order to return the amount
;	 of scaling that was done.
;	30 May 2000 Added SILENT keyword.
;	29 December 2000 Added FLOAT keyword.
;-

	common lamspace,lambda0,lamslope,space0,spaslope

	XFULL = 16384
	YFULL = 1024

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

;Read the SIA table and main header:
	siatable = mrdfits(infile,0,header,silent=silent)
	specbinx = fxpar(header,'SPECBINX')
	specbiny = fxpar(header,'SPECBINY')
	xsegsize = XFULL / specbinx
	ysegsize = YFULL / specbiny
	fheader = header

;Read the data, which should be in the first num_ext extensions:
	numext = num_ext(infile)
	if (silent eq 0) then print,'Reading ',numext,' extensions of data'
	if keyword_set(float) then image = fltarr(xsegsize,ysegsize) $
	else image = intarr(xsegsize,ysegsize)
	for extnum = 1,numext do begin
		if keyword_set(float) then $
			subimage = float(mrdfits(infile,extnum,header,/fscale,$
			silent=silent)) $
		else $
			subimage = fix(mrdfits(infile,extnum,header,/fscale,$
			silent=silent))
			;fscale keyword is required with these files,
			; because that's how they convert to unsigned integers.
;SHOULD ADD a warning here if the input file is not an int.
		xorigin = fxpar(header,'XORIGIN')
		yorigin = fxpar(header,'YORIGIN')
		temp = size(subimage)
		if (temp[0] eq 1) then begin
			xsubs = temp[1]-1
			ysubs = 1-1
		endif
		if (temp[0] eq 2) then begin
			xsubs = temp[1]-1
			ysubs = temp[2]-1
		endif
		image[xorigin:xorigin+xsubs,yorigin:yorigin+ysubs] = $
			subimage
			;note that inserting rather than adding avoids double
			; counting of photons (which may appear in multiple
			; extensions
	endfor

;The second and third extensions should contain the stim pulses (which
; may repeat data from the first extension)
;stop
;	if (num_ext(infile) ne 3) then begin
;		print,'OPUS2HIST: The number of extensions is not what is expected'
;		stop
;	endif

;If the EXPAND keyword is set, rescale the data:
	if keyword_set(expand) then begin
		xsegsize = xsegsize * specbinx
		ysegsize = ysegsize * specbiny
		print,'Expanding input image by ',strcompress(specbinx),$
			' x ',strcompress(specbiny)
		image = rebin(float(image),xsegsize,ysegsize)/$
			float(specbinx*specbiny)
		xscaled = specbinx
		yscaled = specbiny
	endif else begin
		xscaled = 1		;if no scaling was done, return
		yscaled = 1		; a scaling of 1. 
	endelse

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
	xsegsize = temp(1)
	ysegsize = temp(2)

	if (silent eq 0) then $
		print,' Size is ',strcompress(xsegsize),$
			' by ',strcompress(ysegsize),' pixels.'
;	fheader = header
	bheader = header
;	bheader = ''		

;	create some variables for use by IMAGE.PRO
		lambda0 = 0
		lamslope = scale
		space0 = 0
		spaslope = scale

	return, image

end
