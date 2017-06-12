function list2list, infile, trange=trange, xrange=xrange, yrange=yrange,$
	prange=prange, scale=scale, phd=phd, fheader=fheader, $
	bheader=bheader,gtiheader,photons=photons,oldstyle=oldstyle, $
	xsegsize=xsegsize,ysegsize=ysegsize,silent=silent, $
	xscaled=xscaled,yscaled=yscaled,gti=gti,alldata=alldata
;+
; NAME:
;	LIST2LIST
;
; PURPOSE:
;	Reads FITS file with binary table extensions. Usually called from
;	READIT rather than run directly. Returns a structure containing the 
;	data.
;
; CATEGORY:
;	FITS
;
; CALLING SEQUENCE:
;	result = LIST2LIST(Filename)
;
; INPUTS:
;	Filename:
;		Input FITS file with photon list data in a binary table
;		extension.
;
; KEYWORD PARAMETERS:
;	NOTE: DO NOT change these without also modifying HIST2HIST,
;	OPUS2HIST, IDF2LIST and LIST2HIST.
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
;	NOTE: The following keyword is not in all routines.
;	PHOTONS:Read only this number of photons from the file. If it is
;		greater than the number of events, read all of the data.
;	OLDSTYLE:Variable used to determine what the form of the structure is.
;	XSEGSIZE:Segment size, as determined from the header.
;	YSEGSIZE:Segment size, as determined from the header.
;	SILENT:	If included, suppress informative messages.
;	XSCALED,YSCALED:
;		If these keywords are present, on return they will contain
;		the value of x and y scaling that was applied to the data.
;		They are probably the values of SPECBINX and SPECBINY or 1,1.
;	GTI:	If present, the Good Time Intervals specified in the data
;		file are returned in this variable.
;	ALLDATA:If present, don't exclude events that fall outside the Good
;		Time Intervals. Specify this keyword if there is no extension
;		containing GTIs (e.g. files from UCB).
;		
; COMMON BLOCKS:
;	LAMSPACE:for IMAGE.PRO.
;		
; MODIFICATION HISTORY:
; 	Written by David Sahnow, based on HIST2LIST, 15 August 1999.
;	18 September 1999 Modified TRANGE keyword to allow multiple ranges.
;	22 October 1999 Fixed a bug which caused all photons to be returned if a
;	 time/x/y/pha range containing no photons was selected.
;	8 December 1999 Added OLDSTYLE, XSEGSIZE, YSEGSIZE keywords for use by
;	 the modified LIST2HIST.
;	30 May 2000 Added SILENT keyword.
;	3 July 2000 Added XSCALED, YSCALED (used by READIT). These
;	 are set equal to SPECBINX/Y if they exist. Otherwise, they are 1.
;	11 January 2001 Added GTI keyword.
;	15-16 January 2001 Added screening on Good Time Intervals. added
;	 ALLDATA keyword.
;	24 February 2003 Now returns scaled data, i.e. x/scale, y/scale,
;	 if the SCALE keyword is set.
;	15 July 2003 Now skips trange checks if time structure element does
;	 not exist. Can read COS MAMA files with no pulse height (oldstyle=4),
;	 or FUV files with pulse height (oldstyle=3).
;	24 July 2003 Fixed bug in printing out trange if more than one
;	 time interval
;	8 August 2003 Added tcheck=0 line outside of loops to prevent cases
;	 where that variable remains undefined.
;	19 January 2004 Added Fixed minor bug in stime[] comparison.
;-

	common lamspace,lambda0,lamslope,space0,spaslope
	
	verbose = 0		;set = 1 to print memory usage

	oldstyle = 0

	if (n_elements(silent) eq 0) then silent = 0

;read the primary header from the FITS file:
	fheader = headfits(infile)

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
	if (keyword_set(photons)) then begin
		result = mrdfits(infile,i,header,range=photons,/fscale,$
			silent=silent)
	endif else begin
		result = mrdfits(infile,i,header,/fscale,silent=silent)
	endelse
	gti = mrdfits(infile,2,silent=silent)
	if (verbose) then memtime,2
	extname = fxpar(header,'EXTNAME')
	if (silent eq 0) then print,extname

	naxis2 = fxpar(header,'NAXIS2')
	tfields = fxpar(header,'TFIELDS')
	if (keyword_set(photons)) then begin	;save number of counts
		numcnts = min([photons,naxis2])
	endif else begin
		numcnts = naxis2		
	endelse
	if (tfmax lt tfields) then tfmax = tfields
	if (namax lt naxis2) then namax = naxis2

	xscaled = fxpar(fheader,'SPECBINX')
	yscaled = fxpar(fheader,'SPECBINY')
	if (xscaled eq 0) then xscaled = 1
	if (yscaled eq 0) then yscaled = 1

	if (silent eq 0) then print
	if (verbose) then memtime,4
	bheader = header

	tcheck=0
	if keyword_set(trange) then begin
		tcheck = 1
		numt = n_elements(trange)/2
		numg = n_elements(gti)
		if keyword_set(alldata) then begin	;ignore GTIs
		endif else begin			;include GTIs
			if (numt eq 1) then trange = reform(trange,numt,2)
			ttrange = dblarr(numt+numg,2)
			ttrange[0:numt-1,*] = trange
			ttrange[numt:numt+numg-1,0] = gti.start
			ttrange[numt:numt+numg-1,1] = gti.stop
			order = sort(ttrange[*,0])
			ttrange = ttrange[order,*]
		endelse

		if (silent eq 0) then begin
			print,'Time range is being limited to:'
			if n_elements(trange) eq 2 then $
				print,trange[0],' to ',trange[1],' seconds' else $
			for j=0,n_elements(trange)/2-1 do begin
;				print,trange[j*2,0],' to ',trange[j*2,1],' seconds'
				print,trange[j,0],' to ',trange[j,1],' seconds'
			endfor
		endif
	endif else begin
		temp = tag_names(result)
		if keyword_set(alldata) then begin	;ignore GTIs
			tcheck = 0
			stime = where(temp eq 'TIME')
			if (stime[0] ge 0) then begin	;added [0] 1/19/04
			trange = minmax(result.time)
				;set trange if there is time data
		endif else begin			;include GTIs
			tcheck = 1
			if (num_ext(infile) gt 1) then begin
				numg = n_elements(gti)
				trange = reform([gti.start,gti.stop],numg,2)
				if (numg ne 1 and silent eq 0) then begin
					print,'Limiting to GTIs: '
					for j=0,numg-1 do begin
						print,trange[j,0],' to ',$
							trange[j,1],' seconds'
					endfor
				endif
			endif
;print,n_elements(trange)
		endelse
		endif
	endelse

;	if not keyword_set(alldata) then begin	;screen using GTIs
;		if (trange ne gti) then begin	;if adjustments are necessary
;		endif else begin
;print,'OK'
;		endelse
;	endif

       if (numcnts ne 0) then begin		;only do stuff if there is data
;	set windows in time, x, y, and pulse height:
	data_fmt = fxpar(bheader,'DATA_FMT')
;	If data_fmt = 0 and data_fmt is an integer, the file may be from OPUS:
	temp = size(data_fmt)
	if (temp(1) eq 2) then begin 
		if (data_fmt eq 0) then $
			data_fmt = $
				strcompress(fxpar(fheader,'INSTMODE'),/remove)
	endif
	rangekeys = n_tags(result)
	names = tag_names(result)
	if (verbose) then memtime,5
	if (data_fmt eq 'XYP_LIST' or data_fmt eq 'XYPT_LIST') then begin
		if (names(2) eq 'Z') then begin		;very old UCB data
			oldstyle = 2
;			result.p = temporary(result.z)
			xsegsize = 32768L
			ysegsize = 32768L
			print,'Cannot read this file - must modify list2hist'
;problem in this section is that result.z must be renamed as result.p somehow.
			stop
		endif
		xsegsize = long(fxpar(bheader,'TMAX1')) + 1L
		ysegsize = long(fxpar(bheader,'TMAX2')) + 1L
	endif
	if (data_fmt eq 'TXYP_LIST') then begin
		if (tcheck eq 0) then begin
			rangekeys = rangekeys - 1
;			trange = minmax(result.time)
		endif
		xsegsize = long(fxpar(bheader,'TMAX2')) + 1L
		ysegsize = long(fxpar(bheader,'TMAX3')) + 1L
		oldstyle = 0
		;if old style (3/97 and before) TXYP files:

		if (names(3) eq 'PULSEHEIGHT') then begin
			oldstyle = 1
;			result.p = temporary(image.pulseheight)
			xsegsize = long(fxpar(fheader,'XSEGSIZE'))
			ysegsize = long(fxpar(fheader,'YSEGSIZE'))
		endif

	endif
	if (data_fmt eq 'TTAG') then begin		;TTAG files from OPUS
		if (tcheck eq 0) then begin
			rangekeys = rangekeys - 1
;			trange = minmax(result.time)
		endif
		xsegsize = 16384L
		ysegsize = 1024L
		oldstyle = 3
	endif
	if (filetype(infile) eq 'COS_NUV_TT') then begin
							;COS NUV - no PHA
		xsegsize = 1024L
		ysegsize = 1024L
		oldstyle = 4
	endif
	if (filetype(infile) eq 'COS_FUV_TT') then begin
							;COS FUV
		xsegsize = 16384L
		ysegsize = 1024L
		oldstyle = 3
	endif

	if (verbose) then memtime,6

	;set keywords, and count the number which require the data to be modified:
	;xrange:
	if (not keyword_set(xrange)) then begin
		xrange = [-1e10,1e10]
	endif
	temp = minmax(result.x)
	if ((xrange(0) gt temp(0)) or (xrange(1) lt temp(1))) then begin
	endif else begin
		rangekeys = rangekeys - 1
	endelse
	if (xrange(0) lt temp(0)) then xrange(0) = temp(0)
	if (xrange(1) gt temp(1)) then xrange(1) = temp(1)
	if (verbose) then memtime,6.1

	;yrange:
	if (not keyword_set(yrange)) then begin
		yrange = [-1e10,1e10]
	endif
	temp = minmax(result.y)
	if ((yrange(0) gt temp(0)) or (yrange(1) lt temp(1))) then begin
	endif else begin
		rangekeys = rangekeys - 1
	endelse
	if (yrange(0) lt temp(0)) then yrange(0) = temp(0)
	if (yrange(1) gt temp(1)) then yrange(1) = temp(1)
	if (verbose) then memtime,6.2

	;prange:
	if (not keyword_set(prange)) then begin
		prange = [-1e10,1e10]
	endif
	if (oldstyle eq 0) then temp = minmax(result.p)
	if (oldstyle eq 1) then temp = minmax(result.pulseheight)
	if (oldstyle eq 2) then temp = minmax(result.x)
	if (oldstyle eq 3) then temp = minmax(result.pha)
	if ((prange(0) gt temp(0)) or (prange(1) lt temp(1))) then begin
	endif else begin
		rangekeys = rangekeys - 1
	endelse
	if (prange(0) lt temp(0)) then prange(0) = temp(0)
	if (prange(1) gt temp(1)) then prange(1) = temp(1)
	if (verbose) then memtime,6.3


	;if there are range keywords set which will trim the data:
	if (rangekeys gt 0) then begin
		if (verbose) then memtime,7
		if (silent eq 0) then print,'Trimming the array'

		if (data_fmt eq 'XYP_LIST') then begin
			good = where( (result.x ge xrange(0)) and $
				(result.x le xrange(1)) and $
				(result.y ge yrange(0)) and $
				(result.y le yrange(1)) and $
				(result.p ge prange(0)) and $
				(result.p le prange(1)) $
				)
			if (verbose) then memtime,7.1
		endif
		if (data_fmt eq 'TXYP_LIST') then begin
			if (oldstyle ne 1) then begin
			good = where( (result.time ge trange(0)) and $
				(result.time le trange(1)) and $
				(result.x ge xrange(0)) and $
				(result.x le xrange(1)) and $
				(result.y ge yrange(0)) and $
				(result.y le yrange(1)) and $
				(result.p ge prange(0)) and $
				(result.p le prange(1)) $
				)
			if (verbose) then memtime,7.1
			endif else begin
			good = where( (result.time ge trange(0)) and $
				(result.time le trange(1)) and $
				(result.x ge xrange(0)) and $
				(result.x le xrange(1)) and $
				(result.y ge yrange(0)) and $
				(result.y le yrange(1)) and $
				(result.pulseheight ge prange(0)) and $
				(result.pulseheight le prange(1)) $
				)
			if (verbose) then memtime,7.2
			endelse
		endif
		if (data_fmt eq 'XYPT_LIST') then begin		;UCB data
			good = where( (result.t ge trange(0)) and $
				(result.t le trange(1)) and $
				(result.x ge xrange(0)) and $
				(result.x le xrange(1)) and $
				(result.y ge yrange(0)) and $
				(result.y le yrange(1)) and $
				(result.p ge prange(0)) and $
				(result.p le prange(1)) $
				)
			if (verbose) then memtime,7.2
		endif
		if (data_fmt eq 'TTAG') then begin	;OPUS data
			if (n_elements(trange) eq 2) then begin
				good = where( (result.time ge trange(0)) and $
					(result.time le trange(1)) and $
					(result.x ge xrange(0)) and $
					(result.x le xrange(1)) and $
					(result.y ge yrange(0)) and $
					(result.y le yrange(1)) and $
					(result.pha ge prange(0)) and $
					(result.pha le prange(1)) $
					)
			endif else begin		;if multiple ranges
				if (silent eq 0) then $
					print,'Using multiple time ranges: '
				tgood = [-1]
				for i=0,(n_elements(trange)/2-1) do begin
					if (silent eq 0) then $
						print,trange[i,0],$
						' to ',trange[i,1]
					tgood = [tgood,where( $
						(result.time ge trange[i,0]) and $
						(result.time le trange[i,1]) $
					)]
				endfor
				tgood = tgood[1:n_elements(tgood)-1]
				good = where( (result[tgood].x ge xrange(0)) and $
					(result[tgood].x le xrange(1)) and $
					(result[tgood].y ge yrange(0)) and $
					(result[tgood].y le yrange(1)) and $
					(result[tgood].pha ge prange(0)) and $
					(result[tgood].pha le prange(1)) $
					)
			endelse
			if (verbose) then memtime,7.2
		endif
		if (verbose) then memtime,8
		if (n_elements(good) gt 1) then begin
			if (n_elements(trange) eq 2) then begin
				if (good[0] ne -1) then result = result(good)
			endif else begin
				if (good[0] ne -1) then result = result(tgood(good))
			endelse
		endif else begin
			if (n_elements(good) ne 0) then begin
				if (good[0] eq -1) then begin
					result = result(0)
					result.x = -1
					result.y = -1
				endif else begin
					result = result(good[0])
					;should also include tgood case (see above)
				endelse
			endif
		endelse

		if (verbose) then memtime,9
	endif

;	modify segment sizes, as appropriate:
	xsegsize = xsegsize / scale
	ysegsize = ysegsize / scale
	result.x = result.x / scale
	result.y = result.y / scale

;	pulse height histogram:
;;	if (keyword_set(phd)) then begin
		if (silent eq 0) then print,'Creating pulse height histogram'
		if (n_elements(result) eq 1) then begin
			phd = intarr(2)
		endif else begin
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
		endelse
;;	endif
       endif else begin		;end of if numcnts loop
	image = fltarr(4,4)
	phd = 0
	scale = findgen(4)
       endelse

;	create some variables for use by IMAGE.PRO
		lambda0 = 0
		lamslope = scale
		space0 = 0
		spaslope = scale

	return, result
;NOTE that scale does not appear to be implemented

end
