pro plotimage,image,numimages,binby,title,page=page,_extra=e
;+
; NAME:
;	PLOTIMAGE
;
; PURPOSE:
;	This procedure makes a PostScript file of all or part of an image.
;	
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	PLOTIMAGE, Image, Number, Binning, Title
;
; INPUTS:
;	Image:	The two dimensional image to plot.
;	Number:	The number of pieces to break the image into (in x).
;	Binning:Bin by this factor in both x and y before plotting.
;	Title:	Title of plot
;
; KEYWORD PARAMETERS:
;	PAGE:	By default, this procedure prints the entire image on one
;		page. If PAGE is set to a number other than -1, however, it
;		prints it on Number pages, and only page PAGE is printed on
;		this call.
;	PAGESCALE:
;		If set, individually scale the page to the maximum pixel on
;		that page. By default, the scaling is to the maximum pixel on
;		any page.
;	.......	This procedure also accepts all keywords that ARRAYPS accepts.
;		These include MTITLE, XTITLE, YTITLE, MINC, MAXC, XSCALE,
;		YSCALE, CBAR, SQUARE, LANDSCAPE, XPSIZE, YPSIZE, XMIN,
;		XMAX, YMIN, YMAX, LOG, VERBOSE, ENCAPSULATED, BLINE,
;		BTEXT1, TLINE, TTEXT1, and all keywords that PLOT accepts.
;
; SIDE EFFECTS:
;	Creates a PostScript file.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 3 February 1997.
;	7 February 1997 Changed so that newimage isn't always an integer
;	20 March 1997 Scaled image before rebinning so pixels with few
;	 counts don't disappear in rebinning. Added call to dateit.
;	31 March 1997 Added Title input, replacing use of infile.
;	16 May 1997 Added memtime calls.
;	6 June 1997 Fixed bug which resulted in 'bin by nn' being added
;	 multiple times if routine was called more than once.
;	19 June 1997 Fixed bug introduced in previous bug fix - title
;	 was not printed when no binning was done.
;	20 June 1997 Removed 'x' and 'y' for x and y axis labels in order
;	 to avoid x1,x2, etc. labels on plots
;	14 November 1997 Moved definition of arrtype after rebinning
;	 to avoid integer wrapping problems. Changed all calls to MEMTIME
;	 to occur only if /VERBOSE is set.
;	18 November 1998 Removed /LANDSCAPE in call to ARRAYPS. _EXTRA
;	 keyword should still allow landscape plots to work
;-

	if (keyword_set(verbose)) then memtime,1
	if (keyword_set(page)) then begin
		if (page ge numimages) then begin
			print,'PAGE should be between 0 and ',numimages-1
			stop
		endif
	endif else begin
		if (n_elements(page) eq 0) then page = -1
	endelse

	;rebin by binby:
	temp = size(image)
;	arrtype = temp(3)		moved below, 11/14/97
	xsize = temp(1)
	ysize = temp(2)

	fullxscale = findgen(xsize)
	fullyscale = findgen(ysize)
	xnewsize = xsize / binby
	ynewsize = ysize / binby
	tempimage = rebin(image*(binby*binby),xnewsize,ynewsize)
		;rebin after scaling image so that pixels won't be truncated
	xscale = rebin(fullxscale,xnewsize)
	yscale = rebin(fullyscale,ynewsize)

	temp = size(tempimage)
	arrtype = temp(3)

	if (binby ne 1) then begin
		btitle = title + ' binned by ' + strcompress(fix(binby))
	endif else begin
		btitle = title
	endelse

	if (keyword_set(verbose)) then memtime,2
	;now divide into numimages images
	x2newsize = xnewsize / numimages

	case arrtype of
		1:	newimage = intarr(x2newsize,ynewsize,numimages)
		2:	newimage = intarr(x2newsize,ynewsize,numimages)
		3:	newimage = lonarr(x2newsize,ynewsize,numimages)
		else:	newimage = fltarr(x2newsize,ynewsize,numimages)
	endcase

	xnewscale = fltarr(x2newsize,numimages)
	if (keyword_set(verbose)) then memtime,3
	for i=0L,numimages-1 do begin
		;note that order was swapped so that top plot would be left edge
		newimage(0:x2newsize-1,*,numimages-1-i) = $
			tempimage(i*x2newsize+0:i*x2newsize+x2newsize-1,*)
		xnewscale(0:x2newsize-1,numimages-1-i) = $
			xscale(i*x2newsize+0:i*x2newsize+x2newsize-1)
		if (keyword_set(verbose)) then memtime,4
	endfor

	;plot them using arrayps.pro
	if (page eq -1) then begin
		arrayps,newimage,x2newsize,ynewsize,numimages,1,$
			xtitle='x',ytitle='y',mtitle=btitle,$
			xscale=xnewscale,yscale=yscale,$
			/colorbar,_extra=e
		if (keyword_set(verbose)) then memtime,5.1
	endif else begin
		i = numimages - 1 - page
		if (keyword_set(pagescale)) then begin	;scale to current page
			arrayps,newimage(*,*,i),x2newsize,ynewsize,1,1,$
				xtitle='',ytitle='',mtitle=btitle,$
				xscale=xnewscale(*,i),yscale=yscale,$
				/colorbar,$	;/landscape,removed 11/18/98
				_extra=e,$
				min=min(newimage(*,*,i)),$
				max=max(newimage(*,*,i))
			if (keyword_set(verbose)) then memtime,5.2
		endif else begin		;scale to max on any page
			arrayps,newimage(*,*,i),x2newsize,ynewsize,1,1,$
				xtitle='',ytitle='',mtitle=btitle,$
				xscale=xnewscale(*,i),yscale=yscale,$
				/colorbar,$	;/landscape, removed 11/18/98
				_extra=e,$
				min=min(newimage),max=max(newimage)
			if (keyword_set(verbose)) then memtime,5.3
		endelse
	endelse

	dateit

	if (keyword_set(verbose)) then memtime,6
	return

end
