;+
; NAME:
;	ARRAYPS
;
; PURPOSE:
;	This procedure takes an array array(Xdim,Ydim,Rows,Columns) and
;	displays it in a Postscript plot.
;
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	ARRAYPS, Array, Xdim, Ydim, Rows, Columns
;
;
; INPUTS:
;	Array:	The input array to display. The array may have two
;		(Xdim,Ydim), three (Xdim,Ydim,nnn) dimensions.
;	Xdim::	Size of first dimension of Array.
;	Ydim:	Size of second dimension of Array.
;	Rows:	Number of rows of plots.
;	Columns:Number of columns of plots.
;
;
; KEYWORD PARAMETERS:
;	MTITLE:	A string array containing the titles for each column of plots.
;		mtitle(columns).
;	XTITLE:	The title for each row of plots.
;	YTITLE:	The y axis label (not recommended if Columns > 1).
;	MINC:	Minimum value of array to scale to.
;	MAXC:	Maximum value of array to scale to.
;	XSCALE:	A vector of size Xdim which contains the coordinates for each
;		x pixel. It must be a linear scale. May also be a two
;		dimensional vector, with size (Xdim,Rows*Columns). In this
;		case, each plot can have its own x scale.
;	YSCALE:	A vector of size Ydim which contains the coordinates for each
;		y pixel. It must be a linear scale. May also be a two
;		dimensional vector, with size (Xdim,Rows*Columns). In this
;		case, each plot can have its own y scale.
;	COLORBAR:
;		If set and nonzero, a color bar will be plotted.
;	COLORPS:If set, generate a color PostScript file. If set to an [N,3]
;		vector, use the three axes as R, G, and B; otherwise
;		use the default color table (probably loaded using
;		TVLCT). Does not reverse like in the B&W case.
;		as is (previous
;	SQUARE:	If square=1, square pixels are used. If square is not set, or
;		square=0, the axes are scaled according to XSCALE and YSCALE
;		if they are included. (Square pixels are the default if
;		XSCALE and YSCALE are not used.) Square pixels should
;		be used for plotting spatial vs. wavelength plots. If
;		SQUARE=2, then the page is filled with the plot.
;	LANDSCAPE:
;		Print as a landscape file. Default is 8.5 x 11.
;	XPSIZE:	Sets the x paper size to this value.
;	YPSIZE:	Sets the y paper size to this value.
;	XMIN:	Minimum x value to display.
;	XMAX:	Maximum x value to display.
;	YMIN:	Minimum y value to display.
;	YMAX:	Maximum y value to display.
;	LOG:	If set and nonzero, a logarithmic scale rather than a linear
;		scale will be used.
;	VERBOSE:If set, extra information is printed while the procedure is
;		running.
;	ENCAPSULATED:
;		If present and nonzero, print a smaller image for encapsulation.
;	BLINE:	Wavelength vector of lines to identify at the bottom of the
;		plot.
;	BTEXT1:	String array of text to be used to annotate lines in BLINE. Must
;		be included if BLINE is included.
;	BPOS:	Y position of data labelled by BLINE. If nonzero, line from
;		label will extend to this position.
;	BFONTSIZE:
;		Font size of bottom labels
;	TLINE:	Wavelength vector of lines to identify at the top of the
;		plot.
;	TTEXT1:	String array of text to be used to annotate lines in TLINE.
;		Must be included if TLINE is included.
;	TPOS:	Y position of data labelled by TLINE. If nonzero, line from
;		label will extend to this position.
;	TFONTSIZE:
;		Font size of top labels
;	FITLABEL:
;		Set if labels font should be adjusted to fit.
;	SEPSCALE:
;		If set, separately scale each subplot in Z between zero and the
;		maximum of the subplot before plotting.
;		
;	+ all keywords accepted by PLOT.
;
; SIDE EFFECTS:
;	Sends data to a PostScript file. Note that the file must be opened
;	and closed outside of this function.
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow.
;	9 August 1992 Moved colorbar to before image so overplot will work.
;	8 April 1994 Corrected bug if not called with encapsulated or colorbar.
;	27 May 1996 Added square=2 option.
;	11 February 1997 Added labelling of lines using DSLINEID_PLOT. Still
;	 not using all features of that procedure, however.
;	11 February 1997 Modified to allow XSCALE to be multidimensional.
;	14 February 1997 Changed colorbar so that it has discrete steps for
;	 integer input arrays.
;	21 February 1997 Added XPSIZE, YPSIZE keywords.
;	31 March 1997 Added scaled XRBORD to ensure it equals 1.0 if 
;	 XPSIZE = 72.0.
;	18 June 1997 Added automatic setting of font size, depending on
;	 page size. Forced [XYZ]CHARSIZE to 1.0.
;	18 June 1997 Added _extra keyword to pass keywords to oplot.
;	1 October 1997 Added BPOS, TPOS keywords, and added extend keyword
;	 in call to dslineid_plot. Added BFONTSIZE, TFONTSIZE keywords.
;	3 October 1997 Added FITLABEL keyword.
;	7 November 1997. Changed XRBORD constant.
;	14 November 1997 Changed some of the KEYWORD_SET calls to N_ELEMENTS()
;	 to avoid problems with the keyword being zero (meaning KEYWORD_SET
;	 assumes that the keyword was not passed.
;	21 November 1997 Added COLORPS keyword, requiring modification of 
;	 MYSCALE.
;	23 February 1998 Changed XSIZE and YSIZE for encapsulated PostScript
;	 in order to make the bounding box a reasonable size.
;	25 November 1998 Added SEPSCALE keyword.
;	27 November 1998 Changed YSCALE to match XSCALE and be multidimensional.
;	14 December 1998 Changed SEPSCALE so it doesn't override COLORBAR.
;	8 July 1999 Changed YPAPER to include effects of multiple columns
;	 when using /ENCAPSULATED.
;	8 September 2000 Modified printing of the color bar, which was
;	 incorrect if the MINC or MAXC keywords were set.
;	12 October 2000 Modified to ensure XPFRACT = YPFRACT, i.e. scaling due to
;	 labels, etc. is the same in both directions.
;-
;
function myscale,scale,sminc,smaxc,array,color=color
; Function to return an array to be plotted directly with TV
; Results are similar to tvscale, except that the scaling and min, max are
;  specified directly. The scale is also flipped such that 0 is white instead
;  of black on the display, unless the /color keyword is set to a nonzero
;  value.
	if ((n_elements(color) eq 0)  or (color eq 0)) then begin
		return, scale * (smaxc-((array>sminc)<smaxc))

	endif
	;otherwise:
		return, scale * ((array>sminc)<smaxc)
end
;
;
pro arrayps, array,XDIM,YDIM,rows,columns,xtitle=rowtitle,mtitle=coltitle,$
	ytitle=ltitle,minc=minc,$
	maxc=maxc,xscale=xscale,yscale=yscale,$
	colorbar=cbar,square=sq,landscape=ls,log=logplot,$
	xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,verbose=verbose,$
	encapsulated=encapsulated,$
	tline=tline,ttext1=ttext1,tpos=tpos,tfontsize=tfontsize,$
	bline=bline,btext1=btext1,bpos=bpos,bfontsize=bfontsize,$
	xpsize=xpsize,ypsize=ypsize,fitlabel=fitlabel,colorps=colorps,$
	sepscale=sepscale,_extra=e

	DISPLAYBITS = 8			;for 0 - 255 display device
	MAXVAL = float((2^DISPLAYBITS) - 1)
	device,bits_per_pixel = DISPLAYBITS
	print,'bits per pixel = ',DISPLAYBITS,' (0 -',fix(MAXVAL),')'
	maxrowcol = float(max([rows,columns]))
;	csize = [1.0,1.0,0.75,0.5,0.33333,0.25]
	csize = [1.0,1.0,1.0,1.0,1,0,1.0]	;replaced above line, 6/18/97
	cs = csize(maxrowcol<5)		;scale character size
	temp = size(array)
	vartype = temp(temp(0)+1)	;type code as returned by SIZE().

	if (n_elements(colorps) ne 0) then begin	;use color PostScript
		if (colorps ne 0) then begin
			device,/color
			temp = size(colorps)
			if ((temp(0) eq 2) and (temp(2) eq 3)) then begin
						;if R,G,B have been passed
				tvlct,colorps(*,0),colorps(*,1),colorps(*,2)
						;set color scale
			endif
			clr = 1
		endif else begin
			clr = 0
		endelse
	endif else begin
		clr = 0
	endelse

;Default xmin,xmax,ymin,ymax:
;	if not(keyword_set(xmin)) then xmin = 0
;	if not(keyword_set(xmax)) then xmax = XDIM-1
;	if not(keyword_set(ymin)) then ymin = 0
;	if not(keyword_set(ymax)) then ymax = YDIM-1
	if (n_elements(xmin) eq 0) then xmin = 0
	if (n_elements(xmax) eq 0) then xmax = XDIM-1
	if (n_elements(ymin) eq 0) then ymin = 0
	if (n_elements(ymax) eq 0) then ymax = YDIM-1
;
;
	if not keyword_set(tline) then tline = 0
	if not keyword_set(bline) then bline = 0

;Set x,y plotting scales. Properly adjusts for the fact that pixel numbers are
; centered on each pixel.
	if keyword_set(xscale) then begin
		;if an xscale has been defined
		;if it has multiple dimensions:
		temp = size(xscale)
		numx = temp(0)			;number of xscale dimensions
;Should have a graceful failure if more than two dimensions.
		dxp12 = (xscale(1) - xscale(0)) / 2.0
		if (numx eq 1) then begin		;if one dimension
			xprange = [xscale(xmin)-dxp12,xscale(xmax)+dxp12]
			xrscale = xscale(xmax) - xscale(xmin) + 2.0*dxp12
		endif else begin			;two dimensions
			xprange = fltarr(2,rows*columns)
			for i=0,rows*columns-1 do begin
				xprange(*,i) = $
					[xscale(xmin,i)-dxp12,xscale(xmax,i)+dxp12]
				xrscale = xscale(xmax,0) - xscale(xmin,0) + 2.0*dxp12
			endfor
		endelse
	endif else begin
		;default xscale
		numx = 1
		xprange = [xmin-0.5,xmax+0.5]
		xrscale = xmax-xmin+1
	endelse

	if keyword_set(yscale) then begin
		;if a yscale has been defined
		;if it has multiple dimensions:
		temp = size(yscale)
		numy = temp(0)			;number of xscale dimensions
;Should have a graceful failure if more than two dimensions.
		dyp12 = (yscale(1) - yscale(0)) / 2.0
		if (numy eq 1) then begin		;if one dimension
			yprange = [yscale(ymin)-dyp12,yscale(ymax)+dyp12]
			yrscale = yscale(ymax) - yscale(ymin) + 2.0*dyp12
		endif else begin			;two dimensions
			yprange = fltarr(2,rows*columns)
			for i=0,rows*columns-1 do begin
				yprange(*,i) = $
					[yscale(ymin,i)-dyp12,yscale(ymax,i)+dyp12]
				yrscale = yscale(ymax,0) - yscale(ymin,0) + 2.0*dyp12
			endfor
		endelse
	endif else begin
		;default yscale
		numy = 1
		yprange = [ymin-0.5,ymax+0.5]
		yrscale = ymax-ymin+1
	endelse

;OLD yscale code (before it allowed multiple dimensions) below:
;	if keyword_set(yscale) then begin
;		;if a yscale has been defined
;		dyp12 = (yscale(1) - yscale(0)) / 2.0
;		yprange = [yscale(ymin)-dyp12,yscale(ymax)+dyp12]
;		yrscale = yscale(ymax) - yscale(ymin) + 2.0*dyp12
;	endif else begin
;		;default yscale
;		yprange = [ymin-0.5,ymax+0.5]
;		yrscale = ymax-ymin+1
;	endelse


	if keyword_set(ls) then begin
;		Landscape orientation:
		XPAPER = 11.0			;x size of paper in inches
		YPAPER = 8.5 			;y size of paper in inches
		XLBORD = 1.00			;left x border in inches
		XRBORD = 0.30			;right x border in inches
			;changed from 0.25, 11/7/97
		YTBORD = 0.75			;top y border in inches
		YBBORD = 0.75			;bottom y border in inches
		device,/landscape
		print,'Landscape Mode'
	endif else begin
;		Portrait orientation:
		XPAPER = 8.5 			;x size of paper in inches
		YPAPER = 11.0			;y size of paper in inches
		XLBORD = 1.00			;left x border in inches
		XRBORD = 0.30			;right x border in inches
			;changed from 0.25, 11/7/97
		YTBORD = 0.25			;top y border in inches
		YBBORD = 0.75			;bottom y border in inches
		device,/portrait
		print,'Portrait Mode'
		if (keyword_set(coltitle) and (rows eq 1)) then YTBORD = 0.375
	endelse

;If encapsulated:
	if not(keyword_set(encapsulated)) then encapsulated = 0
	if  (encapsulated) then begin	;newly modified 2/23/98

		XLBORD = 1.00			;left x border in inches
		XRBORD = 0.30			;right x border in inches
		YTBORD = 0.25			;top y border in inches
		YBBORD = 0.75			;bottom y border in inches
		XPAPER = 8.5			;set to a nominal value
		YPAPER = (XPAPER * yrscale / (xrscale*columns)) $
			> ((YTBORD + YBBORD) * 1.5)
				;scale ysize to x size
				;added columns variable above, 7/8/99
				;note that is probably should be scaled by columns *some factor, if columns > 1
	endif

	if keyword_set(xpsize) then begin
		XPAPER = xpsize
		XRBORD = max([XRBORD,XPAPER/54.])
			;changed from ../72., 11/7/97
	endif
	if keyword_set(ypsize) then begin
		YPAPER = ypsize
	endif

;Set font size based on paper size:
	temp = max([XPAPER,YPAPER])		;maximum dimension of plot
	fsize = (2.25 + temp*0.44) > 8.0	;minimum of 8 point
	device,font_size=fsize
	print,'font size is ',fsize


	XSIZE = (XPAPER - XRBORD - XLBORD)	;size of plotting area
	YSIZE = (YPAPER - YBBORD - YTBORD)

	CBARHEIGHT = 0.25		;height of colorbarin inches
	CBARWIDTH = XSIZE-1.0 > 0.5	;width of colorbar in inches

;Set device to be XSIZE x YSIZE with XBORD and YBORD borders:
	if keyword_set(ls) then begin
;		Landscape orientation:
	        device,yoffset=XPAPER-XLBORD,xoffset=YBBORD,xsize=XSIZE,ysize=YSIZE,/inches
	endif else begin
;		Portrait orientation:
	        device,xoffset=XLBORD,yoffset=YBBORD,xsize=XSIZE,ysize=YSIZE,/inches
	endelse

;print,XLBORD,YBBORD
;print,XSIZE,YSIZE
;help,array
;stop

;Now subtract off some small amount for space for titles, etc. between plots
; if more than one plot per page
	if (columns eq 1) then begin
		XPFRACT = 1.0
	endif else begin
		XPFRACT = 0.85
	endelse
	if (rows eq 1) then begin
		YPFRACT = 1.0
	endif else begin
		YPFRACT = 0.85
	endelse
	if (bline[0] ne 0) then begin	;leave extra space if line ids.
		YPFRACT = YPFRACT * 0.85
	endif
	if (tline[0] ne 0) then begin	;leave extra space if line ids.
		YPFRACT = YPFRACT * 0.85
	endif
;new 10/12/00:
	temp = min([XPFRACT,YPFRACT])
	XPFRACT = temp
	YPFRACT = temp
;end of new

;Default titles:
	if not(keyword_set(rowtitle)) then rowtitle = ''
	if not(keyword_set(coltitle)) then coltitle = strarr(columns)
	if not(keyword_set(ltitle)) then ltitle = ''

;Default minc,maxc:
;	if not (keyword_set(minc)) then minc = min(array)
;	if not (keyword_set(maxc)) then maxc = max(array)
	if (n_elements(minc) eq 0) then minc = min(array)
	if (n_elements(maxc) eq 0) then maxc = max(array)
	minc = float(minc)
	maxc = float(maxc)
	print,'Minimum and maximum of scale: ',minc,maxc

	if not(keyword_set(logplot)) then logplot = 0
	
;Take logs if /log flag
; sminc,smaxc = log of minc,maxc if /log; otherwise = minc,maxc
	if (keyword_set(logplot) and logplot) then begin
		if (maxc le 0) then begin
			print,'Maximum value <0; cannot take log'
			return
		endif
		if (minc le 0) then minc = min(array(where(array gt 0)))
			;avoid zeroes
		sminc = alog10(minc)
		smaxc = alog10(maxc)
		array = alog10(array>minc)
	endif else begin
		sminc = minc
		smaxc = maxc
	endelse
	print,'Minimum and maximum of scale: ',sminc,smaxc


	XRANGE = columns      		;number of reads to display
	YRANGE = rows			;number of frames in each image	

        xval = XSIZE / float(XRANGE)	;these are in inches - size of each
        yval = YSIZE / float(YRANGE)	; plotting region

	if keyword_set(sq) then begin
		if (sq eq 1) then begin
			xrscale = xmax-xmin+1
			yrscale = ymax-ymin+1
		endif
		if (sq eq 2) then begin
			xrscale = xval
			yrscale = yval
		endif
	endif

	if keyword_set(verbose) then begin
		print,'x range: ',xprange
		print,'y range: ',yprange
	end

	if keyword_set(verbose) then begin
		print,'xval,yval = ',xval,yval
		print,'xrscale,yrscale = ',xrscale,yrscale
	endif

	if not(keyword_set(cbar)) then cbar = 0 
	if (cbar) then yval = yval - 3.0*CBARHEIGHT/float(YRANGE)
					;leave room for color bar

	if (smaxc ne sminc) then begin
		scale = MAXVAL / (smaxc - sminc)
	endif else begin
		scale = 0.
	endelse
	
;*******************Colorbar**************
;	Plot a color bar at the bottom of the page? : 
;;	if (cbar and (keyword_set(sepscale) eq 0)) then begin
	if (cbar) then begin
		xvalc = CBARWIDTH		;size of color bar in inches
		yvalc = CBARHEIGHT
		if keyword_set(verbose) then begin
			print,' Making color bar'
		endif
;		Create the color bar array:
		if (scale ne 0) then begin
			cbarray = findgen(MAXVAL+1)/scale; + sminc
			if ( (vartype le 4) and (scale ge 1.0) ) then begin
				;if byte, integer, or long and range of counts
				; is small
				;rebin so pixels change every integer at most.
				cbarray = findgen(MAXVAL+1)
				cbarray =round(cbarray/scale)
;				x2 = congrid(cbarray,smaxc-sminc+1)
;				cbarray = congrid(x2,MAXVAL+1)
			endif
		endif else begin
			cbarray = fltarr(MAXVAL+1); + sminc
		endelse

;		Plot the color bar:

;x0,x1,y0,y1 are in normalized coordinates
		x0c = ((XSIZE-xvalc+XRBORD-XLBORD)/2.0) / XSIZE
		y0c = 0.0
		x1c = x0c + xvalc/XSIZE
		y1c = y0c + yvalc/YSIZE
;x00,y00 are in inches
		x00c = x0c * XSIZE
		y00c = y0c * YSIZE
		if keyword_set(verbose) then begin
			print,'(x0c,y0c)   = ',x0c,y0c
			print,'(x00c,y00c) = ',x00c,y00c
		endif
		c12 = (cbarray(1) - cbarray(0)) / 2.0
		xprangec = [cbarray(0)-c12,cbarray(MAXVAL)+c12]
		yprangec = [0,1]
		if ( (vartype le 4) and (scale ge 1.0) ) then begin
			;if byte, integer, or long and range of counts is small
			xprangec = [sminc,smaxc]
		endif

		mincb = min(cbarray)
		maxcb = max(cbarray)
		tv, myscale(scale,mincb,maxcb,cbarray,color=clr),x00c,y00c,$
;		tv, myscale(scale,sminc,smaxc,cbarray,color=clr),x00c,y00c,$
			xsize=xvalc,ysize=yvalc,/inches
		;overplot axes and a title:
				if (keyword_set(sepscale) ne 0) then begin
					!xtitle = 'Relative Counts / Pixel' 
				endif else begin
					!xtitle = 'Counts / Pixel' 
				endelse
;				if (keyword_set(logplot) and (logplot)) then $
;					!xtitle = 'Log '+ !xtitle
				!ytitle = ''                            
				!mtitle = ''                            
		if (keyword_set(logplot) and (logplot)) then begin
			plot_oi,[1,2],[2,1],/noerase,/nodata,$
				xrange=10.0^xprangec,yrange=yprangec,$
				xstyle=1,ystyle=1,$
				xticklen=0.2,yticklen=0,$
				yticks=1,ytickname=[' ',' '],$	;no y annotation
				position=[x0c,y0c,x1c,y1c]
		endif else begin
			plot,[1,2],[2,1],/noerase,/nodata,$
				xrange=xprangec,yrange=yprangec,$
				xstyle=1,ystyle=1,$
				xticklen=0.2,yticklen=0,$
				yticks=1,ytickname=[' ',' '],$	;no y annotation
				position=[x0c,y0c,x1c,y1c]
		endelse
	endif

;***************

;	make box dimensions reflect pixel dimensions (i.e. make pixels square,
;	 scale them to real size, or fill the page):
	if ( abs(xval/xrscale) lt abs(yval/yrscale) ) then $
		yval = (xval/xrscale)*float(yrscale)
	if ( abs(yval/yrscale) lt abs(xval/xrscale) ) then $
		xval = (yval/yrscale)*float(xrscale)
		
	temp = XPFRACT * xval
	xvspace = xval - temp
	xval = temp
	temp = YPFRACT * yval
	yvspace = yval - temp
	yval = temp
	if keyword_set(verbose) then begin
		print,'yval,yvspace,YSIZE = ',yval,yvspace,YSIZE
		print,'xval,xvspace,XSIZE = ',xval,xvspace,XSIZE
	endif

	!ytitle = ltitle				;no room for ytitle
	xcent = (1.0 - (xval +(xval+xvspace)*(XRANGE-1))/XSIZE)/2.0
			;add this to center in x
	zscale = scale					;default z scaling
	zmin = sminc
	zmax = smaxc
	for j = 0,XRANGE-1 do begin
		print,'Plot title for column ',j,' is: ',coltitle(j)
		!mtitle = coltitle(j)	
		for i=YRANGE-1,0,-1 do begin
				;display with first frame at bottom
			if (numx eq 1) then begin
				xrrange = xprange
			endif else begin
				xrrange = xprange(*,i+YRANGE*j)
			endelse
			if (numy eq 1) then begin
				yrrange = yprange
			endif else begin
				yrrange = yprange(*,i+YRANGE*j)
			endelse
			if (i lt YRANGE-1) then !mtitle = ''
				;title only on top plot
			;x0,x1,y0,y1 are in normalized coordinates
			x0 = (xval+xvspace)*j/XSIZE + xcent
			y0 = 1.0 + (yval+yvspace)*(float(i)-YRANGE)/YSIZE
			x1 = x0 + xval/XSIZE
			y1 = y0 + yval/YSIZE
			;x00,y00 are in inches
			x00 = x0 * XSIZE
			y00 = y0 * YSIZE

			if keyword_set(verbose) then begin
				print,'x00,y00 = ',x00,y00
				print,'(x0,y0) = ',x0,y0
				print,'(x1,y1) = ',x1,y1
			end
;			find scale factor for plotting:
			if (keyword_set(sepscale)) then begin
				zmin = 0
				zmax = max(array(xmin:xmax,ymin:ymax,i,j))
				zscale = MAXVAL/zmax
			endif
;			plot array:
			tv, myscale(zscale,zmin,zmax,$
			  array(xmin:xmax,ymin:ymax,i,j),color=clr),$
				x00,y00,$
				xsize=xval,ysize=yval,$
				/inches
			;overplot axes and a title:
			if (strlen(rowtitle) gt 0) then begin
				!xtitle = rowtitle
				if (rows gt 1) then $
					!xtitle = !xtitle + $
						strcompress(string(i))
			endif else begin
				!xtitle = ''                            
			endelse
			print,'   ',!xtitle
			!xtitle = !xtitle
			plot,[1,0],[0,1],/noerase,/nodata,$
				xrange=xrrange,$
				yrange=yrrange,$
				xstyle=1,ystyle=1,$
				ticklen=0.025,charsize=cs,$
				position=[x0,y0,x1,y1],$
				xcharsize=1.0,ycharsize=1.0,zcharsize=1.0,$
				_extra=e
			if (bline[0] ne 0) then begin
				;font size scaling:
				if (n_elements(bfontsize) ne 0) then begin
					lcharsize = bfontsize / fsize	
				endif else begin
					lcharsize = 1
				endelse
				;if lines are to be identified:
				dslineid_plot,bline,btext1,/bottom,$
					extend=bpos,lcharsize=lcharsize,$
					fontsize=fsize,fitlabel=fitlabel
			endif		
			if (tline[0] ne 0) then begin
				;font size scaling:
				if (n_elements(tfontsize) ne 0) then begin
					lcharsize = tfontsize / fsize	
				endif else begin
					lcharsize = 1
				endelse
				;if lines are to be identified:
				dslineid_plot,tline,ttext1,$
					extend=tpos,lcharsize=lcharsize,$
					fontsize=fsize,fitlabel=fitlabel

			endif		
		endfor
qx0 = x0
qy0 = y0
qx1 = x1
qy1 = y1
qx00 = x00
qy00 = y0
	endfor

	return
	end

