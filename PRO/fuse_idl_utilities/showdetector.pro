pro showdetector, infile,binto=binto,pagesize=pagesize, $
	numimages=numimages,landscape=landscape,expose=expose,$
	overexpose=overexpose,nosave=nosave,color=color,pagenum=pagenum,$
	psfile=psfile,inputdata=inputdata,maxc=maxc,minc=minc,$
	siclabel=siclabel,liflabel=liflabel,$
	sicspec=sicspec,lifspec=lifspec,title=title,psnoopen=psnoopen,$
	psnoclose=psnoclose,_extra=e
;+
; NAME:
;	SHOWDETECTOR
;
; PURPOSE:
;	This procedure writes a FUSE detector image to a PostScript file,
;	along with file information such as date acquired, number of counts,
;	etc.
;
; CATEGORY:
;	Detector, Plotting.
;
; CALLING SEQUENCE:
;	SHOWDETECTOR, Infile
;
; INPUTS:
;	Infile:	Filename of the histogram or photon list file to be plotted.
;
; RESTRICTIONS:
;	The IDLPS environment variable should be set to the directory to which
;	the PostScript file will be written. If it does not exist, the
;	file will be written to the current directory.
;
;	If the image to print is large, this program may require a
;	substantial amount of memory.
;	
; KEYWORD PARAMETERS:
;	SICLABEL:A two column file containing labels for the plot. The
;		first column is the label, and the second is the wavelength.
;		These labels are placed below the image.
;	LIFLABEL:Same as SICLABEL, but placed above the image.
;	SICSPEC:Spectrum to label ('LWRS','MDRS','HIRS') for the SiC spectrum.
;		If included, the lines from the labels will be extended to the 
;		approximate y position of this spectrum.
;	LIFSPEC:Same as SICSPEC, but for the LiF labels
;	BINTO:	A two element vector describing the approximate final [x,y]
;		size of the image. Default is [4096,256]. Because the binning
;		is the same in x and Y, the final result may not be exactly
;		what was specified.
;	PAGESIZE:
;		A two element vector describing the [x,y] page size.
;		The Default is [8.5,11]
;	NUMIMAGES:
;		Number of of pieces to break the image into in x. Default
;		is 4.
;	LANDSCAPE:
;		Set if plot should be in landscape mode. This is the
;		default, except for 8.5 x 11.
;	EXPOSE:	If set, adjust the color scale so that the maximum color
;		is set to include 98% of all pixels (excluding pixels with
;		no counts), i.e. the very brightest
;		pixels will be overexposed.
;	OVEREXPOSE:
;		If set, adjust the color scale so that the maximum color
;		is set to 15, so that any pixels brighter than 15 are
;		overexposed.
;	NOSAVE:	If set, don't save the .XDR file. This speeds things up.
;	COLOR:	If set, use color PostScript with the current screen palette.
;	PAGENUM:If set, print on NUMIMAGES pages rather than all on one page,
;		and only page PAGENUM is printed. Note that the first page
;		has PAGENUM = 0.
;	INPUTDATA:
;		If this keyword is set equal to an array containing
;		2D data (such as that created by READIT), this data is used
;		instead of reading Infile. This can be used to save time
;		by reading the data once, and then plotting multiple times.
;		Note that Infile is still used in the title of the plot,
;		and the header information will not be printed on the plot.
;	MINC:	If set, uses this value as the minimum of the color scale.
;	MAXC:	If set, uses this value as the maximum of the color scale.
;		OVEREXPOSE will override this keyword.
;	TITLE:	Use this string as the title of the plot rather than the
;		filename.
;	PSNOOPEN:
;		If set, don't open a new postscript file, but append to
;		the previous one. If a number of plots are to be sent to
;		the same PostScript file, all but the first should include
;		this keyword.
;	PSNOCLOSE:
;		If set, don't close the postscript file when done. If a number
;		of plots are to be sent to the same PostScript file, all but
;		the last should include this keyword.
; The following are passed directly to READIT:
;	PSFILE:	If set to a filename, output goes to this file rather than
;		the default file $IDLPS/idl.ps.
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
;	FARF:	For IDF file format only. If included, read X and Y in the
;		FARF coordinate frame rather than the final X and Y.
;	RAW:	For IDF file format only. If included, read X and Y in the
;		raw coordinate frame rather than the final X and Y.
;	PHA:	For GAIN file format only. Read this pulse height value.
;		If not specified, a pulse height of 8 will be assumed.
; The following are passed directly to SHOWFHEADER:
;	NOCOUNTS:
;		If set, don't display the number of counts.
;	NOINTTIME:
;		If set, don't display the integration time.
;	NOSEGMENT:
;		If set, don't display the segment name.
;	NOROOTNAME:
;		If set, don't display the root name.
;	NOTARGET:
;		If set, don't display the target name.
;	NOID:	If set, don't display the program ID.
;
; OUTPUTS:
;
; SIDE EFFECTS:
;	A PostScript file is generated. If PSFILE is defined, it will be the
;	name of the file. Otherwise, it will be $IDLPS/idl.ps.
;
; EXAMPLE:
;	For the examples below, choose a file to print, e.g.:
;
;	INFILE = '$UCBDATA/Detectors/Flight/FL02/116char4/1A-HDSiCLiF-16'
;
;
;	To print a thumbnail image of a detector image binned to
;	8192x512 on 8.5x11 paper with all pixels containing more
;	then 15 counts set to black, enter:
;
;	SHOWDETECTOR,INFILE,binto=[8192,512],/NOSAVE,/OVEREXPOSE
;
;
;	To print a full size image in four parts on a single 36x36
;	plotter page with the top 2% brightest pixels truncated, use:
;
;	SHOWDETECTOR,INFILE,BINTO=[16384,1024],PAGESIZE=[36,36],/LANDSCAPE,
;		/EXPOSE,/NOSAVE
;	$nawk -f ~/utility/36x36la.awk $IDLPS/idl.ps > $IDLPS/plotter.ps
;
;
;	To print a single full size image on a 288x36 plotter page with the
;	top 2% brightest pixels truncated and with labels every angstrom
;	at the location of the SiC MDRS, use:
;
;	LLIST = '/home/kix/sahnow/linelists/Angstromlist.dat'
;	SHOWDETECTOR,INFILE,BINTO=[16384,1024],PAGESIZE=[36,288],/LANDSCAPE,
;		/EXPOSE,/NOSAVE,NUMIMAGES=1,SICLABEL=LLIST,SICSPEC='MDRS'
;	$nawk -f ~/utility/288x36la.awk $IDLPS/idl.ps > $IDLPS/plotter.ps
;
;	To print a thumbnail image of just the pulse height = 8 data from
;	the file 'gain1A.fit', use:
;
;	SHOWDETECTOR,'gain1A.fit',/NOSAVE,/EXPOSE,PHA=8
;
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 7 November 1997.
;	10 November 1997 Added NOSAVE keyword
;	11 November 1997 Modified to change to/from PS device.
;	1 December 1997 Added COLOR keyword.
;	17 July 1998 Removed $SCRATCH in save line.
;	14 November 1998 Add PAGE keyword. Swapped pagesize if necessary.
;	20 November 1998 Replaced call to INTEGRAL with DSINTEGRAL.
;	7 February 1999 Added PSFILE keyword.
;	5 March 1999 Fixed error with EXPOSE keyword when binning.
;	21 August 1999 Added _extra keyword to use extra readit keywords.
;	22 August 1999 Added INPUTDATA keyword.
;	30 January 2000 Added EXPAND keyword in call to readit to rescale
;	 histogram data back to full size.
;	2 March 2000 Modified output filename so not defining IDLPS doesn't
;	 cause the program to crash.
;	30 May 2000 Added MINC, MAXC keywords.
;	6 September 2000 Removed titles on x and y axes.
;	12 October 2000 Added SICLABEL, LIFLABEL keywords.
;	23 July 2003 Added TITLE, PSNOOPEN, PSNOCLOSE keywords.
;	23 December 2003 Modified closing of PS file to work with Windows.
;	12 January 2004 Added to title for keywords used by READIT.
;-

;Use existing data:
	if keyword_set(inputdata) then begin
		image = inputdata
		bheader = 0
		fheader = 0
	endif else begin
;Or read the data file:
		image = readit(infile,phd=phd,bheader=bheader,$
			fheader=fheader,/expand,xscaled=xscaled,$
			yscaled=yscaled,_extra=e)
	endelse
	temp = size(image)
	xsize = temp(1)
	ysize = temp(2)

;Set keywords that aren't already set:
	if (n_elements(pagesize) ne 2) then pagesize = [8.5,11]
			;default page size is 8.5 x 11.

	if (n_elements(binto) ne 2) then binto = [4096,256]
			;default pixel size is 4096 x 256
	binby = max([xsize/binto[0],ysize/binto[1]])>1

	if (n_elements(numimages) ne 1) then numimages = 4

	if (n_elements(landscape) eq 0) then begin
		if ((pagesize[0] ne 8.5) or (pagesize[1] ne 11))  then begin
			ls = 1
		endif else begin
			ls = 0
		endelse
	endif else begin
			ls = 1
	endelse

	if (ls eq 1) then begin		;make sure xpagesize > ypagesize
		if (pagesize[0] lt pagesize[1]) then $
			pagesize = [pagesize[1],pagesize[0]]
	endif

	if (n_elements(pagenum) ne 1) then pagenum = -1

	if (binby ne 1) then begin	;added if statements 3/5/99
		hg = histogram(rebin(float(image),xsize/binby,ysize/binby)* $
			binby*binby,min=0)
	endif else begin
		hg = histogram(image,min=0)
	endelse
	hg(0) = 0		;exclude pixels with no counts
	if not keyword_set(minc) then minc = 0
	if not keyword_set(maxc) then begin
		if (n_elements(expose) eq 1) then begin
			mc = dsintegral(hg,98)
			maxc = mc 		;* binby * binby
		endif else begin
			hg = histogram(rebin(image*binby*binby,$
				xsize/binby,ysize/binby))
			maxc = dsintegral(hg,100)
		endelse
	endif

	if (n_elements(overexpose) eq 1) then maxc = 15

;	stuff to save in order to speed up future plots:

	if (n_elements(nosave) eq 0) then begin
		print,'Saving data to an .xdr file'
		save,image,infile,phd,fheader,bheader,file='temp.xdr'
			;,goodbot,goodtop,label,label2,pl
	endif

;Read lines to use in labelling plot, if desired:
	if keyword_set(siclabel) then begin
		seg = strcompress(fxpar(fheader,'DETECTOR'),/rem)
		if (seg ne '1A' and seg ne '1B' and seg ne '2A' and $
			seg ne '2B') then begin
			print,'Segment name read from file not valid.'
			seg = ''
			read,'Enter segment name (e.g. 1A): ',seg
		endif
		channel = 'SiC' + seg
		if keyword_set(sicspec) then begin
			print,'Labelling ',channel
			slit = sicspec
			locatespectrum,channel,slit,xledge,xredge,ybot,ytop
			sicy = 0.5*float(ybot+ytop)
		endif else begin	;default to LWRS if not specified
			slit = 'LWRS'
			sicy = 0
		endelse
		print,'Reading SiC label file'
		readcol,siclabel,slabel,swl,format='A,D',/silent
		spix = wlscalef(channel,slit,swl,/wave2pix,/silent)
	endif else begin
		slabel = ''
		spix = 0
	endelse
	if keyword_set(liflabel) then begin
		seg = strcompress(fxpar(fheader,'DETECTOR'),/rem)
		if (seg ne '1A' and seg ne '1B' and seg ne '2A' and $
			seg ne '2B') then begin
			print,'Segment name read from file not valid.'
			seg = ''
			read,'Enter segment name (e.g. 1A): ',seg
		endif
		channel = 'LiF' + seg
		if keyword_set(lifspec) then begin
			print,'Labelling ',channel
			slit = lifspec
			locatespectrum,channel,slit,xledge,xredge,ybot,ytop
			lify = 0.5*float(ybot+ytop)
		endif else begin	;default to LWRS if not specified
			slit = 'LWRS'
			lify = 0
		endelse
		print,'Reading LiF label file'
		readcol,liflabel,llabel,lwl,format='A,D',/silent
		lpix = wlscalef(channel,slit,lwl,/wave2pix,/silent)
	endif else begin
		llabel = ''
		lpix = 0
	endelse


;Now plot:
	if (not keyword_set(psnoopen)) then begin	;open a new PS file?
		outd = getenv('IDLPS')
		set_plot,'ps'
		if (keyword_set(PSFILE)) then begin
			device,/schoolbook,filename=PSFILE,font_size=10,$
				/landscape
		endif else begin
			device,/schoolbook,filename=outd+'idl.ps',font_size=10,$
				/landscape
		endelse
	endif
;Set to color PostScript, and load color table:
	if (n_elements(color) ne 0) then begin
		colorps = 1
		device,/color
		tvlct,r,g,b,/get	;retreive current color table
		rrev = r
		grev = g
		brev = b
		;set background to white, and text to black:
		temp = n_elements(rrev)-1
		rrev(temp) = 0
		grev(temp) = 0
		brev(temp) = 0
		tvlct,rrev,grev,brev
	endif else begin
		colorps = 0
	endelse

	if keyword_set(title) then begin
		title = title 
	endif else begin
		title = infile
; Modify title if desired:
		if keyword_set(e) then begin
			temp = where(strupcase(tag_names(e)) eq 'FARF')
			if (temp[0] ne -1) then $
				title = title + ' (FARF Coordinates)'
			temp = where(strupcase(tag_names(e)) eq 'RAW')
			if (temp[0] ne -1) then $
				title = title + ' (Raw Coordinates)'
			temp = where(strupcase(tag_names(e)) eq 'PHA')
			if (temp[0] ne -1) then $
				title = title + ' (PH = ' + $
					strcompress(fix(e.pha),/rem) + ')'
			temp = where(strmid(strupcase(tag_names(e)),0,2) eq 'TR')
			if (temp[0] ne -1) then $
				title = title + ' (Time = ' + $
					strcompress((e.trange[0]),/rem) + $
					'-'+ strcompress((e.trange[1]),/rem) $
					+ ')'
			temp = where(strmid(strupcase(tag_names(e)),0,2) eq 'XR')
			if (temp[0] ne -1) then $
				title = title + ' (X = ' + $
					strcompress((e.xrange[0]),/rem) + $
					'-'+ strcompress((e.xrange[1]),/rem) $
					+ ')'
			temp = where(strmid(strupcase(tag_names(e)),0,2) eq 'YR')
			if (temp[0] ne -1) then $
				title = title + ' (Y = ' + $
					strcompress((e.yrange[0]),/rem) + $
					'-'+ strcompress((e.yrange[1]),/rem) $
					+ ')'
			temp = where(strmid(strupcase(tag_names(e)),0,2) eq 'PR')
			if (temp[0] ne -1) then $
				title = title + ' (PHA = ' + $
					strcompress((e.prange[0]),/rem) + $
					'-'+ strcompress((e.prange[1]),/rem) $
					+ ')'
		endif
		if keyword_set(raw) then title = title + ' (RAW Coordinates)'
		if keyword_set(pha) then title = title + ' (PH = ' + $
			strcompress(fix(pha)) + ')'
	endelse
	if keyword_set(xscaled) or keyword_set(yscaled) then begin
		if (xscaled+yscaled ne 2) then title = title + ' (expanded'+ $
			strcompress(xscaled) + ' x' + $
			strcompress(yscaled) + ')' 
	endif

	!p.font = 0
	if (ls eq 0) then begin
		plotimage,image,numimages,binby,title,$
			minc=minc,maxc=maxc,$
			xpsize=pagesize[0],ypsize=pagesize[1],$
			xtickformat='(i6)',ytickformat='(i5)',$
			xticks=16,yticks=8,colorps=colorps,$
			page=pagenum,xtitle='',ytitle='',$
			btext1=slabel,bline=spix,bpos=sicy,$
			ttext1=llabel,tline=lpix,tpos=lify
	endif else begin
		plotimage,image,numimages,binby,title,$
			minc=minc,maxc=maxc,$
			xpsize=pagesize[0],ypsize=pagesize[1],$
			xtickformat='(i6)',ytickformat='(i5)',$
			xticks=16,yticks=8,$
			/landscape,colorps=colorps,$
			page=pagenum,xtitle='',ytitle='',$
			btext1=slabel,bline=spix,bpos=sicy,$
			ttext1=llabel,tline=lpix,tpos=lify
	endelse

	if (not keyword_set(inputdata)) then $
		showfheader,0.02,0.95,[fheader,bheader],image,$
			_extra=e

	if (pagesize(0) gt 11) then begin $
		print,'Run the awk script!'
		print,' e.g. nawk -f ~/utility/36x36la.awk idl.ps > plotter.ps'
	endif

	if (not keyword_set(psnoclose)) then begin	;close the PS file?
		device,/close
		if (!version.os_family eq 'unix') then set_plot,'X'
		if (!version.os_family eq 'Windows') then set_plot,'win'
	endif else begin
		plot,[0,1],/nodata,color=!d.n_colors-1
	endelse
	!p.font = -1

end
