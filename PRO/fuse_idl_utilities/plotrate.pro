pro plotrate, infile, data, timearray, ratearray, timestep=timestep, $
	timeoffset=timeoffset, trange=trange, xrange=xrange, yrange=yrange,$
	prange=prange,inputdata=inputdata,outfile=outfile,maxrate=maxrate,$
	noplot=noplot,noshade=noshade,silent=silent,title=title,$
	channel=channel,slit=slit,header=mainheader,_extra=e
;+
; NAME:
;	PLOTRATE
;
; PURPOSE:
;	This procedure plots the count rate from a detector time tag data file,
;	or from an array previously created with READIT. 
;
; CATEGORY:
;	Detector.
;
; CALLING SEQUENCE:
;	PLOTRATE, Infile, Data, Time, Rate
;
; INPUTS:
;	Infile:	Name of a time tag file. If infile is a vector of file
;		names, the data from all of these files will be combined
;		before making the rate plot.
;
; OPTIONAL OUTPUTS:
;	Data:	A structure containing the raw data.
;	Time:	A vector containing the time for each element of Rate
;	Rate:	A vector containing the count rate.
;
; KEYWORDS:
;	INPUTDATA:
;		If this keyword is set equal to a structure containing time
;		tag data (such as that created by MRDFITS), this data is used
;		instead of reading Infile. This can be used to save time
;		by reading the data once, and then plotting multiple times.
;		Note that Infile is still used in the title of the plot.
;	TIMESTEP:
;		Display the rates binned over this time interval in
;		seconds. If not set, TIMESTEP=1.
;	TIMEOFFSET:
;		If included, returns the Modified Julian Day of the exposure
;		start.
;	SILENT:	If included, does not print informational messages from MRDFITS.
;	TRANGE:	An [N,2] element matrix containing the start and ending
;		times of N intervals to include in the array. If N = 1,
;		a simple two element vector, [tstart,tend] may be used.
;	XRANGE:	A [N,2] element vector containing the start and ending x
;		pixel of N pixel ranges to include in the array. If N = 1,
;		a simple to element vector, [xstart,xend] may be used.
;	YRANGE:	A [N,2] element vector containing the start and ending y
;		pixel of N pixel ranges to include in the array. If N = 1,
;		a simple to element vector, [ystart,yend] may be used.
;NOTE that multiple ranges in XRANGE and YRANGE only work if the INPUTDATA 
; keyword is used. This will change if LIST2LIST is modified to allow multiple
; N > 1 for XRANGE and YRANGE.
;	PRANGE:	A two element vector containing the range of pulse height
;		to include in the array.
;	OUTFILE:Set this keyword to a filename to send the output data to a file
;		rather than a plot.
;	MAXRATE:Maximum count rate to display on the plot.
;	NOPLOT:	If set, no plot is made.
;	NOSHADE:If set, and there are times which fall outside the Good Time
;		Intervals, these regions are not shaded.
;	TITLE:	Use this string as the title of the plot rather than the
;		filename.
;	CHANNEL:Channel name, e.g. 'LiF1A'. If this keyword (and SLIT) is
;		present, then the x-y range returned by LOCATESPECTRUM.PRO
;		will be used instead of any XRANGE or YRANGE.
;	SLIT:	'HIRS','MDRS','LWRS', or 'PINH'. Used with the CHANNEL keyword
;		to specify the region on the detector to use. 
;	HEADER:	Returns the header of the FITS file.
; The following are passed directly to SHOWFHEADER2:
;	NOSEGMENT:
;		If set, don't display the segment name.
;	NOROOTNAME:
;		If set, don't display the root name.
;	NOTARGET:
;		If set, don't display the target name.
;	NOID:	If set, don't display the program ID.
;
; SIDE EFFECTS:
;	Creates plots or a file.
;
; EXAMPLE:
;	To plot the count rate of the data in the file 'file.fits',
;	averaged over 4 second intervals, using the two xranges
;	3000 < x < 6400 and 7000 < 15000:
;
;	IDL> plotrate, 'file.fits', data, time, rate, timestep=4, $
;	IDL> xrange = [[3000,7000],[6400,15000]]
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 21 May 1999.
;	14 June 1999 Modified to read multiple files. Can't concatonate
;	 structures, so returned values are wrong in this case.
;	2 July 1999 Added date on plot.
;	14 August 1999 Now extracts MJD of exposure start time, and TIMEOFFSET
;	 keyword.
;	15 August 1999 Added TRANGE, XRANGE, YRANGE, PRANGE keywords, and
;	 replaced MRDFITS with LIST2LIST.
;	17 August 1999 Fixed an error in calculating rates.
;	21 August 1999 Plot now notes if XRANGE, YRANGE, TRANGE, or PRANGE
;	 have been used.
;	22 August 1999 Added INPUTDATA and OUTFILE keywords.
;	18 September 1999 Modified TRANGE keyword to allow multiple ranges.
;	22 September 1999 Fixed a bug introduced on 9/18, which prevented
;	 a single time range from working.
;	22 October 1999 Added MAXRATE keyword. Modified to work if tm
;	 contains only one element.
;	23 October 1999 Added call to SHOWFHEADER.
;	29 October 1999 Moved SHOWFHEADER text to bottom left corner of plot,
;	 and removed it if using existing data.
;	16 February 2000 Modified XRANGE and YRANGE keywords to allow multiple
;	 ranges.
;	3 March 2000 Modified plotting of notes about xrange and yrange to use
;	 !x.crange and !y.crange rather than normalized coordinates.
;	15 May 2000 Added back in code (apparently removed on 8/17/99) to
;	 use noninteger TIMESTEPs.
;	16 May 2000 Added reading of TTPERIOD header keyword to see if TTAG
;	 insertion rate doesn't match the TIMESTEP keyword. Added NOPLOT
;	 option.
;	30 May 2000 Modified to use SHOWFHEADER2 rather than SHOWFHEADER.
;	6 September 2000 Now reads in Good Time Intervals and shades other 
;	 regions. Added NOSHADE keyword. Slightly modified reading of main
;	 FITS header.
;	12 September 2000 Modified shading to fix a bug.
;	31 October 2001 Now sets gtistart and gtistop if INPUTDATA is defined.
;	 Corrected a bug which improperly identified good x ranges, then good y
;	 ranges, rather than combining them, in the case where there are
;	 multiple x and y ranges. Added SILENT keyword. Now prints out name of 
;	 files being processed.
;	18 June 2003 Redid form of output file and added headers to the columns.
;	 Forced output plot ranges to exactly match TRANGE.
;	28 July 2003 Added TITLE keyword.
;	30 July 2003 Added _extra keyword to allow keywords to be passed
;	 to showfheader2
;	19 December 2003 Added CHANNEL and SLIT keywords.
;	21 January 2004 Added HEADER keyword.
;	22 January 2004 Removed unnecessary call to MRDFITS.
;-

;Allowed range of x, y, pulse height:
	XMIN = 0
	XMAX = 16383
	YMIN = 0
	YMAX = 1023
	PMIN = 0
	PMAX = 31

	if keyword_set(timestep) then begin
		timestep = float(timestep)
	endif else begin
		timestep = 1.0
	endelse
; Use x, y, t, ph thresholds:
	lcount = 0		;number of extra labels to display
	label = strarr(5)
	if (not keyword_set(prange)) then begin
		prange = [-1e10,1e10]
	endif else begin
		label[lcount] = $
			'Pulse Height Limited to ['+$
			string(prange[0])+','+string(prange[1])+'] units'
		lcount = lcount + 1
	endelse
	if (not keyword_set(trange)) then begin
		trange = [-1e10,1e10]
	endif else begin
		sizt = n_elements(trange)/2
		trange = reform(trange,sizt,2)
		label[lcount] = 'Time Limited to '
		for i=0,sizt-1 do begin
			label[lcount] = label[lcount] + $
			'['+string(trange[i,0])+','+ $
			string(trange[i,1])+'] '
		endfor
		label[lcount] = label[lcount] + 'seconds'
		lcount = lcount + 1
	endelse
	if keyword_set(channel) and keyword_set(slit) then begin
					;slit specified
		locatespectrum,channel,slit,xledge,xredge,ybot,ytop,/silent
		xrange = [xledge,xredge]
		yrange = [ybot,ytop]
	endif
	if (not keyword_set(xrange)) then begin
		xrange = [-1e10,1e10]
	endif else begin
		sizx = n_elements(xrange)/2
		xrange = reform(xrange,sizx,2)
		label[lcount] = 'X Limited to '
		for i=0,sizx-1 do begin
			label[lcount] = label[lcount] + $
			'['+string(xrange[i,0])+','+ $
			string(xrange[i,1])+'] '
		endfor
		label[lcount] = label[lcount] + 'pixels'
		lcount = lcount + 1
	endelse
	if (not keyword_set(yrange)) then begin
		yrange = [-1e10,1e10]
	endif else begin
		sizy = n_elements(yrange)/2
		yrange = reform(yrange,sizy,2)
		label[lcount] = 'Y Limited to '
		for i=0,sizy-1 do begin
			label[lcount] = label[lcount] + $
			'['+string(yrange[i,0])+','+ $
			string(yrange[i,1])+'] '
		endfor
		label[lcount] = label[lcount] + 'pixels'
		lcount = lcount + 1
	endelse
	xin = (xrange < XMAX) > XMIN
	yin = (yrange < YMAX) > YMIN
	tin = (trange)
	pin = (prange < PMAX) > PMIN 

;Read in the data file(s), unless INDATA exists:
	if keyword_set(inputdata) then begin	;use inputdata
		data = inputdata
		print,'Using existing data!'

		print,'Using time ranges: '
		tgood = [-1]
		for i=0,(n_elements(trange)/2-1) do begin
			print,'t = ',strcompress(trange[i,0],/rem),$
			' to ',strcompress(trange[i,1],/rem)
			tgood = [tgood,where( $
				(data.time ge trange[i,0]) and $
				(data.time le trange[i,1]) $
			)]
		endfor
		tgood = tgood[1:n_elements(tgood)-1]
		data = data(tgood)
		print,n_elements(data),' events remaining'
;Set the GTIs to include all the data:
		gtistart = min(trange)
		gtistop = max(trange)

		print,'Using x and y ranges: '
		xygood = [-1]
		for i=0,(n_elements(xrange)/2-1) do begin
			print,'x = ',strcompress(xrange[i,0],/rem),$
				' to ',strcompress(xrange[i,1],/rem),$
				' and y = ',strcompress(yrange[i,0],/rem),$
				' to ',strcompress(yrange[i,1],/rem)
			xygood = [xygood,where( $
				(data.x ge xrange[i,0]) and $
				(data.x le xrange[i,1]) and $
				(data.y ge yrange[i,0]) and $
				(data.y le yrange[i,1]) $
			)]
		endfor
		xygood = xygood[1:n_elements(xygood)-1]
		data = data(xygood)
		print,n_elements(data),' events remaining'
		datatime = data.time

		if keyword_set(title) then titl = title else titl = infile
		titl = title + ' (' + strcompress(timestep) + ' s avg.)'
		timeoffset = 0
		label[lcount] = 'Using existing data'
		lcount = lcount + 1
	endif else begin		;read data files
		numfiles = n_elements(infile)
		print,'Using data files:'
		if (numfiles gt 1) then begin		;if multiple files
			timeoffset = dblarr(numfiles)
			for i=0,numfiles-1 do begin
				print,infile[i]
				gtidata = mrdfits(infile[i],2,/silent)
								;read GTIs
				datatemp = list2list(infile[i],prange=prange,$
					trange=trange,$
					xrange=xrange,yrange=yrange,$
					fheader=mainheader,silent=silent)
				if (i eq 0) then begin
					datatime = datatemp.time
					datax = datatemp.x
					datay = datatemp.y
					datapha = datatemp.pha
					gtistart = gtidata.start
					gtistop = gtidata.stop
				endif else begin
					datatime = [datatime,datatemp.time]
					datax = [datax,datatemp.x]
					datay = [datay,datatemp.y]
					datapha = [datapha,datatemp.pha]
					gtistart = [gtistart,gtidata.start]
					gtistop = [gtistop,gtidata.stop]
				endelse
				timeoffset[i] = $
					double(fxpar(mainheader,'expstart'))
					;extract start time in MJD
				ttperiod = fxpar(mainheader,'TTPERIOD')
					;time stamp frequency
				if ((ttperiod ne 1) and (timestep ne 1)) $
					then begin
					print,'TTPERIOD = ',ttperiod
					print,'TIMESTEP = ',timestep
					if (ttperiod ne timestep) then begin
						print,'Type .con to continue'
						stop
					endif
				endif
			endfor
			print,'Sorting into time order'
			print,' This may take a long time for large files!'
			q = sort(datatime)		;sort into time order
			datatime = datatime[q]
			datax = datax[q]
			datay = datay[q]
			datapha = datapha[q]
			titl = 'Multiple Files (' + strcompress(timestep) + $
				' s avg.)'
		endif else begin
			print,infile

			data = list2list(infile,prange=prange,$
				trange=trange,xrange=xrange,yrange=yrange,$
				fheader=mainheader,silent=silent)
			gtidata = mrdfits(infile,2,/silent)	;read GTIs
			gtistart = gtidata.start
			gtistop = gtidata.stop
			timeoffset = double(fxpar(mainheader,'expstart'))
					;extract start time in MJD
			ttperiod = fxpar(mainheader,'TTPERIOD')
					;time stamp frequency
			if ((ttperiod ne 1) and (timestep ne 1)) $
				then begin
				print,'TTPERIOD = ',ttperiod
				print,'TIMESTEP = ',timestep
				if (ttperiod ne timestep) then begin
					print,'Type .con to continue'
					stop
				endif
			endif
			datatime = data.time
			if keyword_set(title) then titl = title else $
				titl = infile
			titl = titl + ' (' + strcompress(timestep) + ' s avg.)'
		endelse
       endelse

;New code to find BTIs:
	gtistart = gtistart(uniq(gtistart,sort(gtistart)))
	gtistop = gtistop(uniq(gtistop,sort(gtistop)))
			;remove doubles and sort
	btistart = gtistop
	btistop = fltarr(n_elements(btistart))
	for i=0,n_elements(btistop)-1 do begin
		temp = where(gtistart ge gtistop[i])
		if (temp[0] ne -1) then begin
			btistop[i] = min(gtistart(temp))
		endif else begin
			btistop[i] = btistart[i]
		endelse
	endfor

	ndata = n_elements(datatime)		;number of lines
	timemm = minmax(datatime)
	if (timemm[0] gt trange[0] and trange[0] ne -1.0e10) then $
		timemm[0] = trange[0]
	if (timemm[1] lt trange[1] and trange[1] ne 1.0e10) then $
		timemm[1] = trange[1]
	timerange = timemm[1] - timemm[0]

	imax = ceil(timerange / timestep)

	tstart = timemm[0] + 0.5 * timestep
	tm = round((datatime - tstart) / timestep + 0.5)
	if (n_elements(tm) gt 1) then begin
		ratearray = histogram(tm,min=0)
	endif else begin
		ratearray = fltarr(2)
	endelse

	timearray = findgen(n_elements(ratearray))*timestep + $
		timemm[0] + timestep/2.
	ratearray = ratearray / timestep		;convert to cps

	if (lcount ne 0) then titl = 'Subset of '+titl

	if not keyword_set(maxrate) then begin	;set y max on plot
		maxrate = max(ratearray)
	endif

;Send the results to a plot, a file or neither:
	if not keyword_set(noplot) then begin
	 if not keyword_set(outfile) then begin
		plot,timearray,ratearray,psym=10,xtitle='Time (sec)',$
			ytitle='Count Rate (cps)',title=titl,/xstyle,$
			xrange=timemm,yrange=[0,maxrate]
		for i=0,lcount-1 do begin
			xloc = 0.15*(!x.crange[1]-!x.crange[0]) + !x.crange[0]
			yloc = (0.90-0.035*i)*(!y.crange[1]-!y.crange[0]) + !y.crange[0]
			xyouts,xloc,yloc,label[i]
		endfor

;Shade the Bad Time Intervals:
		if not keyword_set(noshade) then begin
		for i=0,n_elements(btistart)-1 do begin
			xcor = (([btistart(i),btistart(i),btistop(i),btistop(i)])> $
				!x.crange(0))< !x.crange(1)
			ycor = [!y.crange(0),!y.crange(1),!y.crange(1),!y.crange(0)]
			oplot,xcor,ycor,psym=10,linestyle=2
			polyfill,xcor,ycor,/line_fill,orientation=45
		endfor
		endif

		xs = 0.05	;for showfheader
		ys = 0.29	;for showfheader
		csize = 0.75	;for showfheader
		if not keyword_set(inputdata) then $
			showfheader2,xs,ys,mainheader,[1],/nocounts,/nointtime,$
				charsize=csize,_extra=e
		dateit
	 endif else begin	;send data to a file
		if not keyword_set(inputdata) then begin
			segment = fxpar(mainheader,'DETECTOR')
			pid = fxpar(mainheader,'PRGRM_ID')
			tid = fxpar(mainheader,'TARG_ID')
			oid = fxpar(mainheader,'OBS_ID')
			eid = fxpar(mainheader,'EXP_ID')
			mjd = double(fxpar(mainheader,'EXPSTART'))
			day = double(60.*60.*24.)	;seconds in one day
			print,'Saving data in ',outfile
			openw,unit,outfile,/get_lun
			printf,unit,format=$
				'("Exposure: ",a4,1x,a2,1x,a2,1x,a3)',$
				pid,tid,oid,eid
			printf,unit,format='("Segment: ",a2)',segment
			printf,unit,format='("X Range: ",i5,1x,i5)',xin
			printf,unit,format='("Y Range: ",i5,1x,i5)',yin
			printf,unit,format='("PH Range:",i5,1x,i5)',pin
			printf,unit,format='("Time Range: ",f16.3,1x,f16.3)',tin
			printf,unit
			printf,unit,'Elapsed Time      Time (MJD)  Rate (cps)'
			for i=0l,n_elements(timearray)-1l do begin
				printf,format='(2x,f10.3,1x,f15.8,2x,f10.2)',$
					unit,timearray[i],$
					mjd+timearray[i]/day,ratearray[i]
			endfor
			free_lun,unit
		endif
	 endelse
	endif
	print,'---------------'

end


