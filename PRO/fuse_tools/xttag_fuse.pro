;
; Subroutine of FUSE_SCAN for processing time tag data
;
;============================================================ XTTAG_FUSE_EVENT
;
; event driver for xttag_fuse
;
pro xttag_fuse_event,event
common fuse_timetag_data,x,y,t,xbin,ybin,gtime1_orig,gtime2_orig,pha
common xttag_fuse_com,base,file,h,time,xcent,ycent,counts,tcounts,title,mask
	widget_control,event.id,get_uvalue=v
	uvalue = gettok(v,'_')
	utype = v
	widget_control,base.x1,get_v=x1
	widget_control,base.x2,get_v=x2
	widget_control,base.y1,get_v=y1
	widget_control,base.y2,get_v=y2
	if x2 lt x1 then begin
		temp = x1 & x1=x2 & x2 = temp
	end
	if y2 lt y1 then begin
		temp = y1 & y1=y2 & y2 = temp
	end
	widget_control,base.deltat,get_v=delt
	widget_control,base.minc,get_v=minc
	loadct,5,/silent
	case uvalue of
	'EXIT': begin
		time = 0 & xcent = 0 & ycent = 0
		counts = 0 & tcounts = 0 & base = 0
		widget_control,event.top,/destroy
		end
	'MOVIE': begin
		widget_control,base.x1,get_v=x1
		widget_control,base.x2,get_v=x2
		widget_control,base.y1,get_v=y1
		widget_control,base.y2,get_v=y2
		xttag_fuse_movie,[x1,x2],[y1,y2],group=event.top
		return
		end
	'PARAM': widget_control,base.main2,sensitive=0
	'PROCESS': begin
		if delt lt 1 then delt = 1
		if minc lt 1 then minc = 1
		widget_control,base.x1,set_v=x1
		widget_control,base.x2,set_v=x2
		widget_control,base.y1,set_v=y1
		widget_control,base.y2,set_v=y2
		widget_control,base.deltat,set_v=delt
		widget_control,base.minc,set_v=minc
		widget_control,/hourglass
		ttag_centroid,t,x,y,time,xcent,ycent,counts,tcounts,image, $
			interval = float(delt), mincounts = minc, $
			xrange = [x1,x2], yrange = [y1,y2]
		widget_control,base.main2,sensitive=1
		fdecomp,file,disk,dir,name
		title = name + ' ('+ strtrim(x1,2)+':'+strtrim(x2,2)+','+ $
				     strtrim(y1,2)+':'+strtrim(y2,2)+')'
		wset,base.id
		tvscl,frebin(alog10(image>0.1),300,300)
		mask = bytarr(n_elements(time))
		for i=0,n_elements(gtime1_orig)-1 do begin
			good = where((time ge gtime1_orig(i)) and  $
				     (time le gtime2_orig(i)),n)
			if n gt 0 then mask(good) = 1b
		end
		widget_control,/hourglass
		end
	'PLOTX': begin
		widget_control,base.minc,get_v=minc
		good = where(counts gt minc,ngood)
		if ngood lt 1 then begin
			istat = dialog_message('No Valid Centroids Computed', $
					/error,dialog_parent=event.top)
			return
		end
		yrange = [min(xcent(good))-20,max(xcent(good))+20]
		if utype eq 'EDIT' then begin
		     line_edit,time,xcent,mask,title=title+' X Centroid', $
			yrange=yrange,xtitle='time (seconds)',min_val=0.1, $
			/modal,group=event.top
		   end else begin
		     lineplot,time,xcent,title=title+' X Centroid', $
			yrange=yrange,xtitle='time (seconds)',min_val=0.1
		end
		end
	'PLOTY': begin
		widget_control,base.minc,get_v=minc
		good = where(counts gt minc,ngood)
		if ngood lt 1 then begin
			istat = dialog_message('No Valid Centroids Computed', $
					/error,dialog_parent=event.top)
			return
		end
		yrange = [min(ycent(good))-20,max(ycent(good))+20]
		if utype eq 'EDIT' then begin
		      line_edit,time,ycent,mask,title=title+' Y Centroid', $
			  yrange=yrange,xtitle='time (seconds)',min_val=0.1, $
			  /modal,group=event.top
		   end else begin
		      lineplot,time,ycent,title=title+' Y Centroid', $
			  yrange=yrange,xtitle='time (seconds)',min_val=0.1
		end
		end
	'PLOTC': begin
		if utype eq 'EDIT' then begin
			line_edit,time,counts,mask,title=title+' Counts', $
				xtitle='time (seconds)'	,/modal,group=event.top
		    end else begin
			lineplot,time,counts,title=title+' Counts', $
				xtitle='time (seconds)'	
		end		    	
		end
	'PLOTTC': begin
		if utype eq 'EDIT' then begin
			line_edit,time,tcounts,mask,title=title+ $
				' Total Counts',xtitle='time (seconds)', $
				group=event.top,/modal
		    end else begin
			lineplot,time,tcounts,title=title+' Total Counts', $
				xtitle='time (seconds)'	
		end
		end
	'WFITS': begin
		filename = dialog_pickfile(file='xttag.fits',filter='*.fits', $
					/write)
		if filename eq '' then return	;no file selected

		a = {time:time,xcentroid:xcent,ycentroid:ycent,counts:counts, $
			tcounts:tcounts}
		hh = h
		sxaddhist,'TTAG Centroid Processing',hh
		sxaddpar,hh,'xmin',x1,'Centroid Processing Xmin'
		sxaddpar,hh,'xmax',x2,'Centroid Processing Xmax'
		sxaddpar,hh,'ymin',y1,'Centroid Processing Ymin'
		sxaddpar,hh,'ymax',y2,'Centroid Processing Ymax'
		sxdelpar,hh,['naxis','bitpix']
		mwrfits,a,filename,hh,/create,/silent
		end
	'WTEXT': begin
		filename = dialog_pickfile(file='xttag.txt',filter='*.txt', $
					/write)
		if filename eq '' then return	;no file selected
		openw,unit,filename,/get_lun
		printf,unit,'; '+title
		printf,unit,';       Time      Xcentroid    Ycentroid' + $
			'     Counts      TCounts'
		for i=0,n_elements(time)-1 do $
		    printf,unit,time(i),xcent(i),ycent(i),counts(i),tcounts(i)
		free_lun,unit
		end
	'WRITE': xttag_fuse_write,event,x,y,t,time,mask,gtime1_orig, $
					gtime2_orig,pha
	'LOAD': begin
		xttag_fuse_load_edit,event,x,y,t,xbin,ybin,time,mask, $
							gtime1,gtime2,pha
		end
	'PHAFULL': begin
		xttag_fuse_goodtimes,t,mask,time,gt1,gt2,good
		good = where(good,ngood)
		if ngood eq 0 then begin
			result = dialog_message('No good events found', $
					dailog_parent=event.top,/error)
			return
		end
		phahist = histogram(pha(good),min=1,max=31)
		phahist = phahist/total(phahist)
		lineplot,findgen(31)+1,phahist,title='Full Image',xtitle='PHA', $
				ytitle='Fraction of events'
		end
	'PHASUB': begin
		xttag_fuse_goodtimes,t,mask,time,gt1,gt2,good
		widget_control,base.x1,get_v=x1
		widget_control,base.x2,get_v=x2
		widget_control,base.y1,get_v=y1
		widget_control,base.y2,get_v=y2
		good = where(good and (x ge x1) and (x le x2) and $
				(y ge y1) and (y le y2),ngood)
		if ngood eq 0 then begin
			result = dialog_message('No good events found', $
					dialog_parent=event.top,/error)
			return
		end
		phahist = histogram(pha(good),min=1,max=31)
		phahist = phahist/total(phahist)
		title = 'Region' + ' ('+ strtrim(x1,2)+':'+strtrim(x2,2)+','+ $
				     strtrim(y1,2)+':'+strtrim(y2,2)+')'
		lineplot,findgen(31)+1,phahist,title=title,xtitle='PHA', $
				ytitle='Fraction of events'
		end
	'WINDOW':
	endcase
	return
end
;============================================================= TTAG_CENTROID
;
pro ttag_centroid,time,x,y,tout,xcent,ycent,counts,tcounts,image, $
	interval=interval, $
	xrange=xrange,yrange=yrange,tmin=tmin,tmax=tmax,mincounts=mincounts
;+
;				ttag_centroid
;
; Routine to compute centroid vs time in time tagged data
;
; CALLING SEQUENCE:
;	ttag_centroid,time,x,y,tout,xcent,ycent,counts,tcounts,image
;
; INPUTS:
;	time - time vector 
;	x - vector of x positions 
;	y - vector of y positions 
;
; KEYWORD INPUTS:
;	interval - time interval to bin the events(Seconds). (default = 60)
;	xrange - 2 element vector giving [xmin,xmax] of the region
;		to count the events (in HIRES pixels)
;	yrange - 2 element vector giving [ymin,ymax] of the region
;		to count the events (in HIRES pixels)
;	tmin - minimum time to process
;	tmax - maximum time to process
;	mincounts - minimum number of counts for computing centroid.  If
;		fewer than mincounts, x and y are set to zero. (default=20)
;
; OUTPUTS:
;	tout - time vector for the output rate vector
;	xcent - x centroid (in hi-res pixels)
;	ycent - y centroid (in hi-res pixels)
;	counts - total counts in each time bin
;	tcounts - vector of total counts in image
;	image - image of region centroided	
;
; HISTORY:
;	D. Lindler, Aug 1999
;-
;-----------------------------------------------------------------------
;
; calling sequence
;
	if n_params(0) lt 1 then begin
	   print,'ttag_centroid,time,x,y,tout,xcent,ycent,counts,tcounts,image
	   print,'Keyword Inputs: XRANGE, YRANGE, INTERVAL, MINCOUNTS'
	   return
	end
	if n_elements(xrange) eq 0 then xrange = [0,16383]
	if n_elements(yrange) eq 0 then yrange = [0,1023]
	if n_elements(interval) eq 0 then interval = 60.0d0
	if n_elements(tmin) eq 0 then tmin = 0.0
	if n_elements(tmax) eq 0 then tmax = max(time)
	if n_elements(mincounts) eq 0 then mincounts = 20
;
; select proper x and y values by range
;
	keep = where((x ge xrange(0)) and (x le xrange(1)) and $
		     (y ge yrange(0)) and (y le yrange(1)) and $
		     (time ge tmin) and (time le tmax),nkeep)
	if nkeep le 1 then begin
		istat = dialog_message('TTAG_CENTROID: ERROR - no events' + $
			' in specified position/time range',/error)
		retall
	endif
	tt = time(keep) - tmin
	xx = x(keep)
	yy = y(keep)
;
; bin events into bins specified by interval.
;
	nbins = long((tmax-tmin)/interval)>1
	tout = findgen(nbins)*interval + (interval/2.)
	tabinv,tt,tout-interval/2.0,index1
	tabinv,tt,tout+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
	xcent = fltarr(nbins)
	ycent = fltarr(nbins)
	counts = (index2-index1+1)>0
;
; compute centroid for each interval with more than mincounts
;
	for i=0L,nbins-1 do begin
		if counts(i) gt mincounts then begin	
			xcent(i) = total(xx(index1(i):index2(i)))/counts(i)			
			ycent(i) = total(yy(index1(i):index2(i)))/counts(i)			
		end
	end
	tout = tout + tmin
;
; compute counts in the time bins for the whole image
;
	tabinv,time,tout-interval/2.0,index1
	tabinv,time,tout+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
	tcounts = (index2-index1+1)
	image = hist_2d(xx,yy,min1=xrange(0),max1=xrange(1), $
			      min2=yrange(0),max2=yrange(1))
return
end

;====================================================== XTTAG_FUSE_MOVIE_EVENT
;
; Subroutine of fuse_scan to create and display a timetag movie
;
;
; Event driver
;
pro xttag_fuse_movie_event,event
	COMMON XInterAnimate_com, topbase, animatebase, pwin, mpegID
	common fuse_timetag_data,x,y,t
	widget_control,event.top,get_uvalue=base
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'EXIT' then begin
		widget_control,event.top,/destroy
		return
	end
;
; process Full Buttons
;
	if uvalue eq 'FULLX' then begin
		widget_control,base.x1,set_v=0
		widget_control,base.x2,set_v=16383
		widget_control,base.magnify,set_v=1
		widget_control,base.binx,set_v=64
	endif
	
	if uvalue eq 'FULLY' then begin
		widget_control,base.y1,set_v=0
		widget_control,base.y2,set_v=1023
		widget_control,base.magnify,set_v=1
		widget_control,base.biny,set_v=16
	endif
	
	if uvalue eq 'FULLT' then begin
		widget_control,base.t1,set_v=base.tmin
		widget_control,base.t2,set_v=base.tmax
	endif

;
; extract parameters
;
	widget_control,base.x1,get_v=x1
	widget_control,base.x2,get_v=x2
	widget_control,base.y1,get_v=y1
	widget_control,base.y2,get_v=y2
	widget_control,base.t1,get_v=t1
	widget_control,base.t2,get_v=t2
	widget_control,base.binx,get_v=binx
	widget_control,base.biny,get_v=biny
	widget_control,base.magnify,get_v=magnify
	widget_control,base.tint,get_v=tint
	widget_control,base.minval,get_v=minval
	widget_control,base.maxval,get_v=maxval
;
; idiot testing
;
	if x1 lt 0 then begin
		x1 = 0
		widget_control,base.x1,set_v=x1
	end
	if x2 gt 16383 then begin
		x2 = 16383
		widget_control,base.x2,set_v=x2
	end
	if y1 lt 0 then begin
		y1 = 0
		widget_control,base.y1,set_v=y1
	end
	if y2 gt 1023 then begin
		y2 = 1023
		widget_control,base.y2,set_v=y2
	end
	if t1 lt 0 then begin
		t1 = 0
		widget_control,base.t1,set_v=t1
	end
	if t2 gt base.tmax then begin
		t2 = base.tmax
		widget_control,base.t2,set_v=t2
	end
	if magnify lt 1 then begin
		magnify = 1
		widget_control,base.magnify,set_v=magnify
	end
	if tint le 0 then begin
		tint = 1.0
		widget_control,base.tint,set_v=tint
	end
	if binx le 0 then begin
		binx = 1
		widget_control,base.binx,set_v=binx
	end
	if biny le 0 then begin
		biny = 1
		widget_control,base.biny,set_v=biny
	end
;
; determine movie size
;
	nframes = long((t2-t1)/tint)>1
	xfsize = (x2-x1+1)/binx*magnify
	yfsize = (y2-y1+1)/biny*magnify
	mvsize = float(nframes)*xfsize*yfsize/1000000.0
	widget_control,base.mvsize,set_v=string(mvsize,'(F8.1)')
	widget_control,base.fsize,set_v = 'Frame Size:     '+ $
		strtrim(xfsize,2)+ ' x '+ strtrim(yfsize,2)
	widget_control,base.nframes,set_v = 'Number of Frames:    '+ $
		strtrim(nframes,2)
;
; More idiot testing
;
	if uvalue ne 'CREATE' then return
	if n_elements(topbase) gt 0 then xinteranimate,/close
	if mvsize gt 128 then begin
		istat = dialog_message('Movie size is too big',/error, $
				dialog_parent=event.top)
		return
	end
	if (xfsize gt 1024) then begin
		istat = dialog_message('Movie X frame size is too big', $
			dialog_parent = event.top,/error)
		return
	endif
	if (yfsize gt 900) then begin
		istat = dialog_message('Movie Y frame size is too big', $
			dialog_parent = event.top,/error)
		return
	endif
	if xfsize lt 1 then begin
		istat = dialog_message('Movie X frame size is too small', $
			dialog_parent = event.top,/error)
	end
	if yfsize lt 1 then begin
		istat = dialog_message('Movie Y frame size is too small', $
			dialog_parent = event.top,/error)
	end
;
; set up animation widget
;
	xinteranimate,group=event.top,set=[xfsize,yfsize,nframes], $
		/showload,/track
	xttag_make_movie,t,x,y,range = [x1,x2,y1,y2], tmin=t1,tmax=t2, $
		interval=tint,xbinsize=binx,ybinsize=biny,magnify=magnify, $
		minval=minval,maxval=maxval
	xinteranimate,group=event.top
		
	return
	end
;========================================================== XTTAG_MAKE_MOVIE
pro xttag_make_movie,time,x,y,range=range,tmin=tmin,tmax=tmax, $
	interval=interval,xbinsize=xbinsize,ybinsize=ybinsize, $
	magnify=magnify,minval=minval,maxval=maxval
;
; Routine to create a FUSE timetag movie
;
; CALLING SEQUENCE:
;	ttag_movie,time,x,y
;
; INPUTS:
;	time - vector of time values 
;	x - vector of x positions 
;	y - vector of y positions 
;
; OPTIONAL KEYWORD INPUTS:
;
;	range = range of pixel positions in unbinnned pixel
;		locations to use: [xmin,xmax,ymin,ymax]
;		(default is the entire image)
;	tmin = minimum time to consider (default = min(time))
;	tmax = maximum time to consider (default = max(time))
;	interval = time interval between movie frames - default is
;		(tmax - tmin)/20
;	xbinsize = binning factor in x (default = 1)
;	ybinsize = binning factor in y (default = 1)
;	magnify = magnfication factor (default = 1)
;	minval = minimum display value for contrast control
;	maxval = maximum display value
;
;
; set defaults
;
	if n_elements(range) eq 0 then range = [0,16383,0,1023]
	if n_elements(tmin) eq 0 then tmin = min(time)
	if n_elements(tmax) eq 0 then tmax = max(time)
	if n_elements(interval) eq 0 then interval = (tmax-tmin)/40
	if n_elements(xbinsize) eq 0 then xbinsize = 2
	if n_elements(ybinsize) eq 0 then ybinsize = 2
	if n_elements(magnify) eq 0 then magnify = 1
	if n_elements(minval) eq 0 then minval = 0
	if n_elements(maxval) eq 0 then maxval = !d.n_colors-1
;
; determine size of output data cube
;
	xmin = range(0)
	xmax = range(1)
	ymin = range(2)
	ymax = range(3)
	ns = long((xmax-xmin+1)/xbinsize)
	nl = long((ymax-ymin+1)/ybinsize)
	nt = long((tmax-tmin)/interval)
	n= ns*nl*nt				;number of elements in cube
;
; adjust maximums to account for partial bins
;
	ymax = ymin + nl*ybinsize - 1
	xmax = xmin + ns*xbinsize - 1
	timemax = tmin + interval*nt
;
; create vector of frame times
;
	framet = findgen(nt)*interval + (tmin + interval/2.0)
;
; find first and last index for each time bin
;
;
	tabinv,time,framet-interval/2.0,index1
	tabinv,time,framet+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
;
; loop on frames
;
	for i = 0,nt-1 do begin
	    i1 = index1(i)
	    i2 = index2(i)
	    if i2 ge i1 then begin
	    	xx = x(i1:i2)
		yy = y(i1:i2)
		good = where((xx ge xmin) and (xx le xmax) and $
			     (yy ge ymin) and (yy le ymax),ngood)
	    end else ngood = 0
	    if ngood gt 0 then begin
	    	xx = xx(good)-xmin
		yy = yy(good)-ymin
		image = hist_2d(xx/xbinsize,yy/ybinsize,min1=0,max1=ns-1, $
			min2=0,max2=nl-1)
		image = bytscl(image,top=!d.n_colors-1,min=minval,max=maxval)
	    end else image = bytarr(ns,nl)
	    if magnify gt 1 then $
	    		image = rebin(image,ns*magnify,nl*magnify,/sample)
	    xinteranimate,frame=i,image=image
	 end
	return
end


;=========================================================== XTTAG_FUSE_MOVIE
;
; Main routine
;
pro xttag_fuse_movie,xrange,yrange,group=group
	common fuse_timetag_data,x,y,t
;
; create widget layout
;

	main = widget_base(/col,group=group,title='FUSE MOVIE CREATOR')

	exit = widget_button(main,uvalue='EXIT',value='EXIT')
;
; region selection
;
	main1 = widget_base(main,/col,/frame,group=group)
	label = widget_label(main1,value='RANGE SELECTION')
	b = widget_base(main1,/row)
	x1 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(0), $
			/integer,/return_events,title='X Range: ')
	x2 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(1), $
			/integer,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLX',value='Full')
	b = widget_base(main1,/row)
	y1 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(0), $
			/integer,/return_events,title='Y Range: ')
	y2 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(1), $
			/integer,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLY',value='Full')
	
	label = widget_label(main1,value='Time Period to display in seconds')
	label = widget_label(main1,value='from start of observation')
	tmin = min(t,max=tmax)
	tmin = long(tmin)
	tmax = long(tmax+1)
	b = widget_base(main1,/row)
	t1 = cw_field(b,uvalue='PARAM',xsize=8,value=tmin, $
			/long,/return_events,title='T Range: ')
	t2 = cw_field(b,uvalue='PARAM',xsize=8,value=tmax, $
			/long,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLT',value='Full')
;
; binning
;
	main2 = widget_base(main,/col,/frame)
	label = widget_label(main2,value='BINNING SELECTION')
	b = widget_base(main2,/row)
	binx = cw_field(b,uvalue='PARAM',xsize=8,value=1, $
			/integer,/return_events,title='Bin X: ')
	biny = cw_field(b,uvalue='PARAM',xsize=8,value=1, $
			/integer,/return_events,title='Bin Y: ')
	magnify = cw_field(main2,uvalue='PARAM',xsize=4,title='Zoom Factor:', $
			/integer,/return_events,value=1)
	b = widget_base(main2,/row)
	v = float(round((tmax-tmin)/100>1))
	tint = cw_field(b,uvalue='PARAM',xsize=8,value=v,/float, $
			/return_events,title='Time Interval: ')
	label = widget_label(b,value=' Seconds')
;
; Estimated Movie Size
;
	main3 = widget_base(main,/col,/frame)
	label = widget_label(main3,value='The estimated movie size in '+ $
						'Megabytes will be:')
	mb = (long(xrange(1)-xrange(0)+1) * $
	      long(yrange(1)-yrange(0)+1) * 100.0)/1000000.
	mvsize = widget_label(main3,/frame,value=string(mb,'(F20.1)'))
	label = widget_label(main3,value='The value must be less than 128.')
	label = widget_label(main3,value='To decrease: increase bin sizes or')
	label = widget_label(main3,value='or decrease range sizes')
	fsize = widget_label(main3,value='Frame Size:              '+ $
		strtrim(xrange(1)-xrange(0)+1,2) + ' x ' + $
		strtrim(yrange(1)-yrange(0)+1,2))
	nframes = widget_label(main3,value='Number of Frames:         100') 
;
; Movie display range
;
;
	main4 = widget_base(main,/col,/frame)
	label = widget_label(main4,value='MOVIE CONTRAST')
	b = widget_base(main4,/row)
	minval = cw_field(b,uvalue='PARAM',xsize=5,value=0,/integer, $
			/return_events,title='Min counts: ')
	maxval = cw_field(b,uvalue='PARAM',xsize=5,value=10,/integer, $
			/return_events,title='Max counts: ')
	create = widget_button(main,uvalue='CREATE',value='LIGHTS,   ' + $
			'CAMERA,   ACTION')
;
; start widget
;
	widget_control,main,/realize
	base = {main:main, x1:x1, x2:x2, y1:y1, y2:y2, t1:t1, t2:t2,  $
		binx:binx, biny:biny, magnify:magnify, tint:tint,  $
		mvsize:mvsize, fsize:fsize, nframes:nframes, minval:minval, $
		maxval:maxval, tmin:tmin, tmax:tmax}
	widget_control,main,set_uvalue=base	
	xmanager,'xttag_fuse_movie',main,/no_block
	return
	end

;========================================================XTTAG_FUSE_LOAD_EDIT
;
; Routine to load time edited image into FUSE_SCAN
;
pro xttag_fuse_load_edit,event,x,y,t,xbin,ybin,time,mask,gtime1,gtime2,pha

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
;
; create new gtime1 and gtime2 array from time,mask
;
	xttag_fuse_goodtimes,t,mask,time,gtime1,gtime2,good
;
; only keep events between user specified PHA min/max
;
	xttag_fuse_pha_range,pha_min,pha_max,group=event.top
	good = where(good and (pha ge pha_min) and (pha le pha_max),ngood)
	if ngood eq 0 then begin
		result = dialog_message('No good events found', $
				dailog_parent=event.top,/error)
		return
	end
;
; create new image
;
	nsout = 16384/xbin
	nlout = 1024/ybin
	data = hist_2d(x(good)/xbin,y(good)/ybin,min1=0,min2=0, $
				max1 = nsout-1, max2 = nlout-1)
	fuse_scan_display,data
return
end	
;============================================================ XTTAG_FUSE_WRITE
;
; Routine to write editted data
;
pro xttag_fuse_write,event,x,y,t,time,mask,gtime1_orig,gtime2_orig,pha

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
;
; get output filename
;
	fdecomp,filename,disk,dir,name,ext
	file = dialog_pickfile(file='edit_'+name+'.'+ext,filter='*.fit', $
				group=event.top,/write)
	if file eq '' then return	;no file selected
;
; create new gtime1 and gtime2 array from time,mask
;
	xttag_fuse_goodtimes,t,mask,time,gt1,gt2,good
;
; merge original file gtimes and new mask gtimes
;
	xttag_fuse_gtmerge,gt1,gt2,gtime1_orig,gtime2_orig,gtime1,gtime2
;
; only keep events between user specified PHA min/max
;
	xttag_fuse_pha_range,pha_min,pha_max,group=event.top
	good = where(good and (pha ge pha_min) and (pha le pha_max),ngood)

	if ngood eq 0 then begin
		result = dialog_message('No good events found', $
				dailog_parent=event.top,/error)
		return
	end
;
; create new file
;
	if ngood gt 0 then begin
		h = header
		sxaddpar,h,'exptime',total(gtime2-gtime1)
		sxaddpar,h,'nevents',ngood
		sxaddpar,h,'nbadfs',n_elements(t)-ngood, $
				'# of events deleted by FUSE_SCAN',/pdu
		data = bytarr(9,ngood)
		if is_ieee_big() then begin
			data(0:3,*) = byte(t(good),0,4,ngood)
			data(4:5,*) = byte(x(good),0,2,ngood)
			data(6:7,*) = byte(y(good),0,2,ngood)
			data(8,*) = byte(pha(good),0,1,ngood)
		    end else begin
		    	temp = t(good)
			host_to_ieee,temp
			data(0:3,*) = byte(temp,0,4,ngood)
			temp = x(good)
			host_to_ieee,temp
			data(4:5,*) = byte(temp,0,2,ngood)
			temp = y(good)
			host_to_ieee,temp
			data(6:7,*) = byte(temp,0,2,ngood)
			data(8,*) = byte(pha(good),0,1,ngood)			
		end
		sxaddpar,h,'naxis2',ngood
		sxdelpar,h,['xbin','ybin']
		expotime = total(gtime2-gtime1)
		sxaddpar,h,'xs-onti',expotime
		sxaddpar,h,'xs-livti',expotime
		fs_fits_write,file,data,h,extname='EVENTS'
		a = mrdfits(filename,2,h,/silent)
		a = replicate({start:0.0d0,stop:0.0d0},n_elements(gtime1))
		a.start = gtime1
		a.stop = gtime2
		mwrfits,a,file,h,/silent,/no_comment
	end
return
end	
;========================================================= XTTAG_GOODMASK
;
function xttag_goodmask,t,gtime1,gtime2
;
; find data within good times
;
	tabinv,t,gtime1,index1
	tabinv,t,gtime2,index2
	index1 = long(index1+0.999)<(n_elements(t)-1)
	index2 = long(index2)<(n_elements(t)-1)
	good = bytarr(n_elements(t))
	for i=0,n_elements(index1)-1 do begin
		if (index1(i) ge 0) and (index2(i) ge index1(i)) then $
			good(index1(i):index2(i))=1b
	end
	return,good
end	
;======================================================== XTTAG_FUSE_GOODTIMES
;
; routine to find good time intervals begin/end
;
pro xttag_fuse_goodtimes,t,mask,time,gtime1,gtime2,good
;
; Inputs: t - vector of time tags for all data
;	  mask - mask of time intervals that are good
;	  time - mid-points of the time intervals
; Outputs:
;	gtime1 - vector of starting times for good intervals
;	gtime2 - vector of ending times for good intervals
;	good - mask for t (1=good timetag data point)
;
	n = n_elements(mask)
	gtime1 = -1.0
	gtime2 = -1.0
	i1 = 0L
	ipos = 1
	while ipos lt n do begin
		if mask(ipos) ne mask(i1) then begin
		    i2 = ipos-1
		    if mask(i1) eq 1 then begin
			if i1 eq 0 then time1 = 0.0 $
				   else time1 = (time(i1-1)+time(i1))/2.0
			time2 = (time(i2) + time(i2+1))/2.0
		        gtime1 = [gtime1,time1]
		        gtime2 = [gtime2,time2]
		    end
		    i1 = i2+1
		end
		ipos = ipos + 1
	end    
	if mask(i1) eq 1 then begin
		if i1 eq 0 then time1 = 0.0 $
			   else time1 = (time(i1-1)+time(i1))/2.0
		time2 = t(n_elements(t)-1)
		gtime1 = [gtime1,time1]
		gtime2 = [gtime2,time2]
	end
	if n_elements(gtime1) gt 1 then begin
		gtime1 = gtime1(1:*)
		gtime2 = gtime2(1:*)
	end
;
; find data within good times
;
	good = xttag_goodmask(t,gtime1,gtime2)
return
end
;====================================================== xttag_fuse_gtmerge
; Merge two lists of goodtimes
;
pro xttag_fuse_gtmerge,a1,a2,b1,b2,out1,out2
;
; out1 and out2 will have good times that are good times in both
; a1 to a2 and b1 to b2
;
	na = n_elements(a1)
	nb = n_elements(b1)
	out1 = dblarr(na+nb+1)
	out2 = dblarr(na+nb+1)

	ia = 0
	ib = 0
	iout = 0
start:
	if a2(ia) lt b1(ib) then begin
		ia = ia+1
		goto,next
	end
	
	if b2(ib) lt a1(ia) then begin
		ib = ib+1
		goto,next
	end

	if a1(ia) gt b1(ib) then out1(iout) = a1(ia) $
			    else out1(iout) = b1(ib) 
		
	if a2(ia) lt b2(ib) then begin
		out2(iout) = a2(ia)
		ia = ia + 1
	    end else begin
		out2(iout) = b2(ib)
		ib = ib + 1
	end
	if out2(iout) ne out1(iout) then iout = iout + 1
next:
	if (ia lt na) and (ib lt nb) then goto,start

	if iout gt 0 then begin
		out1 = out1(0:iout-1)
		out2 = out2(0:iout-1)
	   end else begin
	   	out1 = 0.0d0
		out2 = 0.0d0
	end
return
end


;========================================================= XTTAG_FUSE_PHA_RANGE
;
; Widget to get range for selected time tag data
;
;
; event driver
;
pro xttag_fuse_pha_range_event,event

	widget_control,event.top,get_uvalue=a
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'DONE' then begin
		widget_control,(*a).minbase,get_value=n
		(*a).min = n
		widget_control,(*a).maxbase,get_value=n
		(*a).max = n
		widget_control,event.top,/destroy
	end
	return
end
;
; xbin/ybin widget
;
pro xttag_fuse_pha_range,pha_min,pha_max,group=group

	base = widget_base(/col,group=group,/modal)
	lab = widget_label(base,value='Select Range of PHA values to keep')
	button = widget_button(base,value='Done',uvalue='DONE')
	minbase = cw_field(base,uvalue='Min PHA',xsize=10,value=1,/integer, $
			/return_events,title='Min PHA: ')
	maxbase = cw_field(base,uvalue='Max PHA',xsize=10,value=31,/integer, $
			/return_events,title='Max PHA: ')
		
	
	a = ptr_new({minbase:minbase,maxbase:maxbase,min:1,max:31})
	
	widget_control,base,set_uvalue=a
	widget_control,base,/realize
	xmanager,'xttag_fuse_pha_range',base
	pha_min = (*a).min
	pha_max = (*a).max
	ptr_free,a
	return
end



;=================================================================  XTTAG_FUSE
pro xttag_fuse,filename,xrange,yrange,header,group=group
;
common fuse_timetag_data,x,y,t,xbin,ybin,gtime1_orig,gtime2_orig
common xttag_fuse_com,base,file,h,time,xcent,ycent,counts,tcounts,title,mask
;
; initialize common
;
	if xregistered('xttag_fuse') then widget_control,base.main,/destroy
	file = filename
	if n_elements(t) le 1 then begin
		istat = dialog_message('Time-tag events no retained during'+ $
				' read',dialog_parent=group,/error)
		return
	end
	time = [t(0),t(n_elements(t)-1)]
	mask = [1,1]
;
; create header for output table
;
	a = mrdfits(file,0,h,/silent)
;
; create widget layout
;
	main = widget_base(/col,group=group,title=file)
	menu = widget_base(main,/row)
	exit = widget_button(menu,uvalue='EXIT',value='EXIT')
	movie = widget_button(menu,uvalue='MOVIE',value='Movie')
	button = widget_button(menu,uvalue='WRITE',value='Write Edited Data')
	main1 = widget_base(main,/col,/frame)
	b = widget_base(main1,/row)
	x1 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(0), $
			/integer,/return_events,title='X Range: ')
	x2 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(1), $
			/integer,/return_events,title=' ')
	b = widget_base(main1,/row)
	y1 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(0), $
			/integer,/return_events,title='Y Range: ')
	y2 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(1), $
			/integer,/return_events,title=' ')
	deltat = cw_field(main1,uvalue='PARAM',xsize=6,value=120,/integer, $
			/return_events,title='Time Intervals (Seconds):')
	minc = cw_field(main1,uvalue='PARAM',xsize=6,value=20,/integer, $
			/return_events,title='Min counts needed to centroid:') 
	process = widget_button(main,uvalue='PROCESS',value='PROCESS')
	main2 = widget_base(main,/col,frame=1)
	split = widget_base(main2,/row)
	col1 = widget_base(split,/col)
	plotx = widget_button(col1,value='  PLOT X Centroids  ',uvalue='PLOTX')
	ploty = widget_button(col1,value='  PLOT Y Centroids  ',uvalue='PLOTY')
	plotc = widget_button(col1,value='  PLOT Counts    ',uvalue='PLOTC')
	plottc = widget_button(col1,value='      PLOT Total Counts      ', $
				uvalue='PLOTTC')
	col2 = widget_base(split,/col)
	button = widget_button(col2,value='Edit',uvalue='PLOTX_EDIT')
	button = widget_button(col2,value='Edit',uvalue='PLOTY_EDIT')
	button = widget_button(col2,value='Edit',uvalue='PLOTC_EDIT')
	button = widget_button(col2,value='Edit',uvalue='PLOTTC_EDIT')
	b = widget_base(main2,/row)
	wfits = widget_button(b,value='Write Fits Table',uvalue='WFITS')
	wtext = widget_button(b,value='Write Text Table',uvalue='WTEXT')
	button = widget_button(main2,value='Load Edited Image', $
		uvalue='LOAD_EDIT')
	b = widget_base(main,/row)
	label = widget_label(b,value='PHA: ')
	button = widget_button(b,value='Full Image',uvalue='PHAFULL')
	button = widget_button(b,value='Sub Image',uvalue='PHASUB')
	window = widget_draw(main,xsize=300,ysize=300,uvalue='WINDOW',/motion)
;
; start widget
;	
	widget_control,main,/realize
	widget_control,main2,sensitive=0
	widget_control,window,get_v = id
	base = {main:main,exit:exit,main1:main1,x1:x1,x2:x2,y1:y1,y2:y2, $
		deltat:deltat,minc:minc,main2:main2,id:id}
	xmanager,'xttag_fuse',main,/no_block
	return
	end
