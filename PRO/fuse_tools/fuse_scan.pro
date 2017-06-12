;+
;				FUSE_SCAN
;
; Routine to interactively examine raw FUSE data files
;
; CALLING SQUENCE:
;	fuse_scan
;		or
;	fuse_scan,data,header
;
; OPTIONAL INPUTS:
;	data - data array
;	header - FITS header
;
; INTERACTIVE INPUT:
;   Image Windows:
;	lower - display of whole image rebinned to 1024 x 128 size
;	upper left - scrollable display of whole image without any
;		rebinning.
;	upper right - zoomed image display
;
;	To select region viewed in upper right image use the scroll bars
;	to the botton and right of the image or place cursor in bottom image
;	and push any mouse button.
;
;	To select region for the zoomed image. Place cursor in the upper
;	left window and push any mouse button.
;
;   MENU BUTTONS:
; 	FILE/READ - to specify and read a FUSE fits file
; 	FILE/PS output - to write postscript file of screen contents
; 	FILE/EXIT - to exit the program
;
; 	COLORS - to change color table
; 	CONTRAST - to change intensity scaling function (linear, log, square 
;		root, or histogram equalization)
;	IMAGE/SET_REGION - use to manually set regions displayed in the
;		big window or the zoom window
;	IMAGE/HISTOGRAM - to plot histogram of data values
;	IMAGE/Display Header - to display the image's header
; 	STATS - to generate statistics of selected region. To generate
;		statistics for a box or draw region, follow directions
;		given above upper left window.
; 	PLOT - to plot rows, columns, row sums, column sums, or cross 
;		sectional plots. After selecting type of plot, follow the
;		directions given above the upper left window.  If a second
;		plot is made without closing the first plot window, it will
;		be overplotted on the existing plot.
;	OVERLAY/Wavelength scale - to display approximate wavelength scale
;		overlay
; 	OVERLAY/Clear Overlay- to clear overlay lines written on the image
;	Time-Tag - time tag processing, plots/movies/good time editing
;	Zoom Image/GAUSSFIT - to fit gaussians in both directions to zoomed 
;		region
; 	Zoom Image/SURFACE - to generate surface plot of zoomed region
;	Zooom Image/CONTOUR - to generate a contour plot of the zoomed region
;	ZOOM Factor- to change the zoom factor of the zoom window
;
;    TEXT Boxes
;	Freeze Min/Max - to Freeze the values of the Min and Max used for
;		image scaling.
;	Min - image minimum - used to set background level for display and
;		for computing statistics of selected regions
;	Max - image maximum - used to set maximum for image scaling
;	Reset Min/Max - sets Min and Max to the original image Min/Max
;	X - displays x position of cursor in image pixel coordinates
;	y - displays y position of cursor in image pixel coordinates
;	Val - displays data value for the pixel at the specified x/y
;
; HISTORY:
;	version 1 D. Lindler, Sept, 1999
;	Oct. 2000: modified to update goodtimes in output time-tag file,
;		added wavelength scale overlay,  Modified menu bar
;		button positions.
;	Dec 2001, added extention number to filename when READ/EXTENSION is
;		is used.
;-
; =========================================================== FUSE_SCAN_EVENT
;
; fuse_scan Event Handler 
;
@xreadfuse
@xttag_fuse
pro fuse_scan_event,event

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
common fuse_timetag_data,xttag,yttag,timetag,xbin,ybin,gtime1,gtime2,pha

	widget_control,event.id,get_uvalue=uvalue
	tvlct,rsave,gsave,bsave
	case uvalue of
	'MAIN': 
	'FILE': begin
		case event.value of
;
; Read  Raw Files
;
		1: begin
		   widget_control,/hourglass
		   if n_elements(filename) ne 0 then fdecomp,filename,disk,dir
		   xreadfuse,flist,header,orig,status,group=event.top,dir=dir
		   if status eq '' then begin
		   	filename = flist(0)
			fuse_scan_display,orig
			widget_control,event.top,tlb_set_title=filename
		   end
		   widget_control,/hourglass
		   end
;
; Read Image Extension
;
		2: begin
		   widget_control,/hourglass
		   if n_elements(filename) ne 0 then fdecomp,filename,disk,dir
		   fuse_scan_exten,header,orig,status,filename, $
		   	group=event.top,dir=dir
		   if status eq '' then begin
		        s = size(orig) & ns = s(1) & nl = s(2)
			if (ns gt 8192) and  $
			   (!version.os_family eq 'Windows') then begin
			   	orig = rebin(orig,ns/2,nl)
				istat = dialog_message('Histogram Data ' + $
					'binned by 2 in X'+ $
					' (Windows IDL limitation)',/info)
				sxaddpar,header,'XBIN',2
			end

			fuse_scan_display,orig
			widget_control,event.top,tlb_set_title=filename
		   end
		   widget_control,/hourglass
		   end
		   
;
; Postscript output
;
		4: fuse_scan_ps
		5: fuse_scan_ps,/reversed
		6: fuse_scan_ps,/color
;
; Exit
;
	        7: begin
		   orig = 0
		   little_image = 0
		   zoom = 0
		   xttag = 0
		   yttag = 0
		   timetag = 0
		   widget_control,event.top,/destroy
		   set_viewport
		   loadct,0
		   return
		   end
		endcase
	       end
	'COLORS': begin
		xloadct,/modal,group=event.top
		tvlct,rsave,gsave,bsave,/get
		end

	'CONTRAST': begin
		scale_type = strmid(event.value,9,strlen(event.value)-9)
		fuse_scan_display
		end
	'IMAGE': begin
		case event.value of
		'IMAGE.Set Region':fuse_scan_set_region,group=event.top
		'IMAGE.Histogram' : fuse_scan_histogram,event.top
		'IMAGE.Display Header': xdisplayfile, "", group=event.top, $
					font='6X13', $
					title='FITS Header', text=header
		endcase
		end
	'MIN_FIELD': fuse_scan_display

	'MAX_FIELD': fuse_scan_display
	
	'FREEZE': begin
		widget_control,freeze,get_value=v
		if v eq 'Freeze Min/Max' then newv = 'UnFreeze Min/Max' $
					 else newv = 'Freeze Min/Max'
		widget_control,freeze,set_value=newv
		end
	'OVERLAY': begin
		case event.value of
		'Overlay.Clear Overlay': begin
			overlay_on = 0
			fuse_scan_display
			end
		'Overlay.Wavelength Scale': begin
			overlay_on = 1
			fuse_scan_overlay,header,xbin,ybin,big_id,little_id
			end
		endcase
		end

	'RESET': begin
		widget_control,min_field,set_value=float(omin)
		widget_control,max_field,set_value=float(omax)
		fuse_scan_display
		end		

	'ZOOM': begin
		x = xoffzoom + 128/zoom_factor
		y = yoffzoom + 128/zoom_factor
		zoom_factor = fix(strmid(event.value,12,2))		
		xoffzoom = (x - 128/zoom_factor)>0
		yoffzoom = (y - 128/zoom_factor)>0
		fuse_scan_zoom,orig,zoom_factor,xoffzoom,yoffzoom,zoom
		fuse_scan_scale,zoom_id,zoom
		end

	'BIG_WINDOW': begin
		if event.type eq 3 then begin
			fuse_scan_position
			return
		end
		x = event.x
		y = event.y
		fuse_scan_xyval,orig,x,y,1,1,0,0
		if (state(0) eq 1) or (state(0) eq 2) or (state(0) eq 9) then  $
				fuse_scan_defroi,event,big_id
		if (state(0) ge 3) and (state(0) le 8) then $
				fuse_scan_lineplot,event,'BIG'
		if (event.press gt 1) or ((state(0) eq 0) and $
		   (event.press eq 1)) then begin
			 xoffzoom = round(x - 128/zoom_factor)>0
			 yoffzoom = round(y - 128/zoom_factor)>0
			 fuse_scan_zoom,orig,zoom_factor,xoffzoom,yoffzoom,zoom
			 fuse_scan_scale,zoom_id,zoom
		end
		end

	'LITTLE_WINDOW': begin
		x = event.x
		y = event.y
		factorx = 1024./ns
		factory = 128.0/nl
		fuse_scan_xyval,orig,x,y,factorx,factory,0,0
		if (state(0) eq 1) or (state(0) eq 2) or (state(0) eq 9) then  $
				fuse_scan_defroi,event,little_id
		if (state(0) ge 3) and (state(0) le 8) then $
				fuse_scan_lineplot,event,'LITTLE'
		if (state(0) eq 0) then begin		
			if event.press ge 1 then little_down=1
			if event.release ge 1 then little_down=0
			if little_down then begin
				factorx = ns/1024.0
				factory = nl/128.
				x1 = ((x*factorx)-350)>0
				y1 = ((y*factory)-256)>0
				widget_control,big_window,set_draw_view=[x1,y1]
				fuse_scan_position
			end
		end
		end

	'ZOOM_WINDOW': begin
		x = event.x
		y = event.y
		fuse_scan_xyval,orig,x,y,zoom_factor,zoom_factor,xoffzoom, $
								yoffzoom
		if (state(0) eq 0) and (event.press ne 0) then begin
			widget_control,x_field,get_value=xx
			widget_control,y_field,get_value=yy
			fuse_scan_xyttag,xx,yy,/to_binned
			xoffzoom = round(xx - 128/zoom_factor)>0
			yoffzoom = round(yy - 128/zoom_factor)>0
			fuse_scan_zoom,orig,zoom_factor,xoffzoom,yoffzoom,zoom
			fuse_scan_scale,zoom_id,zoom
		end
		if (state(0) eq 1) or (state(0) eq 2) or (state(0) eq 9) then  $
				fuse_scan_defroi,event,zoom_id
		if (state(0) ge 3) and (state(0) le 8) then $
				fuse_scan_lineplot,event,'ZOOM'
		end

	'X_FIELD': begin
		widget_control,x_field,get_value=x
		widget_control,y_field,get_value=y
		fuse_scan_xyval,orig,x,y,1,1,0,0
		end

	'Y_FIELD': begin
		widget_control,x_field,get_value=x
		widget_control,y_field,get_value=y
		fuse_scan_xyval,orig,x,y,1,1,0,0
		end


	'ZOOM IMAGE': Begin
		case event.value of

		'Zoom Image.Surface Plot': begin
		if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   (xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	 	   yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
		   data = orig(xoffzoom:xend,yoffzoom:yend)
		   x = findgen(xend-xoffzoom+1)+xoffzoom
		   y = findgen(yend-yoffzoom+1)+yoffzoom
		   widget_control,min_field,get_value=imin
		   widget_control,max_field,get_value=imax
		   case scale_type of 
			'Linear': data = data>imin<imax
			'Log': 	begin
				tmin=imax/1e4
				data = alog10((data-imin)>tmin<(imax-tmin))
				end
			'Sqrt': data = sqrt(data>0>imin<imax)
			'Hist. Eq.': data = hist_equal(data)
		   endcase
		   xsurface,data
		end
		end

		'Zoom Image.Contour Plot': begin
		if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   (xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	 	   yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
		   data = orig(xoffzoom:xend,yoffzoom:yend)
		   x = findgen(xend-xoffzoom+1)+xoffzoom
		   y = findgen(yend-yoffzoom+1)+yoffzoom
		   fuse_scan_xyttag,x,y,/frac
		   live_contour,data,xindep=x,yindep=y
		end
		end

		'Zoom Image.GAUSSFIT.Emission': begin
		if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   (xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	 	   yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
		   data = orig(xoffzoom:xend,yoffzoom:yend)
		   x = findgen(xend-xoffzoom+1)+xoffzoom
		   y = findgen(yend-yoffzoom+1)+yoffzoom
		   fuse_scan_gfit,data,x,y,1
		end
		end
		'Zoom Image.GAUSSFIT.Absorption': begin
		if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   (xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	 	   yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
		   data = orig(xoffzoom:xend,yoffzoom:yend)
		   x = findgen(xend-xoffzoom+1)+xoffzoom
		   y = findgen(yend-yoffzoom+1)+yoffzoom
		   fuse_scan_gfit,data,x,y,2
		end
		end
		endcase
	    end				
	'TTAG': begin
		if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   (xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	 	   yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
		   data = orig(xoffzoom:xend,yoffzoom:yend)
		   xr = [xoffzoom,xend]
		   yr = [yoffzoom,yend]
		   fuse_scan_xyttag,xr,yr
		   xttag_fuse,filename,xr,yr,group=event.top   
		 end
		 end
	'STATS': begin
		 val = event.value
		 case val of
;
;	Whole image
;
		 3:  begin
		     widget_control,/hourglass
		     widget_control,min_field,get_value=minv
		     fuse_scan_stats,orig,minv,title='Image Statistics', $
		     		group=event.top
		     widget_control,/hourglass
		     end
;
;	Zoom Image
;
		4:  begin
		   if (xoffzoom ge 0) and (yoffzoom ge 0) and $
		   	(xoffzoom le ns-1) and (yoffzoom le nl-1) then begin
		   	xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1 ) $
									<(ns-1)
	 	   	yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1 ) $
									<(nl-1)
		   	data = orig(xoffzoom:xend,yoffzoom:yend)		
		     	widget_control,/hourglass
		     	widget_control,min_field,get_value=minv
			title='Region ['+strtrim(xoffzoom,2)+':'+ $
			      strtrim(xend,2)+', '+strtrim(yoffzoom,2)+':'+ $
			      strtrim(yend,2)	 
		     	fuse_scan_stats,data,minv,title=title,group=event.top
		     	widget_control,/hourglass
		   end
		   end
;
; PHA whole image
;
		5: begin
			if n_elements(pha) lt 2 then begin
				r = dialog_message(dialog_parent = event.top, $
					'Time-Tag data not available',/error)
				return
			end
			good = where(xttag_goodmask(timetag,gtime1,gtime2),ng)
			if ng eq 0 then begin
				r=dialog_message('No events found',/error, $
						dialog_parent=event.top)
				return
			end
			phahist = histogram(pha(good),min=1,max=31)
			phahist = phahist/total(phahist)
			lineplot,findgen(31)+1,phahist,title='Whole Image', $
				xtitle='PHA',ytitle='Fraction of events'
		   end
;
; 	Sub Array
;
		 else: begin
		     if val eq 6 then val=9	;state for box pha
		     state = [val,0,-1,-1]
		     if state(0) ne 2 then mess = $
		 	'Place cursor on first corner and push left button' $
			else mess = $			
			'Push mouse button, hold down, trace region,'+ $
			' then release'
		     widget_control,message,set_value=mess
		     end
		 endcase
		 end
;

	'PLOT' : fuse_scan_lineplot,event
	else:
	endcase
return
end
;=========================================================== FUSE_SCAN_EXTEN
; Routine to read a image extension
;
pro fuse_scan_exten,header,orig,status,file,group=group,dir=dir
	common fuse_timetag_data,x,y,time,xbin,ybin,gtime1,gtime2,pha
;
; get file name
;
	if n_elements(directory) eq 0 then directory=''
	file = dialog_pickfile(title='Select FUSE data file(s)',/must_exist, $
		filter=directory+'*.fit')
	if file(0) eq '' then begin
		status = 'No File Selected'
		return
	endif
;
; read file
;
	fits_open,file,fcb,/no_abort,message=status
	if !err lt 0 then goto,error_exit
;
; find valid extensions
;
	naxis = fcb.naxis
	xtension = strtrim(fcb.xtension)
	good = where((naxis eq 2) and (xtension ne 'BINTABLE'),ngood)
	if ngood eq 0 then begin
		status = 'No Valid Image extensions found in file'
		goto,error_exit
	end
;
; select extension
;
	if ngood eq 1 then begin
		exten = good(0)
	   end else begin
	   	list = '#'+strtrim(good,2)+'      '+ $
			strtrim(fcb.axis(0,good),2)+' x ' + $
			strtrim(fcb.axis(1,good),2) + '    '+ $
			strtrim(abs(fcb.bitpix(good)),1)+ ' bit              '
		select_list,'Select File Extension',list,isel
		if n_elements(isel) eq 0 then begin
			status='No extension selected'
			fits_close,fcb
			return
		end
		exten = good(isel(0))
	end
;
; read data
;
	time = 0
	x = 0
	y = 0
	pha = 0
	fits_read,fcb,orig,header,/no_abort,message=status,exten=exten
	err = !err
	fits_close,fcb
	fdecomp,file,disk,dir,name,ext
	file = disk+dir+name+'['+strtrim(exten,2)+'].'+ext
	if err lt 0 then goto,error_exit
	status=''
	return
;
; error exit
;
error_exit:
	result = dialog_message(status,dialog_parent = group,/error)
	return
end
;================================================================= SELECT_LIST
;
; Routine to select a list element
;
PRO select_list_event, event
;
;This procedure is the event handler for select_list
;
	COMMON select_list, val
	WIDGET_CONTROL, event.id, GET_VALUE = value, GET_UVALUE = val
	widget_control, event.top, /DESTROY
	return
END

PRO select_list, command_line, items, iselected, group=group, modal=modal
 	common select_list, val
	val = -1


	base = WIDGET_BASE( TITLE = command_line, /COLUMN, MODAL=MODAL,	$
		GROUP_LEADER=GROUP)
       	XMENU, items, base, COLUMN=1,y_scroll_size=300,x_scroll_size=250
;
; Realize the widgets:
;
 	WIDGET_CONTROL, base, /REALIZE
	XMANAGER, 'select_list', base, GROUP_LEADER = GROUP
	iselected = val
 return
 end


;=========================================================== FUSE_SCAN_POSITION
;
; Routine to report regions displayed
;
pro fuse_scan_position
common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze

;
; big window
;
	widget_control,big_window,get_draw_view=v
	xr = [v(0),(v(0)+744)<(ns-1)]
	yr = [v(1),(v(1)+514)<(nl-1)]
	fuse_scan_xyttag,xr,yr
	widget_control,big_position,set_value=strtrim(xr(0),2)+':'+ $
		strtrim(xr(1),2)+'  '+strtrim(yr(0),2)+':'+ $
		strtrim(yr(1),2)
;
; zoom window
;
	xend = (xoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	yend = (yoffzoom + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
	xr = [xoffzoom,xend]
	yr = [yoffzoom,yend]
	fuse_scan_xyttag,xr,yr
	widget_control,zoom_position,set_value=strtrim(xr(0),2)+':'+ $
		strtrim(xr(1),2)+'  '+strtrim(yr(0),2)+':'+ $
		strtrim(yr(1),2)
	return
	end
;============================================================ FUSE_SCAN_PLOTS
pro FUSE_SCAN_PLOTS,xx,yy,color=color,overlay=overlay,linestyle=linestyle

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
;
; plot vector in all three windows
;
;
; convert to window coordinates for all three windows and plot
;
	if n_elements(color) eq 0 then color = !d.n_colors-1
	if keyword_set(overlay) then color=!d.n_colors-1
	if n_elements(linestyle) eq 0 then linestyle=0

	wset,big_id
	if keyword_set(overlay) then device,set_graphic=6
	plots,xx,yy,/dev,color=color,line=linestyle

	wset,little_id
	if keyword_set(overlay) then device,set_graphic=6
	factorx = 1024./ns
	factory = 128.0/nl
	plots,long(xx*factorx),long(yy*factory),/dev,color=color,line=linestyle

	wset,zoom_id
	if keyword_set(overlay) then device,set_graphic=6
	xpos = (xx-xoffzoom)*long(zoom_factor) + zoom_factor/2
	ypos = (yy-yoffzoom)*long(zoom_factor) + zoom_factor/2
	plots,xpos>(-32768)<32767,ypos>(-32768)<32767, /dev, $
	      		color=color,line=linestyle
	device,set_graphic=3
return
end
;
;========================================================== FUSE_SCAN_SET_REGION
;
; widget to set display regions
;
pro fuse_scan_set_region,group=group

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze

	base = widget_base(/col,group=group,/modal)
	done = widget_button(base,value='Done')
	label = widget_label(base,value='Large Window')
	widget_control,big_window,get_draw_view=v
	x = v(0) & y = v(1)
	fuse_scan_xyttag,x,y
	xlarge = cw_field(base,/row,title='XMIN:',/long,/return_events, $
		value=x, xsize=13)
	ylarge = cw_field(base,/row,title='YMIN:',/long,/return_events, $
		value=y, xsize=13)
	label = widget_label(base,value='Zoom Window')
	x = xoffzoom
	y = yoffzoom
	fuse_scan_xyttag,x,y
	xzoom = cw_field(base,/row,title='XMIN:',/long,/return_events, $
		value = x,xsize=13)
	yzoom = cw_field(base,/row,title='YMIN:',/long,/return_events, $
		value = y,xsize=13)
	fzoom = cw_field(base,/row,title='Zoom Factor:',/long, $
		/return_events,xsize=7,value=zoom_factor)
	widget_control,base,/realize
	xbase = {done:done,xlarge:xlarge,ylarge:ylarge,xzoom:xzoom, $
		yzoom:yzoom,fzoom:fzoom}
	info = ptr_new(xbase)
	widget_control,base,set_uvalue=info
	xmanager,'fuse_scan_set_region',base,/no_block
return
end
pro fuse_scan_set_region_event,event

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze

	widget_control,event.top,get_uvalue=info
	widget_control,(*info).xzoom,get_value=x
	widget_control,(*info).yzoom,get_value=y
	widget_control,(*info).fzoom,get_value=v
	fuse_scan_xyttag,x,y,/to_binned
	zoom_factor = v(0)>1<32
	xoffzoom = x(0)>0<(ns-256/zoom_factor)
	yoffzoom = y(0)>0<(nl-256/zoom_factor)
	fuse_scan_zoom,orig,zoom_factor,xoffzoom,yoffzoom,zoom
	fuse_scan_scale,zoom_id,zoom
	widget_control,(*info).xlarge,get_value=x
	widget_control,(*info).ylarge,get_value=y
	fuse_scan_xyttag,x,y,/to_binned
	x = x(0)>0<(ns-745)
	y = y(0)>0<(nl-515)
	widget_control,big_window,set_draw_view=[x,y]
	fuse_scan_position

	if event.id eq (*info).done then begin
			if ptr_valid(info) then ptr_free,info
			widget_control,event.top,/destroy
	end
	return
end

	
	
;
;============================================================ FUSE_SCAN_DISPLAY
; 
; Routine to set up and display image all three windows
;
pro fuse_scan_display, image

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	widget_control,/hourglass
common fuse_timetag_data,xttag,yttag,timetag,xbin,ybin,gtime1,gtime2,pha
;
; process new image?
;
	if n_params(0) gt 0 then begin		;new image?
		orig = image
		omin = min(orig)
		omax = max(orig)
		widget_control,freeze,get_value=v
		if v eq 'Freeze Min/Max' then begin
			widget_control,min_field,set_value=float(omin)
			widget_control,max_field,set_value=float(omax)
		end
		s = size(orig) & ns = s(1) & nl = s(2)
		nsout = 1024
		nlout = 128
		little_image = frebin(image,nsout,nlout)
		nxsize = ns
		if !version.os_family eq 'Windows' then nxsize = nxsize<8190
		widget_control,big_window,draw_xsize=nxsize,draw_ysize=nl, $
			scr_xsize=750<(ns+30),scr_ysize=542<(nl+30)
	end
	widget_control,message,set_value=scale_type+' Display'
;
; display big image
;
	fuse_scan_scale,big_id,orig
;
; display little image
;	
	fuse_scan_scale,little_id,little_image
;
; display zoom image
;
	fuse_scan_zoom,orig,zoom_factor,xoffzoom,yoffzoom,zoom
	fuse_scan_scale,zoom_id,zoom
;
; update overlay
;
	if overlay_on then fuse_scan_overlay,header,xbin,ybin,big_id,little_id
;
; update histogram
;
	if xregistered('fuse_scan_histogram') then begin
		fuse_scan_histogram_comp
		fuse_scan_histogram_plot
	end
;
; update positions
;
	fuse_scan_position		

	wset,big_id
	widget_control,/hourglass
	return
end
; ============================================================ FUSE_SCAN_SCALE
;
; Routine to scale and display an image
;
pro fuse_scan_scale, window_id, image

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	

	widget_control,min_field,get_value=imin
	widget_control,max_field,get_value=imax
	case scale_type of 
		'Linear': pic = bytscl(image,min=imin,max=imax,top=!d.n_colors)
		'Log': 	begin
			tmin=imax/1e4
			pic = bytscl(alog10((image-imin)>tmin), $
					min=alog10(tmin), $
					max=alog10(imax-imin),top=!d.n_colors)
			end
		'Sqrt': pic = bytscl(sqrt((image-imin)>0),min=0, $
					max=sqrt(imax-imin),top=!d.n_colors)
		'Hist. Eq.': pic = hist_equal(image,minv=imin,maxv=imax, $
					top=!d.n_colors)
	endcase

	wset,window_id
	erase
	tv,pic
	return
end
;
;============================================================== FUSE_SCAN_ZOOM
;
; ROUTINE TO CREATE ZOOMED IMAGE -
;
pro fuse_scan_zoom,image,zoom_factor,xoff,yoff,zoom
	s = size(image) & ns = s(1) & nl = s(2)
	if (xoff lt 0) or (yoff lt 0) or $
	   (xoff ge ns-1) or (yoff ge nl-1) then begin
	   	zoom = fltarr(256,256)
		return
	end 
	xend = (xoff + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	yend = (yoff + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
	zoom = rebin(image(xoff:xend,yoff:yend),(xend-xoff+1)*zoom_factor, $
						(yend-yoff+1)*zoom_factor,/samp)
	fuse_scan_position
	return
end
;
; ============================================================== FUSE_SCAN_XVAL
;
; ROUTINE TO UPDATE X/Y/Value fields
;
pro fuse_scan_xyval,image,x,y,zoomfactx,zoomfacty,xoff,yoff

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	
	s = size(image) & ns = s(1) & nl = s(2)
	xx = xoff + fix(x/zoomfactx)
	yy = yoff + fix(y/zoomfacty)
	if (xx ge 0) and (xx le ns-1) and (yy ge 0) and (yy le nl-1) then begin
		widget_control,val_field,set_value=float(image(xx,yy))
		fuse_scan_xyttag,xx,yy
		widget_control,x_field,set_value=xx
		widget_control,y_field,set_value=yy
	endif
	return
end

;=========================================================== FUSE_SCAN_XYTTAG
; Routine to convert back and forth from binned and raw timetag positions
;
pro fuse_scan_xyttag,x,y,to_binned=to_binned,frac=frac
;
	common fuse_timetag_data,xttag,yttag,timetag,xbin,ybin,gtime1,gtime2
	if n_elements(to_binned) eq 0 then to_binned = 0
	if n_elements(frac) eq 0 then frac = 0
	if n_elements(xbin) eq 0 then xbin = 1
	if n_elements(ybin) eq 0 then ybin = 1
	if to_binned then begin
		if frac then begin
			x = (x - (xbin-1)/2.0)/xbin
			y = (y - (ybin-1)/2.0)/ybin
		   end else begin
		   	x = long(x)/xbin
			y = long(y)/ybin
		end
	   end else begin
	   	if frac then begin
			x = float(x)*xbin + (xbin-1)/2.0	
			y = float(y)*ybin + (ybin-1)/2.0
		   end else begin
		   	x = long(x)*xbin
			y = long(y)*ybin
		end
	end
return
end

; ==================================================  FUSE_SCAN_HISTOGRAM_EVENT
;
; Histogram event handler
;
pro fuse_scan_histogram_event,event

common fuse_scan_histogram_common,hist_id,loghist, $
	hxrange,hyrange,xhist,yhist, $
	hmin_field,hmax_field,hslider

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	

	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
	    'EXIT' : begin
			widget_control,event.top,/destroy
			return
			end
	    'LINEAR': 	loghist = 0
	    'LOG' : 	loghist = 1
	    'RESET' : begin
			widget_control,min_field,get_value=minv
			widget_control,max_field,get_value=maxv	
			widget_control,hmin_field,set_value=minv
			widget_control,hmax_field,set_value=maxv
	     		end
	     		
	     else: 
	endcase
	fuse_scan_histogram_plot
	return
end
;
; ===================================================  FUSE_SCAN_HISTOGRAM_COMP
; 
; Routine to compute the histogram
;
pro fuse_scan_histogram_comp
common fuse_scan_histogram_common,hist_id,loghist, $
	hxrange,hyrange,xhist,yhist, $
	hmin_field,hmax_field,hslider

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	

	widget_control,min_field,get_value=minv
	widget_control,max_field,get_value=maxv
	range = maxv-minv
	if (datatype(orig) eq 'INT') or (datatype(orig) eq 'LON') then begin
		minv = long(minv)-0.5
		maxv = long(maxv)+1.5
		if range gt 65536 then binsize=range/50000.0 else binsize=1
	   end else begin	
		if (range gt 500) and (range lt 65536) then begin
			minv = long(minv)-0.5
			maxv = long(maxv)+1.5
			binsize=1
	   	    end else begin
	   		binsize = (maxv-minv)/50000
		end
	end
	yhist = histogram(orig,min=minv,max=maxv,binsize=binsize)
	xhist = lindgen(n_elements(yhist))*binsize+(minv+binsize/2)
	widget_control,hmin_field,set_value=minv
	widget_control,hmax_field,set_value=maxv

return
end
; ==================================================== FUSE_SCAN_HISTOGRAM_PLOT
;
; Routine to plot the histgram
;
pro fuse_scan_histogram_plot
common fuse_scan_histogram_common,hist_id,loghist, $
	hxrange,hyrange,xhist,yhist, $
	hmin_field,hmax_field,hslider

	widget_control,hmin_field,get_value=xmin
	widget_control,hmax_field,get_value=xmax
	widget_control,hslider,get_value=factor
	wset,hist_id
;
; determine binning for the histogram
;
	good = where((xhist ge xmin) and (xhist le xmax),ngood)
	if ngood eq 0 then begin
		ngood = n_elements(xhist)
		good = lindgen(xhist)
	end
	nsum = 1
	while ngood/nsum gt 500 do nsum = nsum + 1
;
; determine yrange
;
	ymin = 0.0
	if loghist then ymin = 0.1
	ymax = max(yhist(good))*factor
	plot,xhist,yhist>0.1,nsum=nsum,ylog=loghist,xrange=[xmin,xmax], $
			yrange = [ymin,ymax],xstyle=1,ystyle=1, $
			ytitle='',xtitle='',title='',psym=10
			
	return
end
;
; ======================================================== FUSE_SCAN_HISTOGRAM	
;
; Main Widget Driver for Histograms
;
pro fuse_scan_histogram,group

common fuse_scan_histogram_common,hist_id,loghist, $
	hxrange,hyrange,xhist,yhist, $
	hmin_field,hmax_field,hslider
common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
	
;
; initialization
;
	hxrange = [0.0, 0.0]
	hyrange = [0.0, 0.0]
	loghist = 0

	base = widget_base(/col,group=group)
	basebar = widget_base(base,/row)
	button = widget_button(basebar,uvalue='EXIT',value='EXIT')
	button = widget_button(basebar,uvalue='LINEAR',value='LINEAR')
	button = widget_button(basebar,uvalue='LOG',value='LOG')
	widget_control,min_field,get_value=minv
	widget_control,max_field,get_value=maxv	
        hmin_field = cw_field(basebar,/row,uvalue='MIN_FIELD',value=minv, $
                title='Xmin: ',xsize=13,/return_events,/float)
        hmax_field = cw_field(basebar,/row,uvalue='MAX_FIELD',value=maxv, $
                title='Xmax: ',xsize=13,/return_events,/float)
	button = widget_button(basebar,uvalue='RESET',value='RESET')

	base1 = widget_base(base,/row)	
	hist_window = widget_draw(base1,uvalue='HIST_WINDOW',retain=2, $
			xsize=600,ysize=350,/button_events)
	hslider = cw_fslider(base1,/drag,min=0.01,max=1.0,uvalue='SLIDER', $
		/vertical,/suppress_value,value=1.0,ysize=350)
	widget_control,base,/realize
	widget_control,hist_window,get_value=hist_id
	fuse_scan_histogram_comp
	fuse_scan_histogram_plot
	xmanager,'fuse_scan_histogram',base,/no_block,group=group
	return
	end
;
; ============================================================= FUSE_SCAN_GFIT
;
;  Integrated Gaussian Fit event driver
;
pro fuse_scan_gfit_event,event
widget_control,event.top,/destroy
return
end
;
; Integrated Gaussian Fit Widget
;
pro fuse_scan_gfit,image,x,y,type,group=group
;
; compute fits
;
	profile1 = total(image,2)
	profile2 = total(image,1)
	fuse_scan_xyttag,x,y,/frac
	fit1 = gaussint_fit(x,profile1,coef1,nterms=5,absorption=type-1)
	fit2 = gaussint_fit(y,profile2,coef2,nterms=5,absorption=type-1)
;
; create widget layout
;
	base = widget_base(/col,group=group)
	button = widget_button(base,uvalue='DONE',VALUE='DONE')
	basex = widget_base(base,/row)
	base1 = widget_base(basex,/col)
	base2 = widget_base(basex,/col)

	lab = widget_label(base1,value='Horizontal Profile')
	lab = widget_label(base2,value='Verticle Profile')

	draw1 = widget_draw(base1,xsize=450,ysize=300)
	draw2 = widget_draw(base2,xsize=450,ysize=300)

	lab = widget_label(base1,value='X-center =' + $
			string(coef1(1),'(F8.2)'))
	lab = widget_label(base2,value='Y-center =' + $
			string(coef2(1),'(F8.2)'))

	lab = widget_label(base1,value='X FWHM =' + $
			string(coef1(2)*2.3548,'(F8.2)'))
	lab = widget_label(base2,value='Y FWHM =' + $
			string(coef2(2)*2.3548,'(F8.2)'))
;
; create widget
;
	widget_control,base,/realize
	widget_control,draw1,get_value=window1
	widget_control,draw2,get_value=window2
;
; Plot Profiles
;
	back1 = coef1(3) + coef1(4)*x
	xx = congrid(x,500,/interp)
	gaussx,xx,coef1(1),coef1(2),coef1(0),yy
	yy = yy + coef1(3) + coef1(4)*xx
	wset,window1

	plot,x,profile1,psym=10,xstyle=1, $
		yrange= [ min(yy)<min(profile1) , max(yy)>max(profile1) ]
	oplot,x,fit1,psym=10,line=2
	oplot,xx,yy,thick=2
	oplot,x,back1,line=1

	back2 = coef2(3) + coef2(4)*y

	xx = congrid(y,500,/interp)
	gaussx,xx,coef2(1),coef2(2),coef2(0),yy
	yy = yy + coef2(3) + coef2(4)*xx
	
	wset,window2

	plot,y,profile2,psym=10,xstyle=1, $
		yrange = [ min(yy)<min(profile2) , max(yy)>max(profile2) ]	
	oplot,y,fit2,psym=10,line=2
	oplot,xx,yy,thick=2
	oplot,y,back2,line=1
	xmanager,'fuse_scan_gfit',base,/no_block
	return
end
;
; ============================================================= FUSE_SCAN_STATS
;
; Routine to compute statitics and print statistics
;
pro fuse_scan_stats,region,background,group=group,title=title
;
; compute statistics
;
	if n_elements(title) eq 0 then title='Statistics'
	if n_elements(background) eq 0 then background=0.0
	n = n_elements(region)
	minv = min(region,max=maxv)
	med = median(region)
	sig = stdev(region,mean)
	tot = mean*n
	medback = med-background
	totback = tot - background*n
	meanback= mean-background
	minvb = minv - background
	maxvb = maxv - background
;
; display result in a widget
;
	base = widget_base(group=group,/col,title=title)
	button = widget_button(base,value = 'DONE')
	lab = widget_label(base,/align_left,value='  NPoints = '+strtrim(n,2))
	lab = widget_label(base,/align_left, $
				value='  Minimum = '+strtrim(minv,2))
	lab = widget_label(base,/align_left, $
			value='  Maximum = '+strtrim(maxv,2))
	lab = widget_label(base,/align_left,value='  Total = '+strtrim(tot,2))
	lab = widget_label(base,/align_left,value='  Median = '+strtrim(med,2))
	lab = widget_label(base,/align_left,value='  Mean = '+strtrim(mean,2))
	lab = widget_label(base,/align_left,value='  StDev = '+strtrim(sig,2))
	lab = widget_label(base,/align_left,value=' ')
	lab = widget_label(base,value='Results after background subtraction')
	lab = widget_label(base,/align_l, $
			value='  Background = '+strtrim(background,2))
	lab = widget_label(base,/align_l,value='  Minimum = '+strtrim(minvb,2))
	lab = widget_label(base,/align_l,value='  Maximum = '+strtrim(maxvb,2))
	lab = widget_label(base,/align_l,value='  Total = '+strtrim(totback,2))
	lab = widget_label(base,/align_l,value='  Median = '+strtrim(medback,2))
	lab = widget_label(base,/align_l,value='  Mean = '+strtrim(meanback,2))
	widget_control,base,/realize
	xmanager,'fuse_scan_stats',base,/no_block
	return
end
pro fuse_scan_stats_event,event
widget_control,event.top,/destroy
return
end
;
; ===========================================================  FUSE_SCAN_DEFROI
;
; Routine to define region of interest
;
pro fuse_scan_defroi,event,window_id

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
common fuse_timetag_data,xttag,yttag,timetag,xbin,ybin,gtime1,gtime2,pha
	
;
; State Vector
;	State(0) = 0 not processing stats
;		   1 Processing Box
;		   2 Processing Defroi
;		   9 Box PHA
;	State(1) = 0 Ready to begin processing
;		 = 1 Button Pressed for first position of box or button
;			Pressed for defroi
;		 = 2 Button pressed for second position of box or button
;			released for defroi
;	State(2) = window id of window being processed
;
;
; convert x and y to data coordinates
;
	x = event.x
	y = event.y
	case window_id of
		big_id: begin
			xdata = x
			ydata = y
			end
		little_id: begin
			factorx = 1024.0/ns
			factory = 128.0/nl
			xdata = long(x/factorx)
			ydata = long(y/factory)
			end
		zoom_id: begin
			xdata = long(x/zoom_factor) + xoffzoom
			ydata = long(y/zoom_factor) + yoffzoom
			end
	endcase
;
; BOX Processing
;
	if state(0) ne 2 then begin
;
; erase previous overlay
;
	    if (state(1) eq 0) or $
	       ((state(1) eq 1) and (xregion(1) eq -1)) then begin
	    	    fuse_scan_plots,[state(2),state(2)],[0,nl],color=255,/o
		    fuse_scan_plots,[0,ns],[state(3),state(3)],color=255,/o
		    state(2) = [-1,-1]
	    	end else begin
		    xx = [xregion(0),xregion(1),xregion(1), $
					xregion(0),xregion(0)]
		    yy = [yregion(0),yregion(0),yregion(1), $
					yregion(1),yregion(0)]
		    fuse_scan_plots,xx,yy,color=255,/o
	    end
	    if event.press eq 0 then begin
		if state(1) eq 0 then begin
		    	state(2) = [xdata,ydata]
		    	fuse_scan_plots,[state(2),state(2)],[0,nl],color=255,/o
		    	fuse_scan_plots,[0,ns],[state(3),state(3)],color=255,/o
		    end else begin
			xregion(1) = xdata
			yregion(1) = ydata
		   	xx = [xregion(0),xregion(1),xregion(1), $
					xregion(0),xregion(0)]
		   	yy = [yregion(0),yregion(0),yregion(1), $
					yregion(1),yregion(0)]
		        fuse_scan_plots,xx,yy,color=255,/o
		end
		return
	    end		
		
	    case state(1) of
	    	0: begin			;processing not started
		   state(1) = 1
		   xregion(0) = xdata
		   yregion(0) = ydata
		   xregion(1) = -1
		   yregion(1) = -1
		   nregion = 1
		   widget_control,message,set_value = 'Place cursor on ' + $
		   		'opposite corner and press left button'
		   return
		   end
		1: begin			;waiting for second corner
		   state(1) = 2
		   xregion(1) = xdata
		   yregion(1) = ydata
		   xregion(0) = [xregion(0),xregion(1),xregion(1), $
					xregion(0),xregion(0)]
		   yregion(0) = [yregion(0),yregion(0),yregion(1), $
					yregion(1),yregion(0)]
		   nregion = 5
		   end
	    endcase
	 end else begin
;
; Draw Processing
; 
	    case state(1) of
	    	0: begin			;not yet started
		   if event.press eq 1 then begin
		   	state(1) = 1
			xregion(0) = xdata
			yregion(0) = ydata
			nregion = 1
		   end
		   return
		   end
		1: begin
		   xregion(nregion) = xdata
		   yregion(nregion) = ydata
		   nregion = nregion + 1
		   if event.release gt 0 then state(1) = 2
		   end
	    endcase
	end
;
; Draw region in all three windows
;
	i1 = nregion-2
	i2 = nregion-1
	if (state(0) eq 1) or (state(0) eq 9) then i1 = 0	;draw entire box
	xx = xregion(i1:i2)
	yy = yregion(i1:i2)
	if (state(0) eq 2) and (state(1) eq 2) then begin  ;back to first point
		xx = [xx,xregion(0)]
		yy = [yy,yregion(0)]
	end
;
; convert to window coordinates for all three windows and plot
;
	fuse_scan_plots,xx,yy
;
; process statistics
;
	if state(1) lt 2 then return
	widget_control,message,set_value = ' '
	if state(0) eq 9 then begin	;pha
		if n_elements(pha) lt 2 then begin
			r = dialog_message('Timetag events not available', $
					dialog_parent = event.top,/error)
		   end else begin
			fuse_scan_xyttag,xregion,yregion
		   	x1 = min(xregion(0:4))
			x2 = max(xregion(0:4))
			y1 = min(yregion(0:4))
			y2 = max(yregion(0:4))
		   	good = where((xttag ge x1) and (xttag le x2) and $
				     (yttag ge y1) and (yttag le y2) and $
				     xttag_goodmask(timetag,gtime1,gtime2), $
				     ngood)
			if ngood lt 1 then begin
			   	r = dialog_message('No events in box',/error, $
					dialog_parent = event.top)
			    end else begin
			    	phahist = histogram(pha(good),min=1,max=31)
				title = 'Region:('+strtrim(x1,2)+':'+ $
					strtrim(x2,2)+','+strtrim(y1,2)+ $
					':'+strtrim(y2,2)+')'
				lineplot,findgen(31)+1,phahist/total(phahist), $
					title=title,xtitle='PHA', $
					ytitle='Fraction of events'
			end
		end
	   end else begin
	   	if nregion gt 2 then begin
			index = polyfillv(xregion(0:nregion-1), $
						yregion(0:nregion-1),ns,nl)
			n = n_elements(index)
			widget_control,min_field,get_value=minv
			if state(0) eq 1 then title='Statistics in Box' $
					 else title='Statistics in Drawn Region'
			if n gt 1 then begin
				widget_control,/hourglass
				fuse_scan_stats,orig(index),minv,group = event.top
				widget_control,/hourglass
			end
		end
	end
	nregion = 0		
	state = [0,0,-1,-1]
return
end
;
; =========================================================  FUSE_SCAN_LINEPLOT
;
; Routine to plot row/column sums
;
pro fuse_scan_lineplot,event,window
;

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
common fuse_timetag_data,xttag,yttag,timetag,xbin,ybin,gtime1,gtime2
;
; State Vector
;
;	State(0) = 3 Row
;                  4 Column
;		   5 Row Sum
;		   6 Column Sum
;		   7 Linefind rowsum
;		   8 Cross Section
;	State(1) = Number of points measured
;
;
; If window not supplied, initialize state vector, and print instructions
;
;
	fdecomp,filename,disk,dir,file
	if n_params(0) lt 2 then begin
		State = [(event.value+2)<8,0,-1,-1,0]
		case state(0) of
			3: mess = 'Select row and click left mouse button'
			4: mess = 'Select column and click left mouse button'
			5: mess = 'Select first row and click left button'
			6: mess = 'Select first column and click left button'
			7: mess = 'Select first row and click left button'
			8: begin
			   mess = 'Select first point and click left button'
			   widths = [1,3,5,7,9,11,15,25,35,55,75]
			   state(4) = widths(event.value-7)
			   xregion(0) = [-1,-1]
			   end
		endcase
		widget_control,message,set_value=mess
		return
	end
;
; erase previous overlay
;
	if state(3) ge 0 then begin
	    case state(0) of 
		3: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		4: fuse_scan_plots,[state(2),state(2)],[0,nl],/o,color=255
		5: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		6: fuse_scan_plots,[state(2),state(2)],[0,nl],/o,color=255
		7: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		8: if xregion(1) ne -1 then begin
		   	xsection_corners,xregion(0),yregion(0), $
				xregion(1),yregion(1),state(4),xcorn,ycorn,dist
			fuse_scan_plots,[xcorn,xcorn(0)],[ycorn,ycorn(0)],/o, $
					color=255
		   endif
			
	    end
	end
;
; convert x and y to data coordinates
;
	fuse_scan_convert,event.x,event.y,window,x,y
	xregion(state(1)) = x
	yregion(state(1)) = y
	state(2) = [x,y]
;
; If button not pressed then plot new overlay and return
;
	if event.press ne 1 then begin
	    case state(0) of 
		3: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		4: fuse_scan_plots,[state(2),state(2)],[0,nl],/o,color=255
		5: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		6: fuse_scan_plots,[state(2),state(2)],[0,nl],/o,color=255
		7: fuse_scan_plots,[0,ns],[state(3),state(3)],/o,color=255
		8: begin
		   if state(1) eq 1 then begin
		   	xsection_corners,xregion(0),yregion(0), $
				xregion(1),yregion(1),state(4),xcorn,ycorn,dist
			fuse_scan_plots,[xcorn,xcorn(0)],[ycorn,ycorn(0)],/o, $
					color=255
		   endif
		   end
	    end
	    return
	end
;
; button pressed
;
	state(1) = state(1) + 1
;
; Row Plot
;
	if state(0) eq 3 then begin
		fuse_scan_plots,[0,ns],[state(3),state(3)],line=1
		xrange = [0,ns]	
		if window eq 'ZOOM' then $
				xrange = [xoffzoom,xoffzoom+256/zoom_factor]
		if window eq 'BIG' then begin
			widget_control,big_window,get_draw_view=v
			xrange = [v(0),(v(0)+720)<(ns-1)]
		end
		fuse_scan_xyttag,xrange,0,/frac
		xv = findgen(ns)
		fuse_scan_xyttag,xv,0,/frac
		lineplot,xv,orig(*,y),title=file+'  Row '+ $
			strtrim(y*ybin,2),xrange=xrange
	end
;
; Column Plot
;	
	if state(0) eq 4 then begin
		fuse_scan_plots,[state(2),state(2)],[0,nl],line=1	
		xrange = [0,nl]
		if window eq 'ZOOM' then $
				xrange = [yoffzoom,yoffzoom+256/zoom_factor]
		if window eq 'BIG' then begin
			widget_control,big_window,get_draw_view=v
			xrange = [v(1),(v(1)+512)<(nl-1)]
		end
		fuse_scan_xyttag,0,xrange,/frac
		xv = findgen(nl)
		fuse_scan_xyttag,0,xv,/frac
		lineplot,xv,reform(orig(x,*)),title=file+'  Column '+ $
				strtrim(x*xbin,2),xrange=xrange
	end
;
; Row Sum
;
	if (state(0) eq 5) or (state(0) eq 7) then begin
	    if (state(1) eq 2) then begin
	        fuse_scan_plots,[0,ns],[yregion(0),yregion(0)],/o,color=255
		fuse_scan_plots,[0,ns],[state(3),state(3)],line=1
		xrange = [0,ns]
		if window eq 'ZOOM' then $
				xrange = [xoffzoom,xoffzoom+256/zoom_factor]
		if window eq 'BIG' then begin
			widget_control,big_window,get_draw_view=v
			xrange = [v(0),(v(0)+720)<(ns-1)]
		end
		y1 = yregion(0)<yregion(1)
		y2 = yregion(0)>yregion(1)
		if y1 eq y2 then data = orig(*,y1) $
			    else data = total(orig(*,y1:y2),2)
		title = 'Rows '+strtrim(y1*ybin,2)+' to '+ strtrim(y2*ybin,2)
		fuse_scan_xyttag,xrange,0,/frac
		xv = findgen(ns)
		fuse_scan_xyttag,xv,0,/frac

		if state(0) eq 5 then $
			lineplot,xv,data,title=file+'  '+title,xrange=xrange $
		    else $
			fuse_linefind,xv,data,xv,header,file+'  '+title, $
				group=event.top,/modal
	    end else fuse_scan_plots,[0,ns],[state(3),state(3)],line=1
	end
;
; Column Sum
;
	if (state(0) eq 6) then begin
	    if (state(1) eq 2) then begin	
	        fuse_scan_plots,[xregion(0),xregion(0)],[0,nl],/o,color=255
		fuse_scan_plots,[state(2),state(2)],[0,nl],line=1
		xrange = [0,nl]
		if window eq 'ZOOM' then $
				xrange = [yoffzoom,yoffzoom+256/zoom_factor]
		if window eq 'BIG' then begin
			widget_control,big_window,get_draw_view=v
			xrange = [v(1),(v(1)+512)<(nl-1)]
		end
		x1 = xregion(0)<xregion(1)
		x2 = xregion(0)>xregion(1)
		if x1 eq x2 then data = reform(orig(x1,*)) $
			    else data = total(orig(x1:x2,*),1)
		title = 'Columns '+strtrim(x1*xbin,2)+' to '+ strtrim(x2*xbin,2)
		fuse_scan_xyttag,xrange,0,/frac
		xv = findgen(nl)
		fuse_scan_xyttag,xv,0,/frac
		lineplot,xv,data,title=file+'  '+title,xrange=xrange
	    end else fuse_scan_plots,[state(2),state(2)],[0,nl],line=1
	end
;
; x-section plot
;
	if (state(0) eq 8) and (state(1) eq 2) then begin
		xsection_corners,xregion(0),yregion(0), $
				xregion(1),yregion(1),state(4),xcorn,ycorn,dist
		fuse_scan_plots,[xcorn,xcorn(0)],[ycorn,ycorn(0)]
		xsection_compute,orig,xcorn,ycorn,dist,state(4),data
		fuse_scan_xyttag,xregion,yregion,/frac
		dist = sqrt((xregion(1)-xregion(0))^2 +  $
			    (yregion(1)-yregion(0))^2)
		title='X '+strtrim(xregion(0),2)+' '+strtrim(yregion(0),2)+ $
			' to '+strtrim(xregion(1),2)+' '+strtrim(yregion(1),2)
		xv = findgen(n_elements(data))
		xv = xv/max(xv)*dist
		lineplot,xv,data,title=file+'  '+title,xrange=[0,dist]

	end
		
;
; Do we need second point?
;
	if (state(0) gt 4) and (state(1) eq 1) then begin
		if (state(0) eq 5) or (state(0) eq 7) then mess = $
				'Select last row and click left button' $
			else mess = 'Select last column and click left button'
		if state(0) eq 8 then mess = $
				'Select ending point and click left button'
	    	widget_control,message,set_value = mess
	    end else begin
	    	widget_control,message,set_value = scale_type + ' Display'
		state = [0,0,-1,-1]
	end
	return
end

;============================================================= FUSE_SCAN_OVERLAY
; Routine to plot wavelength overlay
;
pro fuse_scan_overlay,h,xbin,ybin,big_id,little_id


	detector = strtrim(sxpar(h,'detector'),2)
;
; get coefficients
;
	case detector of
	  '1A': begin
	  	coef = [[-135022.83,       127.97941D0,    0.0099817230D0], $
	   		[175071.24,      -156.89625D0,   -0.0024052021D0]]
		wmin = [990,1000]
		wmax = [1080,1090]
		end
	  '1B': begin
	  	coef = [[-139906.61,       109.03151D0,     0.018138421D0], $
	   		[161618.17,      -162.80883D0,    0.0011034282D0]]
		wmin = [1090,900]
		wmax = [1190,990]
		end
	  '2A': begin
	  	coef = [[167070.27,      -130.72053D0,   -0.0082661123D0], $
	   		[-139746.96,       147.48503D0,    0.0066074294D0]]
		wmin = [1090,920]
		wmax = [1180,1010]
		end
	  '2B': begin
	  	coef = [[166445.98,      -159.89416D0,    0.0057074141D0], $
	   		[-136294.76,       110.58599D0,     0.024242810D0]]
		wmin = [980,1020]
		wmax = [1080,1100]
		end
	  else: return
	endcase
;
; plot on big window
;
	wset,big_id
	!fancy = 1
	ypos = [1024/ybin-25,25]	;y-position of grid
	ydir = [-1,1]			;direction of the ticks
	for i=0,1 do begin		;loop on the 2 channels
		w = [wmin(i),wmax(i)]
		c = coef(*,i)
		x = c(0) + c(1)*w + c(2)*w*w
		x = x/xbin
		y = ypos(i)
		plots,x,[y,y],/dev,thick=2
		for ww=w(0),w(1),5 do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/xbin
			plots,[x,x],[0,ydir(i)*10]+y,/dev,thick=2
			if ydir(i) eq -1 then yoff=5 else yoff=-15
			xyouts,x,y+yoff,strtrim(ww,2),align=0.5,/dev, $
					charsize=1.5
		end
		for ww=w(0),w(1) do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/xbin
			plots,[x,x],[0,ydir(i)*6]+y,/dev,thick=2
		end
	end
;
; plot on little window
;
	wset,little_id
	ypos = [124,4]
	for i=0,1 do begin		;loop on the 2 channels
		w = [wmin(i),wmax(i)]
		c = coef(*,i)
		x = c(0) + c(1)*w + c(2)*w*w
		x = x/16
		y = ypos(i)
		plots,x,[y,y],/dev,thick=2
		if ydir(i) eq -1 then yoff=-15 else yoff=8
		xyouts,x(0),y+yoff,strtrim(w(0),2),align=0.5,/dev
		xyouts,x(1),y+yoff,strtrim(w(1),2),align=0.5,/dev
		for ww=w(0),w(1),10 do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/16
			plots,[x,x],[0,ydir(i)*5]+y,/dev,thick=2
		end
	end
	xyouts,10,30,'SiC '+detector,/dev,charsize=1.5
	xyouts,10,80,'LiF '+detector,/dev,charsize=1.5
	
return
end	

; ================================================================ XSECTION_*
;
; Routines for Cross section plots
;
pro xsection_corners,x1,y1,x2,y2,width,xcorners,ycorners,dist
;
; Compute corners for cross section plot from (x1,y1) to (x2,y2)
; with a width of WIDTH
;
	dist = sqrt(float(x2-x1)^2+float(y2-y1)^2)
	theta = atan(y2-y1,x2-x1)
	if (x2 eq x1) and (y2 eq y1) then x2 = x1+1
	
	dx = (width/2.0)*sin(theta)
	dy = (width/2.0)*cos(theta)
	xcorners = [x1+dx,x2+dx,x2-dx,x1-dx]
	ycorners = [y1-dy,y2-dy,y2+dy,y1+dy]
	return
	end
	
pro xsection_compute,image,xcorners,ycorners,dist,width,xsection
;
; routine to compute cross-section using four corners of box computed by
; xsection_corners
;
	x0 = [0,dist,dist,0]
	y0 = [0,0,width,width]
	polywarp,xcorners,ycorners,x0,y0,1,kx,ky
	pic = poly_2d(image,kx,ky,1,round(dist+0.5)>2,width)
	if width eq 1 then xsection = pic else xsection = total(pic,2)
return
end

;============================================================  FUSE_SCAN_PS
;
; Routine to generate postscript output files
;
pro fuse_scan_ps,color=color,reversed=reversed

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze

	if n_elements(color) eq 0 then color=0
;
; get output file
;
	file = dialog_pickfile(file='idl.ps',filter='*.ps',/write)
	if file eq '' then return	;no file selected
;
; get images to be displayed
;
	wset,little_id & pic1 = tvrd()
	wset,zoom_id & pic2 = tvrd()
	wset,big_id
	widget_control,big_window,get_draw_view=v
	pic3 = tvrd(v(0),v(1),745<(ns-v(0)),515<(nl-v(1)))
;
; rescale to 0 to 255
;
	nc = (!d.n_colors-1) < 255
	scale = 255/float(nc)
	pic1 = byte(pic1*scale+0.5)
	pic2 = byte(pic2*scale+0.5)
	pic3 = byte(pic3*scale+0.5)
;
; reverse
;
	if keyword_set(reversed) then begin
		pic1 = 255b-pic1
		pic2 = 255b-pic2
		pic3 = 255b-pic3
	end
;
; get color table
;
	if color then tvlct,r,g,b,/get
;
; set up postscript file
;
	orig_device = !d.name
	set_plot,'ps'
	xsize = 10.0
	ysize = 7.5
	device,/land,xsize=xsize,ysize=ysize,xoff=0.5,yoff=10.5,color=color, $
		file=file,bits=8,/inches
	!p.font = 0
;
; load color table 
;
	if color then tvlct,frebin(r,256),frebin(g,256),frebin(b,256)
;
; display images
;
	tv,pic1,0,0,xsize=10,ysize=1.25,/inches
	tv,pic2,7,4,xsize=3,ysize=3,/inches
	s = size(pic3) & nx = float(s(1)) & ny = float(s(2))
	rat = nx/ny
	if rat gt 745.0/515.0 then begin
		xsize3 = 6.94
		ysize3 = 6.94/rat
	   end else begin
	   	ysize3 = 4.8
		xsize3 = 4.8*rat
	end
	yoff3 = 7.0-ysize3
	tv,pic3,0,yoff3,xsize=xsize3,ysize=size3,/inches

;
; Display color bar
;
	bar = bindgen(256,2)
	if keyword_set(reversed) then bar = reverse(bar)
	tv,bar,1.25,1.45,xsize=4.25,ysize=0.3,/inches
;
; write image scaling information
;
	widget_control,min_field,get_value=imin
	widget_control,max_field,get_value=imax
	case scale_type of
		'Linear': title = 'Linear Display'
		'Log': title = 'Logarithmic Display'
		'Sqrt': title = 'Square Root Display'
		'Hist. Eq.': title = 'Histogram Equalized Display'
	end
	xyouts,3.375/xsize,1.85/ysize,title,/norm,align=0.5
	xyouts,1.2/xsize,1.55/ysize,strtrim(imin,2),align=1.0,/norm
	xyouts,5.6/xsize,1.55/ysize,strtrim(imax,2),align=0.0,/norm
;
; draw location of pic2 in pic3
;
	x1 = xoffzoom
	y1 = yoffzoom
	x2 = x1 + 256/zoom_factor
	y2 = y1 + 256/zoom_factor
	if (x1 ge v(0)) and (y1 ge v(1)) and (x2 le (v(0)+nx-1)) $
	   and (y2 le (v(1)+ny-1)) then begin
	   	x1 = (x1 - v(0))/nx*xsize3/xsize
		x2 = (x2 - v(0))/nx*xsize3/xsize
		y1 = ((y1 - v(1))/ny*ysize3+yoff3)/ysize
		y2 = ((y2 - v(1))/ny*ysize3+yoff3)/ysize
		plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
		plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2, $
			color=255,line=2
	endif
;
; draw location of pic3 in pic1
;
	x1 = v(0)/float(ns)
	x2 = (v(0) + nx - 1)/float(ns)
	y1 = v(1)/float(nl)*1.25/ysize
	y2 = (v(1) + ny - 1)/float(nl)*1.25/ysize
	plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
	plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2,color=255,line=2
;
; plot boxes around images
;
	x = [0,1,1,0,0]
	y = [0,0,1,1,0]
	plots,x*10/xsize,y*1.25/ysize,/norm,thick=2
	plots,x*xsize3/xsize,(y*ysize3+yoff3)/ysize,/norm,thick=2
	plots,(x*3+7)/xsize,(y*3+4)/ysize,/norm,thick=2
	plots,(x*4.25+1.25)/xsize,(y*0.3+1.45)/ysize,/norm,thick=2
;
; write position information
;
	widget_control,big_position,get_value=v
	xyouts,0.01,(yoff3-0.15)/ysize,v(0),/norm,charsize=0.8
	widget_control,zoom_position,get_value=v
	xyouts,7.1/xsize,3.85/ysize,v(0)+'  zoom = '+strtrim(zoom_factor,2) $
				,/norm,charsize=0.8
;
; write header information
;
	xpos = 7/xsize
	ypos = 3.7/ysize
	yoff = 0.2/ysize
	fdecomp,filename,disk,dir,name,ext
	xyouts,5.0/xsize,7.1/ysize,name+'.'+ext,/norm,align=0.5
	xyouts,xpos,ypos-yoff,/norm, $
		'Targname = '+strtrim(sxpar(header,'targname'),2)
	xyouts,xpos,ypos-yoff*2,/norm,strtrim(sxpar(header,'dateobs'),2) + $
			' '+ strtrim(sxpar(header,'timeobs'),2)
	xyouts,xpos,ypos-yoff*3,/norm, $
			'Detector = '+strtrim(sxpar(header,'detector'),2)
	xyouts,xpos,ypos-yoff*4,/norm, $
			'Aperture = '+strtrim(sxpar(header,'aperture'),2)
	device,/close
	set_plot,orig_device
	if color then tvlct,r,g,b
return
end
	

;
;============================================================ FUSE_SCAN_CONVERT
;
; Routine to convert x,y coordinates from screen to data and vice versa
;
pro fuse_scan_convert,xin,yin,window,xout,yout,to_screen=to_screen
;
; Inputs: xin, yin, window
;	window = 'big','little', or 'zoom'
; Outputs: xout, yout
; Keyword: /to_screen - if supplied coordinates are converted from
;			data to screen coord, otherwise conversion is
;			for screen to data.
;
common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
;
; conversion from screen to data
;
	if keyword_set(to_screen) then begin

		case window of
			'BIG': begin
				xout = xin
				yout = yin
				end
			'LITTLE': begin
				xfactor = ns/1024.
				yfactor = nl/128.
				xout = fix(xin/xfactor)
				yout = fix(yin/yfactor)
				end
			'ZOOM': begin
				xout = (xin - xoffzoom)*zoom_factor + $
							zoom_factor/2
				yout = (yin - yoffzoom)*zoom_factor + $
							zoom_factor/2
				end
		end
	    end else begin
;
; conversion for screen to data
;
		case window of
			'BIG': begin
				xout = xin
				yout = yin
				end
			'LITTLE': begin
				xfactor = ns/1024.
				yfactor = nl/128.
				xout = fix(xin*xfactor+xfactor/2)
				yout = fix(yin*yfactor+yfactor/2)
				end
			'ZOOM': begin
				xout = long(xin/zoom_factor) + xoffzoom
				yout = long(yin/zoom_factor) + yoffzoom
				end
		end
		xout = xout>0<(ns-1)
		yout = yout>0<(nl-1)
	end
	return
end

; ================================================================== FUSE_SCAN
;
; Main Routine
;
pro fuse_scan,image,h

common fuse_scan_common,message,orig,header,ns,nl,omin,omax,filename,flist, $
	big_window,big_id, state,xregion,yregion,nregion,big_position, $
	zoom_id,zoom,zoom_factor,xoffzoom,yoffzoom,zoom_position, $
	little_image, little_id, little_down, little_ns, little_nl, $
	draw_x, draw_y, ndraw, overlay_on, rsave,gsave,bsave, $
	scale_type, min_field,max_field, x_field, y_field, val_field, freeze
common fuse_timetag_data,xttag,yttag,timetag,gtime1,gtime2
	
	if n_elements(image) eq 0 then image = fltarr(750,512)
	if n_elements(h) gt 0 then header=h else header=['END      ']
	if xregistered('fuse_scan') then begin
		fuse_scan_display,image
		xttag = 0
		yttag = 0
		timetag = 0
		return
	end
		
;
; initialization
;	
	filename = 'fuse_scan'
	s = size(image) & ns = s(1) & nl = s(2)
	zoom_factor = 5
	xoffzoom = ns/2 - 128/zoom_factor
	yoffzoom = ns/2 - 128/zoom_factor
	little_down = 0
	scale_type = 'Linear'
	state = [0,0,-1,-1]
	xregion = intarr(20000)
	yregion = intarr(20000)
	nregion = 0
	overlay_on = 0

; create widget layout
;
	base = widget_base(/col,group=0,uvalue='MAIN',/tracking_events)
	widget_control,base
;
; Button Bar
;
	basebar = widget_base(base,/row)
	desc = ['1\FILE','0\Read Raw File(s)','0\Read Image Extension', $
		'1\PS output','0\B/W', $
		'0\B/W Reversed','2\Color','2\EXIT']
	button = cw_pdmenu(basebar,desc,uvalue='FILE')
	button = widget_button(basebar,uvalue='COLORS',value='Colors')
	desc = ['1\Contrast','0\Linear','0\Sqrt','0\Log','2\Hist. Eq.']
	button = cw_pdmenu(basebar,desc,uvalue='CONTRAST',/Return_full_name)
	desc = ['1\IMAGE','0\Set Region','0\Histogram', $
		'2\Display Header']
	button = cw_pdmenu(basebar,desc,uvalue='IMAGE',/return_full_name)
	desc = ['1\Stats','0\Box','0\Draw Region','0\Whole Image', $
		'0\Zoomed Image','0\PHA:Whole image','2\PHA: Box']
	button = cw_pdmenu(basebar,desc,uvalue='STATS')
	desc = ['1\Plot','0\Row','0\Column','0\Row Sum','0\Column Sum', $
		'0\LineFind Row Sum', $
		'1\X-Section','0\1 pixel wide','0\3','0\5','0\7','0\9', $
		'0\11','0\15','0\25','0\35','0\55','2\75']
	button = cw_pdmenu(basebar,desc,uvalue='PLOT')
	
	desc = ['1\Overlay','0\Wavelength Scale','2\Clear Overlay']
	button = cw_pdmenu(basebar,desc,uvalue='OVERLAY',/return_full_name)
	button = widget_button(basebar,uvalue='TTAG',value='Time-Tag')
	desc = ['1\Zoom Image','1\GAUSSFIT','0\Emission','2\Absorption', $
		'0\Surface Plot','2\Contour Plot']
	button = cw_pdmenu(basebar,desc,uvalue='ZOOM IMAGE',/return_full_name)
	desc = ['1\Zoom Factor','0\ 1','0\ 2','0\ 3','0\ 4','0\ 5','0\ 6', $
		'0\ 7','0\ 8','0\ 9','0\10','0\16','2\32']
	button = cw_pdmenu(basebar,desc,uvalue='ZOOM',/return_full_name)

	basewindows = widget_base(base,/row)
	basebig = widget_base(basewindows,/col)
	basex = widget_base(basebig,/row)
	big_position = widget_label(basex,value='XXXXX:XXXX XXXXX:XXXX')
	message = widget_label(basex,xsize=500,value = '    ')
	big_window = widget_draw(basebig,uvalue='BIG_WINDOW', $
			xsize=750,ysize=512,x_scroll_size=700, $
			y_scroll_size=542,/button_events,/motion, $
			/viewport_events)
	base2 = widget_base(basewindows,/col)
	zoom_window = widget_draw(base2,uvalue='ZOOM_WINDOW',retain=2, $
			xsize=256,ysize=256,/motion,/button_events)
	zoom_position = widget_label(base2,value='XXXXX:XXXX XXXXX:XXXX')

	base2a = widget_base(base2,/col,/frame)

	freeze = widget_button(base2a,uvalue='FREEZE',value='Freeze Min/Max')
        min_field = cw_field(base2a,/row,uvalue='MIN_FIELD',value=omin, $
                title='Min: ',xsize=13,/return_events,/float)
        max_field = cw_field(base2a,/row,uvalue='MAX_FIELD',value=omax, $
                title='Max: ',xsize=13,/return_events,/float)
	button = widget_button(base2a,uvalue='RESET',value='Reset Min/Max')

	base2b = widget_base(base2,/col,/frame)
	x_field = cw_field(base2b,/row,uvalue='X_FIELD',value=-1, $
                title='X:   ',xsize=13,/return_events,/long)
	y_field = cw_field(base2b,/row,uvalue='Y_FIELD',value=-1, $
                title='Y:   ',xsize=13,/return_events,/long)
	val_field = cw_field(base2b,/row,uvalue='VAL_FIELD',value=-1, $
                title='Val: ',xsize=13,/return_events,/float)
		
	little_window = widget_draw(base,uvalue='LITTLE_WINDOW',retain=2, $
			xsize=1024,ysize=128,/button_events,/motion)

;
; create widget
;
	widget_control,base,/realize
        widget_control,big_window,get_value=big_id
        widget_control,little_window,get_value=little_id
        widget_control,zoom_window,get_value=zoom_id
	fuse_scan_display,image
	tvlct,rsave,gsave,bsave,/get

	xmanager,'fuse_scan',base,/no_block
	return
end
