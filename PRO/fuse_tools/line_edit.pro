;+
;				line_edit
;
; Widget plot edit tool for FUSE_SCAN
;
; CALLING SEQUENCE:
;	line_edit,x,y,mask
;
; INPUTS
;	x - input x vector
;	y - input y vector
;	mask - mask of good data (1=good, 0=bad)
; OPTIONAL INPUT PARAMETERS
;	ptitle - main plot title
;	title - title of the plot or overplot vectors
;	xtitle - title for the xaxis
;	ytitle - title for the yaxis
;	xrange - initial xrange for the plot
;	yrange - initial yrange for the plot
;	group - group id of calling widget
;	modal - modal (set to make modal widget)
;
; INTERACTIVE INPUTS:
;	In addition to the widget parameters controlled by buttons and
;	text field inputs:
;	1) Use the left mouse button to measure a feature location in the
;	   plot.  
;	2) Push the center mouse button and hold it down to define 
;	   two corners of a region to be zoomed.
;
; The large text box may be edited before writing to a file
;
; HISTORY:
;	version 1  D. Lindler  Sept 1999
;-
;----------------------------------------------------------------------------  

;====================================================== LINE_EDIT_EVENT
;
; Main event driver for line_edit
;
pro line_edit_event,event
	common line_edit_common,info,xarray,yarray,m,m1,rsave,gsave,bsave
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
	'EXIT': begin
		xarray = 0
		yarray = 0
		wdelete,info.pixid
		tvlct,rsave,gsave,bsave
		widget_control,event.top,/destroy
		end
	'WRITE': begin
		case event.value of
		'Write.Postscript File': begin
			file = dialog_pickfile(file='idl.ps',filter='*.ps', $
					/write)
			if file eq '' then return	;no file selected
			orig_device = !d.name
			set_plot,'ps'
			device,/land,bits=8,/color,file=file
			!p.font = 0
			set_viewport,0.1,0.9,0.1,0.9
			line_edit_plot,info,xarray,yarray,m,/ps
			device,/close
			set_plot,orig_device
			!p.font = -1
			end
		'Write.ASCII Table': begin
			file = dialog_pickfile(file='idl.txt',filter='*.txt', $
					/write)
			if file eq '' then return	;no file selected
			openw,unit,file,/get_lun
			printf,unit,';'+info.title
			printf,unit,';'+info.ytitle
			printf,unit,';'+info.xtitle
			for i=0,info.ns-1 do $
					printf,unit,xarray(i),yarray(i),m(i)
			free_lun,unit
			end
		'Write.FITS Table': begin
			file = dialog_pickfile(file='idl.fits', $
						filter='*.fits',/write)
			if file eq '' then return	;no file selected
			sxaddpar,h,'XTITLE',info.xtitle
			sxaddpar,h,'YTITLE',info.ytitle
			sxaddpar,h,'TITLE',info.title
			a = {x:xarray,y:yarray,mask:m}
			mwrfits,a,file,h,/create
			end
		endcase
		end
	'MAIN': 
	'ZOOM': begin
		widget_control,info.message,set_v= $
		  'Place Cursor on first corner and push left mouse button'
		info.state = 'ZOOM1'
		end
	'DELETE': begin
		widget_control,info.message,set_v= $
		  'Place Cursor on first corner and push left mouse button'
		info.state = 'DELETE1'
		end
	'KEEP': begin
		widget_control,info.message,set_v= $
		  'Place Cursor on first corner and push left mouse button'
		info.state = 'KEEP1'
		end
	'UNZOOM': begin
		widget_control,info.xmin_base,set_value = min(xarray)
		widget_control,info.xmax_base,set_value = max(xarray)
		widget_control,info.ymin_base,set_value = min(yarray)
		widget_control,info.ymax_base,set_value = max(yarray)
		line_edit_plot,info,xarray,yarray,m
		end
	'RANGE': line_edit_plot,info,xarray,yarray,m
	'XLOG': begin
		info.xlog = 1 - info.xlog
		if info.xlog eq 1 then v='X Linear' else v='X Log'
		widget_control,info.xbutton,set_value=v
		line_edit_plot,info,xarray,yarray,m
		end
 	'YLOG': begin
		info.ylog = 1 - info.ylog
		if info.ylog eq 1 then v='Y Linear' else v='Y Log'
		widget_control,info.ybutton,set_value=v
		line_edit_plot,info,xarray,yarray,m
		end
	'DELETE_ALL': begin
		m1 = m
		m(*) = 0
		line_edit_plot,info,xarray,yarray,m
		end			
	'KEEP_ALL': begin
		m1 = m
		m(*) = 1
		line_edit_plot,info,xarray,yarray,m
		end
	'UNDO': begin
		m = m1
		line_edit_plot,info,xarray,yarray,m
		end
	'PLOT1': begin
		xd = event.x	;device coordinates
		yd = event.y
		wset,info.plot1_id
		device,copy=[0,0,950,450,0,0,info.pixid]
		if (info.state eq 'X/Y') then begin
		    plots,[xd,xd],[0,450],color=1,/dev
		    plots,[0,950],[yd,yd],color=1,/dev
		    v = convert_coord(xd,yd,/dev,/to_data)
		    widget_control,info.message,set_value= $
			'X = '+strtrim(v(0),2)+'    Y = '+strtrim(v(1),2)
		    if event.press eq 2 then begin
				info.x1 = xd
				info.y1 = yd
				info.state = 'ZOOM2'
				return
		    endif
		end
		if (strpos(info.state,'1') gt 0) and  $
		   (event.press eq 1) then begin
		        plots,[xd,xd],[0,450],color=1,/dev
		    	plots,[0,950],[yd,yd],color=1,/dev
		   	info.x1 = xd
			info.y1 = yd
			widget_control,info.message,set_v= $
				'Position at second corner and'+ $
				' push left mouse button'
			if info.state eq 'ZOOM1' then info.state = 'ZOOM2'
			if info.state eq 'DELETE1' then info.state = 'DELETE2'
			if info.state eq 'KEEP1' then info.state = 'KEEP2'
			return
		endif
		if (strpos(info.state,'2') gt 0) then begin
		    x = [info.x1,xd]
		    y = [info.y1,yd]
		    plots,[x(0),x(1),x(1),x(0),x(0)], $
			  [y(0),y(0),y(1),y(1),y(0)],/dev,color=1
		    
		    if (event.release eq 2) or (event.press eq 1) then begin
			v = convert_coord(x,y,/dev,/to_data)
			x = v(0,*)
			y = v(1,*)		    
			if info.state eq 'ZOOM2' then begin
		    	    widget_control,info.xmin_base,set_value = min(x)
			    widget_control,info.xmax_base,set_value = max(x)
			    widget_control,info.ymin_base,set_value = min(y)
			    widget_control,info.ymax_base,set_value = max(y)
			  end else begin
			    inside = where((xarray gt min(x)) and $ 
			    		   (xarray lt max(x)) and $
					   (yarray gt min(y)) and $
					   (yarray lt max(y)), np)
			    m1 = m
			    if np gt 0 then begin
			        if info.state eq 'DELETE2' then m(inside) = 0
			        if info.state eq 'KEEP2' then m(inside) = 1
			    end
			end
			line_edit_plot,info,xarray,yarray,m
			return
		    end
		end	    
			
		end
	else:
	endcase
	return
	end

;============================================================ LINE_EDIT_PLOT
;
; Routine to generate the plot
;
pro line_edit_plot,info,x,y,mask,ps=ps

	if not keyword_set(ps) then begin
		wset,info.plot1_id
		set_viewport,0.1,0.9,0.1,0.9
	end
	widget_control,info.xmin_base,get_value=xmin
	widget_control,info.xmax_base,get_value=xmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax
	plot,[xmin,xmax],[ymin,ymax],/nodata,ytitle=info.ytitle, $
		xtitle=info.xtitle,title=info.title,xstyle=1,ystyle=1, $
		xlog=info.xlog,ylog=info.ylog,color=1,xrange=[xmin,xmax], $
		yrange=[ymin,ymax]
	if info.ylog ne 1 then oplot,!x.crange,[0,0],line=2,color=1
	good = where((x ge xmin) and (x le xmax),ngood)
	symsize=1.6
	if ngood gt 100 then symsize=0.8
	if ngood gt 200 then symsize=0.5
	if ngood gt 300 then symsize=0.2
	if ngood gt 500 then symsize=0.01
;
; loop on groups of good and bad data regions
;
	ns = n_elements(x)
	i1 = 0L
	ipos = 1L
	while ipos lt ns do begin
		if mask(ipos) ne mask(i1) then begin
			i2 = ipos-1
			if mask(i1) eq 0 then color=2 else color=1
			oplot,x(i1:i2),y(i1:i2),color=color,psym=-4, $
				symsize=symsize
			i1 = i2+1
		end
		ipos = ipos + 1
	end
	i2 = ns-1
	if mask(i1) eq 0 then color=2 else color=1
	oplot,x(i1:i2),y(i1:i2),color=color,psym=-4,symsize=symsize	
	if not keyword_set(ps) then begin
		wset,info.pixid
		device,copy=[0,0,950,450,0,0,info.plot1_id]
	end
	info.state = 'X/Y'
return
end


;==================================================================== LINE_EDIT
;
; Plot widget main routine	
	
pro line_edit,xin,yin,mask,title=title,xtitle=xtitle,ytitle=ytitle, $
	group=group,xrange=xrange,yrange=yrange, $
	min_val=min_val,max_val=max_val, modal=modal

;
; calling sequence
;
	if n_params(0) lt 1 then begin
		print,'CALLING SEQUENCE:  line_edit,x,y,mask
		print,'Keyword INPUTS: title,xtitle,ytitle,xrange,yrange
		return
	end

	common line_edit_common,info,xarray,yarray,m,m1,rsave,gsave,bsave
	if n_elements(title) eq 0 then title=''
	if n_elements(xtitle) eq 0 then xtitle=''
	if n_elements(ytitle) eq 0 then ytitle=''
	if n_elements(mask) eq 0 then mask=replicate(1b,n_elements(xin))
	if n_elements(min_val) eq 0 then min_val=-1e37
	if n_elements(max_val) eq 0 then max_val = 1e37

;
; initilization	
;
	ns = n_elements(xin)
	xarray = xin
	yarray = yin
	m = mask
	tvlct,r,g,b,/get
	rsave = r
	gsave = g
	bsave = b
	r(0) = [255,0,255]
	g(0) = [255,0,  0]
	b(0) = [255,0,  0]
	tvlct,r,g,b 
	set_viewport
	
	main = widget_base(/col,group=group,/tracking,uvalue='MAIN', $
			title='PLOT EDIT ROUTINE',modal=modal)
	menu = widget_base(main,/row,/frame)
	exit = widget_button(menu,value='EXIT',uvalue='EXIT')
	desc = ['1\Write','0\Postscript File','0\ASCII Table','2\FITS Table']
	button = cw_pdmenu(menu,desc,uvalue='WRITE',/return_full_name)
	zoom = widget_button(menu,uvalue='ZOOM',value='Zoom')
	unzoom = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
	xbutton = widget_button(menu,uvalue='XLOG',value='X Log     ')
	ybutton = widget_button(menu,uvalue='YLOG',value='Y Log     ')
	button = widget_button(menu,uvalue='DELETE_ALL',value='Delete All')
	button = widget_button(menu,uvalue='DELETE',value='Delete Region')
	button = widget_button(menu,uvalue='KEEP_ALL',value='Keep All')
	button = widget_button(menu,uvalue='KEEP',value='Keep Region')
	button = widget_button(menu,uvalue='UNDO',value='UNDO')
	message = widget_text(main,xsize=80,value=' ')
;
; draw window
;
	plot1 = widget_draw(main,uvalue='PLOT1',retain=2, $
				xsize=950,ysize=450,/button_events,/motion)

	basex = widget_base(main,/row,/frame)	
        xmin_base = cw_field(basex,/row,uvalue='RANGE',value=min(xin), $
                title='X Min: ',xsize=13,/return_events,/float)
        xmax_base = cw_field(basex,/row,uvalue='RANGE',value=max(xin), $
                title='X Max: ',xsize=13,/return_events,/float)
        ymin_base = cw_field(basex,/row,uvalue='RANGE',value=min(yin), $
                title='Y Min: ',xsize=13,/return_events,/float)
        ymax_base = cw_field(basex,/row,uvalue='RANGE',value=max(yin), $
                title='Y Max: ',xsize=13,/return_events,/float)
;
; create pixmap
;
	window,xs=950,ys=450,/pixmap,/free
	pixid = !d.window
;
; save widget info in structure
;
	widget_control,main,/realize
	widget_control,plot1,get_value=plot1_id

	info = {message:message,ns:ns,pixid:pixid,xtitle:xtitle, $
		ytitle:ytitle,title:title,plot1:plot1, $
		main:main,xmin:min(xin),ymin:min(yin), $
		xmax:max(xin),ymax:max(yin),xlog:0,ylog:0,xmin_base:xmin_base, $
		ymin_base:ymin_base,xmax_base:xmax_base,ymax_base:ymax_base, $
		state:'X/Y',xbutton:xbutton,ybutton:ybutton, $
		x1:0, y1:0, plot1_id:plot1_id, $
		min_val:min_val,max_val:max_val}
;
; set initial range
;	
	if n_elements(xrange) gt 0 then begin
		widget_control,info.xmin_base,set_v=xrange(0)
		widget_control,info.xmax_base,set_v=xrange(1)
	end
	if n_elements(yrange) gt 0 then begin
		widget_control,info.ymin_base,set_v=yrange(0)
		widget_control,info.ymax_base,set_v=yrange(1)
	end
	
	line_edit_plot,info,xarray,yarray,m
	xmanager,'line_edit',main
	mask = m
	m = 0
	return
	end
