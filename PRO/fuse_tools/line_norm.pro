;+
;				line_norm
;
; Widget to interactively normalize a spectrum
;
; CALLING SEQUENCE:
;
;	line_norm,x,y,ynorm,norm,xnodes,ynodes
;
; INPUTS
;	x - input x vector
;	y - input y vector
;
; OUTPUTS:
;	ynorm - normalized spectrum
;	norm - continuum used for normalization 
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
; OPTIONAL INPUT/OUTPUTS
;	xnodes - xnode positions of the spline
;	ynodes - ynode positions of the spline
;
; INTERACTIVE INPUTS:
;	In addition to the widget parameters controlled by buttons and
;	text field inputs:
;	1) Use the left mouse button to move/drag a control point
;	2) Use the right button to add a new control point
;	3) Use center button to zoom.  (Select one corner, hold down and
;		drag to opposite corner of region to be zoomed)
;
;
; HISTORY:
;	version 1  D. Lindler  Sept 1999
;	Mar 2001, modified to work if wavelength vector is in descending
;		order
;-
;----------------------------------------------------------------------------  

;====================================================== line_norm_EVENT
;
; Main event driver for line_edit
;
pro line_norm_event,event
	common line_norm_common,info,xarray,yarray,rsave,gsave,bsave,xn,yn,xnorm,fnorm
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
	'EXIT': begin
		wdelete,info.pixid
		wdelete,info.pixid2
		tvlct,rsave,gsave,bsave
		widget_control,event.top,/destroy
		return
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
			line_norm_plot,info,xarray,yarray,xn,yn,/ps
			oplot,xnorm,fnorm,color=3,thick=2
			oplot,xn,yn,color=2,symsize=1,psym=2,thick=2
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
			fn = spline(xn,yn,xarray,info.tension)
			for i=0,info.ns-1 do $
				printf,unit,xarray(i),yarray(i),fn(i), $
						yarray(i)/fn(i)
			free_lun,unit
			end
		'Write.FITS Table': begin
			file = dialog_pickfile(file='idl.fits', $
						filter='*.fits',/write)
			if file eq '' then return	;no file selected
			sxaddpar,h,'XTITLE',info.xtitle
			sxaddpar,h,'YTITLE',info.ytitle
			sxaddpar,h,'TITLE',info.title
			fn = spline(xn,yn,xarray,info.tension)
			a = {x:xarray,y:yarray,norm:fn,ynorm:yarray/fn}
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
	'UNZOOM': begin
		widget_control,info.xmin_base,set_value = min(xarray)
		widget_control,info.xmax_base,set_value = max(xarray)
		widget_control,info.ymin_base,set_value = min(yarray)
		widget_control,info.ymax_base,set_value = max(yarray)
		line_norm_plot,info,xarray,yarray,xn,yn
		widget_control,info.xmin_base,get_v=x1
		widget_control,info.xmax_base,get_v=x2
		xnorm = (x2-x1)/1000.0*findgen(1001)+x1
		fnorm = spline(xn,yn,xnorm,info.tension)
		end
	'RANGE': begin
		widget_control,info.xmin_base,get_v=x1
		widget_control,info.xmax_base,get_v=x2
		xnorm = (x2-x1)/1000.0*findgen(1001)+x1
		fnorm = spline(xn,yn,xnorm,info.tension)
		line_norm_plot,info,xarray,yarray,xn,yn
		end
	'XLOG': begin
		info.xlog = 1 - info.xlog
		if info.xlog eq 1 then v='X Linear' else v='X Log'
		widget_control,info.xbutton,set_value=v
		line_norm_plot,info,xarray,yarray,xn,yn
		end
 	'YLOG': begin
		info.ylog = 1 - info.ylog
		if info.ylog eq 1 then v='Y Linear' else v='Y Log'
		widget_control,info.ybutton,set_value=v
		line_norm_plot,info,xarray,yarray,xn,yn
		end
	'TENSION': begin
		info.tension = event.value
		fnorm = spline(xn,yn,xnorm,info.tension)
		wset,info.plot1_id
		device,copy=[0,0,950,450,0,0,info.pixid]
		end
	'ADD_NODE': Begin
		info.state = 'ADD'		
		widget_control,info.message,set_v= $
				'Position at node and'+ $
				' push left mouse button'
		end
	'DELETE_NODE': Begin
		widget_control,info.message,set_v= $
				'Position at node and'+ $
				' push left mouse button'
		info.state = 'DELETE'
		end
		
	'PLOT1': begin
		xd = event.x	;device coordinates
		yd = event.y
		v = convert_coord(xd,yd,/dev,/to_data) ; data coordinates
		wset,info.plot1_id
		device,copy=[0,0,950,450,0,0,info.pixid2]
		if (info.state eq 'X/Y') then begin
		    plots,[xd,xd],[0,450],color=1,/dev
		    plots,[0,950],[yd,yd],color=1,/dev
		    widget_control,info.message,set_value= $
			'X = '+strtrim(v(0),2)+'    Y = '+strtrim(v(1),2)
		    if event.press eq 2 then begin
				info.x1 = xd
				info.y1 = yd
				info.state = 'ZOOM2'
				return
		    endif
		    if event.press eq 1 then info.state = 'DRAG'
		    if (info.state eq 'X/Y') and (event.press ne 4) then return
		end
;
; Add Node
;
		if (event.press eq 4) or $
		   ((event.press eq 1) and (info.state eq 'ADD')) then begin
			xn = [xn,v(0)]
			yn = [yn,v(1)]
			sub = sort(xn)
			xn = xn(sub)
			yn = yn(sub)
			fnorm = spline(xn,yn,xnorm,info.tension)
			info.state = 'X/Y'
			widget_control,info.message,set_value=' '
			goto,replot_spline
		end
;
; Delete Node
;
		if (event.press eq 1) and (info.state eq 'DELETE') then begin
			diff = abs(xn-v(0))
			good = where(diff ne min(diff),ngood)
			if ngood lt 3 then begin
			    r = dialog_message('You need at least 3 nodes', $
			    		/error,dialog_parent=event.top)
			    end else begin
				xn = xn(good)
				yn = yn(good)
			end
			fnorm = spline(xn,yn,xnorm,info.tension)
			info.state = 'X/Y' 
			widget_control,info.message,set_value=' '
			goto,replot_spline
		end			
;
; Drag Node
;
		if (info.state eq 'DRAG') then begin
			diff = abs(xn-v(0))
			good = where(diff eq min(diff))
			info.node = good(0)
			xn(info.node) = v(0)
			yn(info.node) = v(1)
			fnorm = spline(xn,yn,xnorm,info.tension)
			if (event.release gt 0) then info.state = 'X/Y' 
			goto,replot_spline
		end
;
; First Zoom Corner
;
		if (info.state eq 'ZOOM1') and  $
		   (event.press eq 1) then begin
		        plots,[xd,xd],[0,450],color=1,/dev
		    	plots,[0,950],[yd,yd],color=1,/dev
		   	info.x1 = xd
			info.y1 = yd
			widget_control,info.message,set_v= $
				'Position at second corner and'+ $
				' push left mouse button'
			info.state = 'ZOOM2'
			return
		endif
;
; Second Zoom Corner
;
		if (info.state eq 'ZOOM2') then begin
		    x = [info.x1,xd]
		    y = [info.y1,yd]
		    plots,[x(0),x(1),x(1),x(0),x(0)], $
			  [y(0),y(0),y(1),y(1),y(0)],/dev,color=1
		    
		    if (event.release eq 2) or (event.press eq 1) then begin
			v = convert_coord(x,y,/dev,/to_data)
			x = v(0,*)
			y = v(1,*)		    
		    	widget_control,info.xmin_base,set_value = min(x)
			widget_control,info.xmax_base,set_value = max(x)
			widget_control,info.ymin_base,set_value = min(y)
			widget_control,info.ymax_base,set_value = max(y)
			line_norm_plot,info,xarray,yarray,xn,yn
			widget_control,info.xmin_base,get_v=x1
			widget_control,info.xmax_base,get_v=x2
			xnorm = (x2-x1)/1000.0*findgen(1001)+x1
			fnorm = spline(xn,yn,xnorm,info.tension)
			goto,replot_spline
		    end else return
		end	    
			
		end
	else:
	endcase
replot_spline:
	wset,info.plot1_id
	device,copy=[0,0,950,450,0,0,info.pixid]
	oplot,xnorm,fnorm,color=3,thick=2
	oplot,xn,yn,color=2,symsize=1,psym=2,thick=2
	wset,info.pixid2
	device,copy=[0,0,950,450,0,0,info.plot1_id]
	wset,info.plot1_id
	return
	end

;============================================================ LINE_NORM_PLOT
;
; Routine to generate the plot
;
pro line_norm_plot,info,x,y,xn,yn,ps=ps

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
	oplot,x,y,color=1,symsize=symsize,psym=-4
	if not keyword_set(ps) then begin
		wset,info.pixid
		device,copy=[0,0,950,450,0,0,info.plot1_id]
		wset,info.pixid2
		device,copy=[0,0,950,450,0,0,info.plot1_id]
		wset,info.plot1_id
	end
	info.state = 'X/Y'
return
end


;==================================================================== LINE_EDIT
;
; Plot widget main routine	
	
pro line_norm,xin,yin,ynorm,norm,xnodes,ynodes, $
	title=title,xtitle=xtitle,ytitle=ytitle, $
	group=group,xrange=xrange,yrange=yrange, $
	min_val=min_val,max_val=max_val, modal=modal
;
; CALLING SEQUENCE:
;	
	if n_params(0) lt 1 then begin
		print,'CALLING SEQUENCE: line_norm,x,y,ynorm,norm
		print,'KEYWORD INPUTS: title,xtitle,ytitle,xrange,yrange
		return
	end
	common line_norm_common,info,xarray,yarray,rsave,gsave,bsave,xn,yn,xnorm,fnorm
	if n_elements(title) eq 0 then title=''
	if n_elements(xtitle) eq 0 then xtitle=''
	if n_elements(ytitle) eq 0 then ytitle=''
	if n_elements(min_val) eq 0 then min_val=-1e37
	if n_elements(max_val) eq 0 then max_val = 1e37

;
; initilization	
;
	ns = n_elements(xin)
	xarray = xin
	yarray = yin
	tvlct,r,g,b,/get
	rsave = r
	gsave = g
	bsave = b
	r(0) = [255,0,255, 0]
	g(0) = [255,0,  0, 255]
	b(0) = [255,0,  0, 0]
	tvlct,r,g,b
	set_xy
	set_viewport 

;;	widget_control,default_font  = $
;;		 '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'
	main = widget_base(/col,group=group,/tracking,uvalue='MAIN', $
			title='Spectrum Normalization Routine',modal=modal)
	menu = widget_base(main,/row,/frame)
	exit = widget_button(menu,value='EXIT',uvalue='EXIT')
	desc = ['1\Write','0\Postscript File','0\ASCII Table','2\FITS Table']
	button = cw_pdmenu(menu,desc,uvalue='WRITE',/return_full_name)
	zoom = widget_button(menu,uvalue='ZOOM',value='Zoom')
	unzoom = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
	xbutton = widget_button(menu,uvalue='XLOG',value='X Log     ')
	ybutton = widget_button(menu,uvalue='YLOG',value='Y Log     ')
	button = widget_button(menu,value='ADD Node',uvalue='ADD_NODE')
	button = widget_button(menu,value='Delete Node',uvalue='DELETE_NODE')
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
; Tension slider
;
	b = widget_base(main,/row)
	lab = widget_label(b,value='Spline Tension')
	slider = cw_fslider(b,min=0.05,max=20.0,value=1.0,/suppress_val, $
		/drag,uvalue='TENSION',xsize=800,title='')
;
; create two pixmaps
;
	window,xs=950,ys=450,/pixmap,/free
	pixid = !d.window
	window,xs=950,ys=450,/pixmap,/free
	pixid2 = !d.window
;
; save widget info in structure
;
	widget_control,main,/realize
	widget_control,plot1,get_value=plot1_id

	info = {message:message,ns:ns,pixid:pixid,pixid2:pixid2,xtitle:xtitle, $
		ytitle:ytitle,title:title,plot1:plot1, $
		main:main,xmin:min(xin),ymin:min(yin), $
		xmax:max(xin),ymax:max(yin),xlog:0,ylog:0,xmin_base:xmin_base, $
		ymin_base:ymin_base,xmax_base:xmax_base,ymax_base:ymax_base, $
		state:'X/Y',xbutton:xbutton,ybutton:ybutton, $
		x1:0, y1:0, node:0, plot1_id:plot1_id,  $
		min_val:min_val,max_val:max_val,tension:1.0}
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
;
; set initial control points
;
       x1 = min(xarray)
       x2 = max(xarray)
	if n_elements(xnodes) gt 2  then begin
		xn = xnodes
		yn = ynodes
	   end else begin
		dx = (x2-x1)/2.0
		xn = x1 + findgen(3)*dx
		yn = fltarr(3)+median(yarray)
	end
;
; create initial plot
;
	widget_control,info.xmin_base,get_v=x1
	widget_control,info.xmax_base,get_v=x2
	xnorm = (x2-x1)/1000.0*findgen(1001)+x1
	fnorm = spline(xn,yn,xnorm,info.tension)
	line_norm_plot,info,xarray,yarray,xn,yn
	oplot,xnorm,fnorm,color=3,thick=2
	oplot,xn,yn,color=2,symsize=1,psym=2,thick=2
	xmanager,'line_norm',main
	if xarray(1) gt xarray(0) then $
		fnorm = spline(xn,yn,xarray,info.tension) $
		else fnorm = reverse(spline(xn,yn,reverse(xarray),info.tension))
	norm = fnorm
	ynorm = yarray*0
	good = where(norm gt 0,ngood)
	if ngood gt 0 then ynorm(good) = yarray(good)/norm(good)
	info = 0
	xarray = 0
	yarray = 0
	fnorm = 0
	xnodes = xn
	ynodes = yn	
	return
	end
