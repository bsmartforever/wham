; CURSOR processing
; 
pro fuse_analysis_cursor,event,uvalue
@fuse_analysis_common
;
; decode event
;
	x = event.x
	y = event.y
	press = event.press
	release = event.release
	windows = ['PLOT_WINDOW','PIC1','PIC2','PIC3','PIC4','ZOOM_WINDOW']
	iwindow = where(windows eq uvalue)
	iwindow = iwindow(0)
	ids = [widget.plot_id,widget.pic1_id,widget.pic2_id,widget.pic3_id, $
		widget.pic4_id,widget.zoom_id]
	nx = [replicate(widget.plot_nx,5),widget.zoom_nx]
	ny = [widget.plot_ny,replicate(widget.plot_ny,4),widget.zoom_ny]
;
; get cursor value
;
	fval = 0.0
	fuse_analysis_curval,x,y,uvalue,w,fval,xim,yim,imname,text
	case state.type of 
;
; X/Y STATE ===============================================
;

	'X/Y': begin
	       	widget_control,widget.text_base,set_value=text
;
; Zoom Image
;
	   	if (event.press eq 1) and (iwindow lt 5) and $
		   (iwindow gt 0) then fuse_analysis_zoom,xim,yim,imname
;
; draw cursor
;
		for i=0,4 do $
			    fuse_analysis_overlay,ids(i),[x,x],[0,ny(i)],state
	       	if iwindow eq 5 then begin
	            fuse_analysis_overlay,ids(5),[0,0]+event.x,[0,ny(5)],state
	       	end
	       	fuse_analysis_overlay,ids(iwindow), $
		    			[0,nx(iwindow)],[y,y],state
		end
;
; ZOOM STATE ===============================================
;
	'ZOOM': begin
		if iwindow ne 0 then return
;
; button not pressed, just plot overlay
;
		if (event.press eq 0) then begin
		    if state.save(0) eq 0 then begin 
			fuse_analysis_overlay,ids(0),[x,x],[0,ny(0)],state
			fuse_analysis_overlay,ids(0),[0,nx(0)],[y,y],state
		      end else begin
			x1 = round(state.save(1))
			y1 = round(state.save(2))
			fuse_analysis_overlay,ids(0), $
		    			[x1,x,x,x1,x1],[y1,y1,y,y,y1],state
		    end
		    return
		end
;
; button for first corner pressed
;
		if (state.save(0) eq 0) then begin
			state.save(0:4) = [1,x,y,w,fval]
			widget_control,widget.text_base,set_value= $
			     'Push Mouse Button at Second Corner'
			return
		end
;
; button pressed for second corner
;			
		wmin = state.save(3)<w
		wmax = state.save(3)>w
		if wmin ne wmax then begin
			widget_control,widget.wmin_field,set_value=wmin
			widget_control,widget.wmax_field,set_value=wmax
		endif
		fmin = state.save(4)<fval
		fmax = state.save(4)>fval
		if fmin ne fmax then begin
			widget_control,widget.plot_select,get_value=isel
			plotpar(isel).ymin = fmin
			plotpar(isel).ymax = fmax
		end
		fuse_analysis_plot,/keep_yrange
		state.type = 'X/Y'
		widget_control,widget.text_base,set_value=' '
		end
;
; ZOOMX STATE ===============================================
;
	'ZOOMX': begin
		if iwindow gt 4 then return
;
; button not pressed, just plot overlay
;
		if (event.press eq 0) then begin
		    if state.save(0) eq 0 then begin 
		        for i=0,4 do $
			  fuse_analysis_overlay,ids(i),[x,x],[0,ny(i)],state
		      end else begin
			x1 = round(state.save(1))
			for i=0,4 do $
			   fuse_analysis_overlay,ids(i),[x1,x1],[0,ny(i)],state
			for i=0,4 do $
			   fuse_analysis_overlay,ids(i),[x,x],[0,ny(i)],state
		    end
		    return
		end
;
; button for first wavelength pressed
;
		if (state.save(0) eq 0) then begin
			state.save(0:4) = [1,x,y,w,fval]
			widget_control,widget.text_base,set_value= $
			     'Push Mouse Button at maximum wavelength'
			return
		end
;
; button pressed for second wavelength
;			
		wmin = state.save(3)<w
		wmax = state.save(3)>w
		if wmin ne wmax then begin
			widget_control,widget.wmin_field,set_value=wmin
			widget_control,widget.wmax_field,set_value=wmax
		endif
		fuse_analysis_plot,/keep_yrange
		state.type = 'X/Y'
		widget_control,widget.text_base,set_value=' '
		end
;
; ZOOMY STATE ===============================================
;
	'ZOOMY': begin
		if iwindow ne 0 then return
;
; button not pressed, just plot overlay
;
		if (event.press eq 0) then begin
		    if state.save(0) eq 0 then begin 
			fuse_analysis_overlay,ids(0),[0,nx(0)],[y,y],state
		      end else begin
			y1 = round(state.save(2))
			fuse_analysis_overlay,ids(0),[0,nx(0)],[y,y],state
			fuse_analysis_overlay,ids(0),[0,nx(0)],[y1,y1],state
		    end
		    return
		end
;
; button for first y value
;
		if (state.save(0) eq 0) then begin
			state.save(0:4) = [1,x,y,w,fval]
			widget_control,widget.text_base,set_value= $
			     'Push Mouse Button at second Y-range value'
			return
		end
;
; button pressed for second y value
;			
		fmin = state.save(4)<fval
		fmax = state.save(4)>fval
		if fmin ne fmax then begin
			widget_control,widget.plot_select,get_value=isel
			plotpar(isel).ymin = fmin
			plotpar(isel).ymax = fmax
		end
		fuse_analysis_plot
		state.type = 'X/Y'
		widget_control,widget.text_base,set_value=' '
		end
	'ROW_SUM': begin
		if iwindow eq 0 then return
	       	fuse_analysis_overlay,ids(iwindow), $
		    			[0,nx(iwindow)],[y,y],state
		if imname eq '' then return
		if state.imname ne '' then $
			fuse_analysis_overlay,ids(state.save(0)), $
	    			[0,nx(iwindow)],[0,0]+state.save(1),state
		if event.press ne 0 then begin
		    if state.imname eq '' then begin
			state.imname = imname
			state.save(0) = iwindow
			state.save(1) = y
			state.save(2) = yim
			widget_control,widget.text_base,set_value='Place '+ $
				'cursor at 2nd row of '+imname+' image'+ $
				'and push mouse button'
		      end else begin
			row1 = round(state.save(2))
			row2 = round(yim)
			fuse_analysis_rowsum,imname,row1,row2
			state.type = 'X/Y'
		    end	       	
		end
		end
	else:
	endcase
	return
end

;------------------------------------------------------- FUSE_ANALYSIS_OVERLAY
;
; Routine to draw overlay lines
;
pro fuse_analysis_overlay,id,x,y,state

	device,set_graphic=6

	n = state.nlines
	m = n_elements(x)			
	wset,id
	for i=0,m-2 do begin
		plots,x[i:i+1],y[i:i+1],/dev,color=!d.n_colors-1
		state.s1(n) = x(i)
		state.s2(n) = x(i+1)
		state.l1(n) = y(i)
		state.l2(n) = y(i+1)
		state.ids(n) = id
		n = n+1
	end
	state.nlines = n
	device,set_graphic=3
	return
end
;----------------------------------------------------- FUSE_ANALYSIS_RMOVERLAY
;
;
; erase plot overlay
;
pro fuse_analysis_rmoverlay,state

	if state.nlines gt 0 then begin
		device,set_graphic=6
		for i=0,state.nlines-1 do begin
			wset,state.ids(i)
			plots,[state.s1(i),state.s2(i)], $
			      [state.l1(i),state.l2(i)],color=!d.n_colors-1,/dev
		end
		state.nlines = 0
		device,set_graphic=3
	end
	return
end
;------------------------------------------------------ FUSE_ANALYSIS_CURVAL
; 
; Routine to get value of the cursor
;
pro fuse_analysis_curval,x,y,uvalue,w,fval,xim,yim,imname,text
;
; INPUTS: x,y - cursor coordinates
;	  uvalue - which window
; OUTUTS: w - wavelength (For all windows)
;	  fval - flux or image value 
;	  xim,yim - image pixel
;	  imname - name of image (e.g. 'SiC1a')
;         text - text message
;
@fuse_analysis_common
	xim = -1
	yim = -1
	imname = ''
	text = ''
;
; get wavelength
;
	wrange = widget.wrange
	if uvalue ne 'ZOOM_WINDOW' then begin
		v = convert_coord(x,y,/device,/to_data)
		w = v(0)>!x.crange(0)<!x.crange(1)
		text = 'W = '+strtrim(string(w,'(F12.4)'),2)
	end
;
; get flux for plot window
;	
	if uvalue eq 'PLOT_WINDOW' then begin
		fval = v(1)>!y.crange(0)<!y.crange(1)
		text = text + '   F = '+strtrim(fval)
	end
;
; get image coordinates for pic window
;
	imname = ''
	ny = widget.pic_ny
	if (uvalue eq 'PIC1') then begin
	    xim = -1
	    fuse_analysis_imcoord,sic1a,x,y,ny,xim,yim,val,imname,text
	    fuse_analysis_imcoord,sic1b,x,y,ny,xim,yim,val,imname,text
	end
	if (uvalue eq 'PIC2') then begin
	    xim = -1
	    fuse_analysis_imcoord,sic2a,x,y,ny,xim,yim,val,imname,text
	    fuse_analysis_imcoord,sic2b,x,y,ny,xim,yim,val,imname,text
	end
	if (uvalue eq 'PIC3') then begin
	    xim = -1
	    fuse_analysis_imcoord,lif1a,x,y,ny,xim,yim,val,imname,text
	    fuse_analysis_imcoord,lif1b,x,y,ny,xim,yim,val,imname,text
	end
	if (uvalue eq 'PIC4') then begin
	    xim = -1
	    fuse_analysis_imcoord,lif2a,x,y,ny,xim,yim,val,imname,text
	    fuse_analysis_imcoord,lif2b,x,y,ny,xim,yim,val,imname,text
	end
;
; Zoom Window
;
	if (uvalue eq 'ZOOM_WINDOW') then begin
	    if zoom.imname eq '' then return
	    imname = zoom.imname
	    xim = x/zoom.factor + zoom.x1
	    yim = y/zoom.factor + zoom.y1
	    istat = execute('nx = '+imname+'.nx')
	    istat = execute('ny = '+imname+'.ny')
	    xim = xim>0<(nx-1)
	    yim = yim>0<(ny-1)
	    istat = execute('fval = '+imname+'.image(xim,yim)')
	    istat = execute('ns = spec_'+imname+'.ns')
	    index = round(xim/float(nx)*ns)
	    istat = execute('w = spec_'+imname+'.wave(index) + '+ $
	    			'spec_'+imname+'.woffset')
	    text = 'W = '+strtrim(string(w,'(F12.4)'),2)
	    text = text + ' Value = '+strtrim(fval,2)+'  '+imname
	    wset,widget.plot_id
	    v = convert_coord(w,yim,/data,/to_device)
	    x = round(v(0))
	end
	return
end
;------------------------------------------------------ FUSE_ANALYSIS_IMCOORD
;
; Routine to get image coordinates from device coordinates
;
pro fuse_analysis_imcoord,data,x,y,ny,xim,yim,val,imname,text
	
	if data.nx lt 1 then begin
		xim=0
		yim=0
		val=0.0
		imname=' '
		text = ' '
		return
	end
	i1 = data.i1
	i2 = data.i2
	s1 = data.s1
	s2 = data.s2
	if (x lt i1) or (x gt i2) then return	;outside of image
	
	xim = long( (x-i1)/float(i2-i1)*(s2-s1) + s1)
	s = size(data.image) & n = s(2)
	yim = long(y/float(ny)*n)
	val = data.image(xim>0<(data.nx-1),yim>0<(data.ny-1))
	imname = data.channel+data.segment
	text = text + ' Value = '+strtrim(val,2)+'  '+imname
return
end
;-------------------------------------------------------- FUSE_ANALYSIS_ZOOM
;
; Routine to zoom the image
;
pro fuse_analysis_zoom,xim,yim,imname

@fuse_analysis_common

	if strtrim(imname) eq '' then return

	widget_control,widget.zoom_slider,get_value=factor
	nx = widget.zoom_nx/factor + 1
	ny = widget.zoom_ny/factor + 1
	istat = execute('nx_im='+imname+'.nx')
	istat = execute('ny_im='+imname+'.ny')
	halfnx = nx/2
	halfny = ny/2
	x1 = (xim-halfnx)>0
	x2 = (x1 + nx)<(nx_im-1)
	y1 = (yim-halfny)>0
	y2 = (y1 + ny)<(ny_im-1)
	nx = x2-x1+1
	ny = y2-y1+1
	nxzoom = nx*factor
	nyzoom = ny*factor
	istat = execute('scaled = '+imname+'.scaled(x1:x2,y1:y2)')
	istat = execute('image = '+imname+'.image(x1:x2,y1:y2)')
	wset,widget.zoom_id
	erase
	tv,rebin(scaled,nxzoom,nyzoom,/sample)
	zoom = {imname:imname,x1:x1,x2:x2,y1:y1,y2:y2,xim:xim,yim:yim, $
		image:image,factor:factor}
	return
end
;------------------------------------------------------ FUSE_ANALYSIS_ROWSUM
;
; Routine to plot row sum
;
pro fuse_analysis_rowsum,imname,row1,row2
@fuse_analysis_common
;
; set to plot window if not valid window for rowsum
;
	widget_control, widget.plot_select, get_value=isel
	if isel lt 6 then isel = 6
	widget_control, widget.plot_select, set_value=isel
;
; extract row sum
;
	r1 = row1<row2
	r2 = row2>row1
	if r1 ne r2 then $
		istat = execute('rowsum = total('+imname+'.image(*,r1:r2),2)') $
	   else istat = execute('rowsum = '+imname+'.image(*,r1)')
	istat = execute('nx = '+imname+'.nx')
	istat = execute('filename = '+imname+'.filename')
	istat = execute('wave = spec_'+imname+'.wave + '+ $
			'spec_'+imname+'.woffset')
	istat = execute('rev = spec_'+imname+'.reversed')
	wave = frebin(wave,nx)
	title = imname + ' row '+strtrim(r1,2)+' to '+strtrim(r2,2)
	sp = {wave:wave,flux:rowsum,title:title,w0:min(wave),w1:max(wave), $
		reversed:rev,woffset:0.0,ns:nx,filename:filename}
	istat = execute('sp'+strtrim(isel-5,2)+' = temporary(sp)')
	fuse_analysis_plot
return
end
