;+
;				FUSE_LINEFIND
;
; Routine to find line postions for wavelength calibration
;
; CALLING SEQUENCE:
;	fuse_linefind
;		or
;	fuse_linefind,wave,flux,x,header,filename
;
; INPUT: 
;	wave - wavelength vector
;	flux - flux vector
;	x - vector of pixel positions
;	header - FITS header for the data
;	filename - name of the file that the data was read from
;
; HISTORY:
;	version 1, D. Lindler, 1999
;	Oct 2000, added line library wavelength shift buttons.
;-
;
;========================================================== FUSE_LINEFIND_EVENT
; Main Event handler
pro fuse_linefind_event,event

    common fuse_linefind_common,info,data,linetab,results,gcoef

    tvlct,info.red,info.green,info.blue
    if event.id eq info.main then return
	
    widget_control,event.id,get_uvalue=uvalue
    case uvalue of 
	'FILE': begin
		case event.value of
		'FILE.Exit': Begin
			tvlct,info.rsave,info.gsave,info.bsave
			wdelete,info.pixid
			info = 0
			data = 0
			linetab = 0
			results = 0
			widget_control,event.top,/destroy
			return
			end
		'FILE.Read': begin
			fuse_linefind_read,info,data
			fuse_linefind_plot,info,data,results,linetab
			end
		'FILE.Read Log': begin
			file = dialog_pickfile( $
					title='Select Fuse_linefind Log file', $
					filter='*.wlog',/must_exist)
			if file eq '' then return	;no file selected
			openr,unit,file,/get_lun
			st = ''
			while not eof(unit) do begin
				readf,unit,st
				widget_control,info.log,set_value=st,/append
			end
			free_lun,unit
			end
		'FILE.Write Log': begin
			file = dialog_pickfile(file='fuse.wlog', $
						filter='*.wlog',/write)
			if file eq '' then return	;no file selected
			widget_control,info.log,get_value=v
			openw,unit,file,/get_lun,/append
			for i=0,n_elements(v)-1 do printf,unit,v(i)
			free_lun,unit
			info.log_update = 0
			end
		'FILE.Write Plot': begin
			file = dialog_pickfile(file='idl.ps', $
						filter='*.ps',/write)
			if file eq '' then return	;no file selected
			orig_device=!d.name
			set_plot,'ps'
			device,/port,bits=8,xsize=7,ysize=9.5,xoff=0.8, $
				yoff=1.0,/inches,/color,file=file
			tvlct,info.red,info.green,info.blue
			fuse_linefind_plot,info,data,results,linetab,/ps
			device,/close
			set_plot,orig_device
			end
		'FILE.Clear Log': begin
			info.log_update = 0
			widget_control,info.log,set_value=''
			end
		'FILE.Write Spectrum in FITS Table': begin
			file = dialog_pickfile(file='idl.fits', $
						filter='*.fits',/write)
			if file eq '' then return	;no file selected
			a = {x:data.x,y:data.flux}
			mwrfits,a,file,/silent,/create
			end
		endcase
		end
	'NORM': begin
		line_norm,data.wave,data.flux,norm,/modal,group=event.top
		info.norm=1
		data.flux=norm
		fuse_linefind_cont,info,data,results
		fuse_linefind_plot,info,data,results,linetab
		end
	'LINE_TABLE': begin
		fuse_linefind_line,event,linetab,info.line_base
		fuse_linefind_plot,info,data,results,linetab
		if linetab.iselect ge 0 then begin
			w = linetab.wlines(linetab.iselect)
			widget_control,info.wave_field,set_value=w
			widget_control,info.id_label, $
					set_value=linetab.ids(linetab.iselect)
			fuse_linefind_plot,info,data,results,linetab
		end else widget_control,info.id_label,set_value=' '	
		end
	'LINE_LIST': begin
		if linetab.nlines ne 0 then begin
		    linetab.iselect = event.index
		    w = linetab.wlines(linetab.iselect)
		    widget_control,info.wave_field,set_value=w
		    widget_control,info.id_label, $
					set_value=linetab.ids(linetab.iselect)
		    widget_control,info.xmin_base,get_value = wmin
		    widget_control,info.xmax_base,get_value = wmax
		    if (w lt wmin) or (w gt wmax) then begin
		    	widget_control,info.xmin_base,set_value = min(w-1)
			widget_control,info.xmax_base,set_value = max(w+1)
			good = where((data.wave gt w-1) and $
				     (data.wave lt w+1),ngood)
			if ngood gt 5 then begin
			    widget_control,info.ymin_base,set_value = $
			    		min(data.flux(good))
			    widget_control,info.ymax_base,set_value = $
			    		max(data.flux(good))
			end
			fuse_linefind_cont,info,data,results
		    end
		    fuse_linefind_plot,info,data,results,linetab
		end
		end
	'SMOOTH': begin
		case event.value of
		'Smooth.Reset to Original': begin
				data.flux = data.orig
				info.norm = 0
				end
		'Smooth.3 point mean': data.flux = smooth(data.flux,3)
		'Smooth.5 point mean': data.flux = smooth(data.flux,5)
		'Smooth.7 point mean': data.flux = smooth(data.flux,7)
		'Smooth.9 point mean': data.flux = smooth(data.flux,9)
		'Smooth.3 point median': data.flux = median(data.flux,3)
		'Smooth.5 point median': data.flux = median(data.flux,5)
		'Smooth.7 point median': data.flux = median(data.flux,7)
		'Smooth.9 point median': data.flux = median(data.flux,9)
		endcase
		fuse_linefind_cont,info,data,results
		fuse_linefind_plot,info,data,results,linetab
		end
	'V_ISM_FIELD': fuse_linefind_plot,info,data,results,linetab
	'RANGE': fuse_linefind_plot,info,data,results,linetab
	'BASELINE': begin
		info.state = 'ADJUST1'
		widget_control,info.message,set_value='Place Cursor at left'+ $
			' or right baseline position and push mouse button'
		end
	'FINDLINE': begin
		fuse_linefind_find,info,data,results
		fuse_linefind_plot,info,data,results,linetab
		end
	'GFIT': begin
		case event.value of
			1:xgaussfit,results.x,results.flux-results.cont, $
			  bcoef,gcoef,fit,/noguess,group=event.top,/modal
			2:xagaussfit,results.x,results.flux/results.cont, $
			  bcoef,gcoef,fit,group=event.top,/modal
			3:xvoigtfit,results.x,results.flux/results.cont, $
			  bcoef,gcoef,fit,group=event.top,/modal
		end
		if n_elements(gcoef) lt 2 then return
		if event.value eq 1 then results.gfit = fit + results.cont $
		   		    else results.gfit = fit*results.cont
		results.gcenter = gcoef(1)
		widget_control,info.cfield(1), $
				set_value=string(gcoef(1),'(F8.2)')
		fuse_linefind_plot,info,data,results,linetab
		fuse_linefind_gcoef,group=event.top
		end
	'WAVE_FIELD': widget_control,info.id_label,set_v = ' '	
	'ZOOM': begin
		widget_control,info.message,set_value= $
			'Place Cursor on first corner'+ $
			' and push left mouse button'
		info.state = 'ZOOM1'
		end
	'UNZOOM_ALL': begin
		widget_control,info.xmin_base,set_value = min(data.wave)
		widget_control,info.xmax_base,set_value = max(data.wave)
		widget_control,info.ymin_base,set_value = min(data.flux)
		widget_control,info.ymax_base,set_value = max(data.flux)
		fuse_linefind_cont,info,data,results

		fuse_linefind_plot,info,data,results,linetab
		end
	'UNZOOM': begin
		widget_control,info.xmin_base,get_value = wmin
		widget_control,info.xmax_base,get_value = wmax
		widget_control,info.ymin_base,get_value = ymin
		widget_control,info.ymax_base,get_value = ymax
		wrange = (wmax-wmin)
		yrange = (ymax-ymin)
		wmin = (wmin - wrange*0.25) > min(data.wave) < wmin
		wmax = (wmax + wrange*0.25) < max(data.wave) > wmax
		ymin = (ymin - yrange*0.25) > min(data.flux) < ymin
		ymax = (ymax + yrange*0.25) < max(data.flux) > ymax
		widget_control,info.xmin_base,set_value = wmin
		widget_control,info.xmax_base,set_value = wmax
		widget_control,info.ymin_base,set_value = ymin
		widget_control,info.ymax_base,set_value = ymax
		fuse_linefind_cont,info,data,results
		fuse_linefind_plot,info,data,results,linetab
		end
	'WLEFT': begin
		data.wave = data.wave + 0.03
		fuse_linefind_plot,info,data,results,linetab
		end		
	'WRIGHT': begin
		data.wave = data.wave - 0.03
		fuse_linefind_plot,info,data,results,linetab
		end		
	'PLOT1': begin
		xd = event.x	;device coordinates
		yd = event.y
		wset,info.plot_id
		device,copy=[0,0,750,450,0,0,info.pixid]
		if (info.state eq 'X/Y') then begin
		    plots,[xd,xd],[0,450],color=1,/dev
		    plots,[0,750],[yd,yd],color=1,/dev
		    if event.press eq 1 then begin
		    	!x = info.xsave
			!y = info.ysave
			v = convert_coord(xd,yd,/dev,/to_data)
			linterp,data.wave,data.x,v(0),xpos
			widget_control,info.cfield(3),set_value=xpos(0)
			fuse_linefind_plot,info,data,results,linetab
		    endif
		    if event.press eq 2 then begin
				info.x1 = xd
				info.y1 = yd
				info.state = 'ZOOM2'
				return
		    endif
		end
		if strmid(info.state,0,6) eq 'ADJUST' then  $
			fuse_linefind_adjust,event,info,data,results,linetab
		if info.state eq 'ZOOM1' and (event.press eq 1) then begin
		        plots,[xd,xd],[0,450],color=1,/dev
		    	plots,[0,750],[yd,yd],color=1,/dev
		   	info.x1 = xd
			info.y1 = yd
			widget_control,info.message,set_value= $
				'Position at second corner and'+ $
				' push left mouse button'
			info.state = 'ZOOM2'
			return
		endif
		if (info.state eq 'ZOOM2') then begin
		    x = [info.x1,xd]
		    y = [info.y1,yd]
		    plots,[x(0),x(1),x(1),x(0),x(0)], $
			  [y(0),y(0),y(1),y(1),y(0)],/dev,color=1
		    
		    if (event.release eq 2) or (event.press eq 1) then begin
		    	if abs(x(1)-x(0)) lt 2 then begin
				widget_control,info.message,set_value=' '
		    		fuse_linefind_plot,info,data,results,linetab
				return
		    	end
			!x = info.xsave
			!y = info.ysave
			v = convert_coord(x,y,/dev,/to_data)
			x = v(0,*)
			y = v(1,*)		    
		    	widget_control,info.xmin_base,set_value = min(x)
			widget_control,info.xmax_base,set_value = max(x)
			widget_control,info.ymin_base,set_value = min(y)
			widget_control,info.ymax_base,set_value = max(y)
			widget_control,info.message,set_value=' '
			fuse_linefind_cont,info,data,results
			fuse_linefind_plot,info,data,results,linetab
			return
		    end
		end	    			
		end
	'SELECT_C': fuse_linefind_select,info,data,uvalue,results 
	'SELECT_G': fuse_linefind_select,info,data,uvalue,results 
	'SELECT_E': fuse_linefind_select,info,data,uvalue,results 
	'SELECT_U': fuse_linefind_select,info,data,uvalue,results 
	'CLOSE_GCOEF': widget_control,event.top,/destroy
	'SELECT_GCOEF': fuse_linefind_select,info,data,uvalue,results 		
	'GCOEF_LIST': begin
		if event.index ge 0 then begin
			results.gcenter = gcoef(1+event.index*3)
			widget_control,info.cfield(1), $
				set_value=string(results.gcenter,'(F8.2)')
			fuse_linefind_plot,info,data,results,linetab
		end
		end
	'DISPERSION': begin
		widget_control,info.log,get_value=st
		n = n_elements(st)
		xtab = fltarr(n)
		wtab = fltarr(n)
		k = 0
		for i=0,n-1 do begin
			st1 = strtrim(st(i))
			if st1 ne '' then begin
				xtab(k) = gettok(st1,' ')
				w = float(gettok(st1,' '))
				vel = float(gettok(st1,' '))
				wtab(k) = w + vel/2.997925d5*w
				if (w gt 0.0) and (xtab(k) gt 0.0) then k = k+1
			end
		end
		if k lt 2 then return
		xtab = xtab(0:k-1)
		wtab = wtab(0:k-1)
		fuse_linefit,data.wave,data.flux,data.x,wtab,xtab,data.h, $
				data.filename,group=event.top
		end			

	else:
    endcase
    istat = xregistered('fuse_linefind_gcoef')	;bring to top
return
end
;------------------------------------------------------ FUSE_LINEFIND_READ
; Routine to read data file
;
pro fuse_linefind_read,info,data

	file = dialog_pickfile(title='Select FUSE data file', $
			/must_exist,filter=data.directory+'*fcal.fit')
	if file eq '' then return
	fdecomp,file,disk,directory,name,ext
	a = mrdfits(file,1,h,/silent)
	junk = mrdfits(file,0,h,/silent)
	wave = a.wave
	flux = a.flux
	bad = wherenan(flux,nbad)
	if nbad gt 0 then flux(bad) = 0.0
	bad = where((flux gt 1e30) or (flux lt 1e-30),nbad)
	if nbad gt 0 then flux(bad) = 0.0
	ns = n_elements(wave)
	x = findgen(ns)
	if wave(ns-1) lt wave(0) then begin
		wave = reverse(wave)
		flux = reverse(flux)
		x = reverse(x)
	end
	data = {ns:ns,filename:name+'.'+ext,wave:wave,flux:flux, orig:flux,$
		directory:directory, h:h,x:x}
	info.state = 'X/Y'
	widget_control,info.xmin_base,set_value=min(wave)
	widget_control,info.xmax_base,set_value=max(wave)
	widget_control,info.ymin_base,set_value=min(flux)
	widget_control,info.ymax_base,set_value=max(flux)
	widget_control,info.main,tlb_set_title=data.filename
return
end
;------------------------------------------------------- FUSE_ANALYSIS_LINE
;
; Routine to process Line table event
;
pro fuse_linefind_line,event,linetab,linebase

    changed = 0
    case event.value of
;
; add line
;
    	'Line Table.Add Line': begin
		fuse_linefind_getline,wline,id
		if wline ne 0.0 then begin
		    changed = 1
		    if linetab.nlines eq 0 then begin
			linetab = {nlines:1,iselect:-1,wlines:wline,ids:id}
	    	      end else begin
			linetab = {nlines:linetab.nlines+1, $
				iselect:linetab.iselect, $
			   	wlines: [linetab.wlines,wline], $
			   	ids:[linetab.ids,id]}
		    endelse
		endif
		endcase
;
; delete selected line
;			
	'Line Table.Delete Selected Line': begin
		if linetab.iselect lt 0 then begin
			result = dialog_message('No Line Selected', $
					dialog_parent=event.top,/error)
		    end else begin
		    	if linetab.nlines eq 1 then begin
				linetab = {nlines:0,iselect:-1}
			    end else begin
				index = lindgen(linetab.nlines)
				good = 	where(index ne linetab.iselect)
				linetab = {nlines:linetab.nlines-1, $
					iselect:-1, $
					wlines:linetab.wlines(good), $
					ids:linetab.ids(good)}
			end
			changed = 1
		endelse
		endcase			
	'Line Table.Read Table': begin
		file = dialog_pickfile(/read,/must_exist,group=event.top)
		if strtrim(file) ne '' then begin
		    readcol,file,wlines,id1,id2,id3,format='D,A,A,A'
		    ids = strtrim(id1+' '+id2+' '+id3)
		    if linetab.nlines eq 0 then begin
		        linetab = {nlines:n_elements(wlines), iselect:-1, $
				   wlines:wlines, ids:ids}
		      end else begin
			linetab = {nlines:linetab.nlines + n_elements(wlines), $
				   iselect:linetab.iselect, $
				   wlines: [linetab.wlines,wlines], $
				   ids: [linetab.ids,ids]}
		    endelse
		changed = 1
		endif
		endcase
	'Line Table.Clear Selection': begin
		linetab.iselect=-1
		changed=1
		end
	'Line Table.Delete Table': begin
		linetab = {nlines:0,iselect:-1}
		changed = 1
		endcase
    endcase

    if changed then begin
;
; sort modified linetab
;
	if linetab.nlines gt 0 then begin
		sub = sort(linetab.wlines)
		iselect = linetab.iselect
		if iselect ge 0 then iselect = where(sub eq iselect)
		linetab = {nlines:linetab.nlines, iselect:iselect(0), $
			   wlines:linetab.wlines(sub), $
			   ids:linetab.ids(sub)}
;
; load new table
;
		st = strtrim(indgen(linetab.nlines)+1,2) + '  '+ $
			string(linetab.wlines,'(F9.4)')+'    '+linetab.ids
	end else st = ''

	widget_control,linebase,set_value = st
	widget_control,linebase,set_list_select = linetab.iselect
	
    end

return
end
;-------------------------------------------------------- FUSE_LINEFIND_GETLINE	
;
; Routine to get a line to add to the line ID table
;
pro fuse_linefind_getline,wline,id,group=group

	mainbase = widget_base(/col,group=group,title='Add New Line')
	wave_base = cw_field(mainbase,/float,title='Wavelength',xsize=12, $
				/return_events,uvalue='WAVE')
	id_base = cw_field(mainbase,title='Identification',xsize=10, $
				/return_events,uvalue='ID')
	base = widget_base(mainbase,/row)
	button = widget_button(base,uvalue='DONE',value='Done')
	button = widget_button(base,uvalue='CANCEL',value='Cancel')
	widget_control,mainbase,/realize
	info = ptr_new({wave_base:wave_base,id_base:id_base,wline:0.0d0,id:' '})
	widget_control,mainbase,set_uvalue=info
	xmanager,'fuse_linefind_getline',mainbase
	wline = (*info).wline
	id = (*info).id
	ptr_free,info
	return
end
pro fuse_linefind_getline_event,event

	widget_control,event.id,get_uvalue=uvalue
	
	case uvalue of
		'DONE': begin
			widget_control,event.top,get_uvalue=info
			widget_control,(*info).wave_base,get_value=v
			(*info).wline = v
			widget_control,(*info).id_base,get_value=v
			(*info).id = v
			end
		'CANCEL':
		'WAVE': return
		'ID': return
	endcase				
	widget_control,event.top,/destroy
return
end



;---------------------------------------------------------- FUSE_LINEFIND_PLOT
;
; Routine to plot the data
;
pro fuse_linefind_plot,info,data,results,linetab,ps=ps
;
; get plot range
;
	widget_control,info.xmin_base,get_value=wmin
	widget_control,info.xmax_base,get_value=wmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax
;
; plot data
;
	if keyword_set(ps) then begin
		set_viewport,0.1,0.95,0.4,0.85
	   end else begin
		wset,info.plot_id
		set_viewport,0.1,0.95,0.1,0.85
	end	   
	plot,data.wave,data.flux,xrange=[wmin,wmax],yrange=[ymin,ymax], $
		xstyle=9,ystyle=1,color=1,/nodata
	if not keyword_set(ps) then begin
		info.xsave = !x
		info.ysave = !y
	end
;
; overplot continuum estimate
;
	oplot,[wmin,wmax],info.continuum,color=3

	oplot,data.wave,data.flux,color=1
;
; overplot results
;

	if results.absorption then sign=-1 else sign=1
;
; gaussian
;
	linterp,[wmin,wmax],info.continuum,results.wave,cont
	oplot,results.wave,results.gfit,color=6
	linterp,data.x,data.wave,results.gcenter,w
	oplot,[w,w],!y.crange,color=6,thick=2
;
; centroid
;
	linterp,data.x,data.wave,results.centroid,w
	oplot,[w,w],!y.crange,color=4,line=2,thick=2
	oplot,results.wave,cont+sign*results.thresh,color=4
;
; edges
;
	linterp,data.x,data.wave,results.center,w
	oplot,[w,w],!y.crange,color=5
	oplot,results.wave,cont+sign*results.hmax,color=5
	oplot,results.wave,cont+sign*results.fmax,color=5
	linterp,data.x,data.wave,results.edges,w
	oplot,[w(0),w(0)],!y.crange,color=5
	oplot,[w(1),w(1)],!y.crange,color=5
;
; user
;
	widget_control,info.cfield(3),get_value=v
	linterp,data.x,data.wave,v,w
	oplot,[w,w],!y.crange,color=8,line=3
;
; overplot lines
;
	if (linetab.nlines gt 0) then begin
		widget_control,info.v_ism_field,get_value=v_ism
		w = linetab.wlines
		w = w + v_ism/2.997925d5*w
		ids = linetab.ids
		good = where((w ge wmin) and (w le wmax),n)
		dely = (ymax-ymin)/20
		delw = (wmax-wmin)/200.0
		for i=0,n-1 do begin
			plots,[0,0]+w(good(i)),[0,1.5*dely]+ymax, $
				color=8,/data
			xyouts,w(good(i))+delw,ymax+1.6*dely,ids(good(i)), $
					color=8,orient=90
		end
		if linetab.iselect ge 0 then $
			plots,[0,0]+w(linetab.iselect),[0,1.5*dely]+ymax, $
				color=1,/data,thick=3
		
	end
	
;
; plot pixel axis
;
	linterp,data.wave,data.x,[wmin,wmax],xrange
	axis,/xaxis,color=1,xrange=xrange,xstyle=1,xtickformat='(I6)'
	if keyword_set(ps) then begin
	    	!p.font = 0
	    	xyouts,0.5,0.3,data.filename+'   '+sxpar(data.h,'targname'), $
				/norm,align=0.5,color=1,charsize=1.3
	 	widget_control,info.wave_field,get_value=w
		widget_control,info.v_ism_field,get_value=vism
		widget_control,info.id_label,get_value=id
		xyouts,0.05,0.25,'Rest Wavelength = '+strtrim(w,2)+ $
				'       '+id,color=1,/norm
		xyouts,0.05,0.22,/norm,color=1, $
			'Velocity ISM = '+ strtrim(string(vism,'(F9.1)'),2)
		xyouts,0.05,0.16,/norm,color=1, $
		    	'Gaussian Center = ' +  $
		    	strtrim(string(results.gcenter,'(F9.2)'),2)
		xyouts,0.05,0.19,/norm,color=1, $
		    'Centroid = '+strtrim(string(results.centroid,'(F9.2)'),2)
		xyouts,0.05,0.16,/norm,color=1, $
		    	'Gaussian Center = ' +  $
		    	strtrim(string(results.gcenter,'(F9.2)'),2)
		xyouts,0.05,0.13,/norm,color=1, $
			'Center From Edges = '+ $
			strtrim(string(results.center,'(F9.2)'),2)
	   	!p.font = -1
	 end else begin
	   	wset,info.pixid
	   	device,copy=[0,0,750,450,0,0,info.plot_id]
	end
	info.state = 'X/Y'
	return
end
;=========================================================== FUSE_LINEFIND_CONT
;
; Routine to compute a continuum
;
pro fuse_linefind_cont,info,data,results
;
; get range of data plotted
;
	widget_control,info.xmin_base,get_value=wmin
	widget_control,info.xmax_base,get_value=wmax
	
	good = where((data.wave ge wmin) and (data.wave le wmax),n)
	if n le 0 then return
	w = data.wave(good)
	f = data.flux(good)
	if info.norm then begin
		info.continuum = [1.0,1.0]
	    end else begin
		if n lt 20 then begin
			info.continuum = [f(0),f(n-1)]
			return
		end
;
; use first 10% and last 10% of data
;
		n10p = n/10
		f1 = avg(f(0:n10p-1))
		w1 = avg(w(0:n10p-1))
		f2 = avg(f(n-n10p:n-1))
		w2 = avg(w(n-n10p:n-1))
		info.continuum = interpol([f1,f2],[w1,w2],[wmin,wmax])
	end
;
; determine if emission or absorption
;
	cont = interpol(info.continuum,[wmin,wmax],w)
	if total(f-cont) gt 0 then isel = 0 else isel = 1
	widget_control,info.linetype,set_value=isel
;
; reinitialize results 
;
	cont = interpol(info.continuum,[min(w),max(w)],w)
	results = {wave:w,x:data.x(good), $
		gfit:w*0,gcenter:0.0,fmax:0.0, $
		thresh:0.0,centroid:0.0,hmax:0.0,edges:[0.0,0.0], $
		center:0.0,absorption:0,flux:f,cont:cont,mult_cont:0}



	return
end
;======================================================== FUSE_LINEFIND_ADJUST
;
; Routine to adjust continuum
;
pro fuse_linefind_adjust,event,info,data,results,linetab
	if info.state eq 'ADJUST1' then if event.press eq 0 then return
	info.state = 'ADJUST2'
;
; convert to data coordinates
;
	x = event.x
	y = event.y
	!x = info.xsave
	!y = info.ysave
	v = convert_coord(x,y,/dev,/to_data)
	w = v(0)
	f = v(1)
;
; determine if left or right continuum level being changed
;
	widget_control,info.xmin_base,get_value=wmin
	widget_control,info.xmax_base,get_value=wmax
	wmid = (wmax+wmin)/2.0
	if w lt wmid then begin				;left continuum
		wcont = [w,wmax]
		fcont = [f,info.continuum(1)]	
	   end else begin
	   	wcont = [wmin,w]
		fcont = [info.continuum(0),f]
	end
;
; extend to full wavelength range
;
	info.continuum = interpol(fcont,wcont,[wmin,wmax])
	plots,[wmin,wmax],info.continuum,color=3,thick=2,line=2
	if event.release gt 0 then begin
		fuse_linefind_plot,info,data,results,linetab
		widget_control,info.message,set_value=' '
	end
	results.cont = interpol(info.continuum,[wmin,wmax],results.wave)
return
end
;========================================================= FUSE_LINEFIND_SELECT
;
; Routine to write selected results to the log
;
pro fuse_linefind_select,info,data,uvalue,results
;
; get x position
;
	type = strmid(uvalue,7,1)
	case type of
		'C': x = results.centroid
		'G': x = results.gcenter
		'E': x = results.center
		'U': widget_control,info.cfield(3),get_value=x
	endcase
;
; get wavelength and Velocity of the ISM
;
	widget_control,info.wave_field,get_value=w
	widget_control,info.v_ism_field,get_value=vism
	widget_control,info.id_label,get_value=id
	
	st = string(x,'(F9.2)')+string(w,'(F10.3)')+string(vism,'(F7.1)')+ $
		' '+type+' '+data.filename+' '+id
	widget_control,info.log,set_value=st,/append
	info.log_update = 1
	return
end	
;=========================================================== FUSE_LINEFIND_FIND
;
; Routine to find the line centers
;
pro fuse_linefind_find,info,data,results
;
; extract data within plot range
;
	widget_control,info.xmin_base,get_value=wmin
	widget_control,info.xmax_base,get_value=wmax
	wave = data.wave
	good = where((wave ge wmin) and (wave le wmax),n)
	if n lt 5 then return
	x = data.x(good)
	flux = data.flux(good)
	wave = wave(good)
;
; subtract continuum
;
	cont = interpol(info.continuum,[wmin,wmax],wave)
	flux = flux - cont
;
; fit gaussian
;
	fmax = max(abs(flux))
	ff = flux/fmax
	xx = x-min(x)
	n = n_elements(xx)
	if avg(ff(n/3:n*2/3)) gt avg(ff) then c0 = max(ff) else c0 = min(ff)
	c1 = xx(n/2)
	c2 = (max(xx)-min(xx))/6.0
	gfit = gaussfit(xx,ff,coef,nterms=3,estimate=[c0,c1,c2])
	gcenter = coef(1)+min(x)
	gfit = gfit*fmax
;
; find line type emission/absorption
;
	widget_control,info.linetype,get_value=isel
	if isel eq 0 then absorption=0 else absorption=1
	if absorption then flux = -flux
;
; find centroid
;
	fmax = max(flux)
	thresh = fmax*info.centroid_thresh
	f = (flux - thresh)>0
	centroid = total(f*x)/total(f)			
;
; find center using edges
;
	hmax = 0.5*fmax
	good = where(flux ge hmax,ng)
	i1 = good(0)
	i2 = good(ng-1)
	if (i1 eq 0) or (i2 eq n-1) then begin
		center = 0.0
		edges = [0.0,0.0]
	   end else begin
	   	linterp,flux(i1-1:i1),x(i1-1:i1),hmax,x1
		linterp,flux(i2:i2+1),x(i2:i2+1),hmax,x2
		edges = [x1(0),x2(0)]
		center = avg(edges)
	end
;
; create structure with results and write into widget
;
	results = {wave:wave,x:x,gfit:gfit+cont,gcenter:gcenter,fmax:fmax, $
		thresh:thresh,centroid:centroid,hmax:hmax,edges:edges, $
		center:center,absorption:absorption,flux:data.flux(good), $
		cont:cont,multcont:0}
	widget_control,info.cfield(0),set_value=string(centroid,'(F8.2)')
	widget_control,info.cfield(1),set_value=string(gcenter,'(F8.2)')
	widget_control,info.cfield(2),set_value=string(center,'(F8.2)')
	return
end
;----------------------------------------------------------  FUSE_LINEFIND_GCOEF
;
pro fuse_linefind_gcoef,group=group


    	common fuse_linefind_common,info,data,linetab,results,gcoef

	if xregistered('fuse_linefind_gcoef') then  $
			widget_control,info.gcoef_main,/destroy
	main = widget_base(/col,group=group)
	menu = widget_base(main,/row)
	button = widget_button(menu,value='Close',uvalue='CLOSE_GCOEF')
	button = widget_button(menu,value='Select',uvalue='SELECT_GCOEF')
	label = widget_label(main,value='Center       ', $
		/align_left)

	center = transpose(gcoef(1,*))
	peak = transpose(gcoef(0,*))
	sigma = transpose(gcoef(2,*))
	value = string(center,'(F9.2)')+string(peak,'(G17.5)')+ $
			string(sigma,'(G12.5)')
	gcoef_list = widget_list(main,uvalue='GCOEF_LIST',xsize=30,ysize=10, $
			value = value)
	info.gcoef_main = main
	info.gcoef_list = gcoef_list
	widget_control,main,/realize
	widget_control,gcoef_list,set_list_select=0
	xmanager,'fuse_linefind_gcoef',main,/no_block, $
		event_handler='fuse_linefind_event'
	return
end
;=========================================================== FUSE_LINEFIND
;
; Main widget driver
;
pro fuse_linefind,wave,flux,x,header,filename,group=group,modal=modal
	common fuse_linefind_common,info,data,linetab,results,gcoef
;
; initialization
;
	if n_elements(wave) eq 0 then wave=findgen(1000)
	if n_elements(flux) eq 0 then flux=fltarr(1000)
	if n_elements(x) eq 0 then x = findgen(1000)
	if n_elements(header) eq 0 then header = ['END           ']
	if n_elements(filename) eq 0 then filename=' '
	if xregistered('fuse_linefind') then return
	tvlct,red,green,blue,/get
	rsave = red
	gsave = green
	bsave = blue
;		      0 1 2  3   4   5   6   7   8   9   10  11
	red(0) =   [255,0,0,255,255,  0,  0,180,240, 75,110,150]
	green(0) = [255,0,0,  0,  0,220,  0, 80,140,180,110,120]
	blue(0) =  [255,0,0,255,  0,  0,255,180,100, 40,185,150]
	tvlct,red,green,blue

	data = {ns:n_elements(wave),filename:filename,directory:'', $
		wave:wave,x:x,orig:flux,flux:flux,h:header}
	linetab = {nlines:0,iselect:-1}
	results = {wave:wave,x:x, $
		gfit:x*0,gcenter:0.0,fmax:0.0, $
		thresh:0.0,centroid:0.0,hmax:0.0,edges:[0.0,0.0], $
		center:0.0,absorption:0,flux:flux,cont:flux*0,multcont:0}

;;	widget_control,default_font  = $
;;		 '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

	main = widget_base(/col,group=group,/tracking)
;
; menu
;
	menu = widget_base(main,/row,/frame)
	desc = ['1\FILE','0\Read','0\Read Log','0\Write Log','0\Write Plot', $
		'0\Clear Log','0\Write Spectrum in FITS Table','2\Exit']
	button = cw_pdmenu(menu,desc,uvalue='FILE',/return_full_name)
	button = widget_button(menu,value = 'Normalize Spectrum',uvalue='NORM')
	desc = ['1\Line Table','0\Add Line','0\Delete Selected Line', $
		'0\Read Table','0\Clear Selection','2\Delete Table']
	button = cw_pdmenu(menu,desc,uvalue='LINE_TABLE',/return_full_name)
	desc = ['1\Smooth','0\Reset to Original','0\3 point mean', $
		'0\5 point mean','0\7 point mean','0\9 point mean', $
		'0\3 point median','0\5 point median','0\7 point median', $
		'2\9 point median']
	button = cw_pdmenu(menu,desc,uvalue='SMOOTH',/return_full_name)
	button = widget_button(menu,uvalue='UNZOOM_ALL',value='UnZoom All')
	button = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
	button = widget_button(menu,uvalue='ZOOM',value='Zoom')
	button = widget_button(menu,uvalue='BASELINE',value='Adjust Continuum')

	desc = ['1\Multiple Line Fit','0\Additive Gaussians', $
		'0\Multiplicative Gaussian Absorption', $
		'2\Multiplicative Voigt Absorption']
	gfit_button = cw_pdmenu(menu,desc,uvalue='GFIT')
	button = widget_button(menu,value='Dispersion Solution', $
		uvalue='DISPERSION')
	button = widget_button(menu,value='<--W',uvalue='WLEFT')
	button = widget_button(menu,value='W-->',uvalue='WRIGHT')
	message = widget_label(main,value='       ',xsize=800)
	
	split = widget_base(main,/row)
	splita = widget_base(split,/col)
	splitb = widget_base(split,/col)
;
; plot window
;	
	plot1 = widget_draw(splita,uvalue='PLOT1',retain=2, $
				xsize=750,ysize=450,/button_events,/motion)
;
; log window
;
	log = widget_text(splita,xsize=60,ysize=6,/scroll, $
					uvalue='MESSAGE',/edit)
;
; find line window
;
	linetype = cw_bgroup(splitb,['Emission','Absorption'],/exclusive,/row, $
				/no_release,set_value=0,uvalue='LINETYPE')

	select = lonarr(4)
	cfield = lonarr(4)

	base = widget_base(splitb,/row)
	select(0) = widget_button(base,uvalue='SELECT_C',value='Select')
	label = widget_label(base,value='Centroid              ')
	cfield(0) = widget_label(base,value='00000.00     ')
	label = widget_label(base,value='     (Red)')

	base = widget_base(splitb,/row)
	select(1) = widget_button(base,uvalue='SELECT_G',value='Select')
	label = widget_label(base,value='Gaussian Fit        ')
	cfield(1) = widget_label(base,value='00000.00     ')
	label = widget_label(base,value='     (Blue)')

	base = widget_base(splitb,/row)
	select(2) = widget_button(base,uvalue='SELECT_E',value='Select')
	label = widget_label(base,value='From Edges         ')
	cfield(2) = widget_label(base,value='00000.00     ')
	label = widget_label(base,value='     (Green)')

	base = widget_base(splitb,/row)
	select(3) = widget_button(base,uvalue='SELECT_U',value='Select')
	label = widget_label(base,value='User Specified')
        cfield(3) = cw_field(base,/row,uvalue='RANGE',value=0.0, $
                title=' ',xsize=9,/return_events,/float)
	label = widget_label(base,value='  (Orange)')

	button = widget_button(splitb,value = 'Find Line Center', $
			uvalue='FINDLINE')
	
	wave_field = cw_field(splitb,/row,uvalue='WAVE_FIELD', $
		value='0000.000',title='Rest Wavelength',xsize=10, $
		/return_events)
	v_ism_field = cw_field(splitb,/row,uvalue='V_ISM_FIELD', $
		value=0.0,title='v_ISM: ',xsize=7,/return_events,/float)
	id_label = cw_field(splitb,value='                   ',xsize=38, $
			title=' ',uvalue='ID_FIELD')
	line_base = widget_list(splitb,uvalue='LINE_LIST',xsize=20,ysize=10, $
			value = replicate(' ',50))

	basex = widget_base(splitb,/row,/frame)
        xmin_base = cw_field(basex,/row,uvalue='RANGE',value=min(wave), $
                title='X Min:',xsize=12,/return_events,/float)
        xmax_base = cw_field(basex,/row,uvalue='RANGE',value=max(wave), $
                title='X Max:',xsize=12,/return_events,/float)
 	basex = widget_base(splitb,/row,/frame)
        ymin_base = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Min:',xsize=12,/return_events,/float)
        ymax_base = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Max:',xsize=12,/return_events,/float)
;
; save bases
;
	widget_control,main,/realize
	widget_control,plot1,get_value=plot_id

	window,xs=750,ys=450,/pixmap,/free	;create pixmap
	pixid = !d.window
	

	info = {main:main,plot_id:plot_id,log:log,linetype:linetype, $
		select:select,cfield:cfield,wave_field:wave_field, $
		v_ism_field:v_ism_field,id_label:id_label, $
		line_base:line_base,xmin_base:xmin_base,xmax_base:xmax_base, $
		ymin_base:ymin_base,ymax_base:ymax_base,rsave:rsave, $
		bsave:bsave,gsave:gsave,state:'X/Y',xsave:!x,ysave:!y, $
		x1:0,y1:0,red:red,blue:blue,green:green,message:message, $
		pixid:pixid,continuum:[0.0,0.0],centroid_thresh:0.2, $
		gfit_button:gfit_button,log_update:0, $
		gcoef_main:0L,gcoef_list:0L,norm:0}
	fuse_linefind_cont,info,data,results
	fuse_linefind_plot,info,data,results,linetab

	xmanager,'fuse_linefind',main,/no_block
	return
end
