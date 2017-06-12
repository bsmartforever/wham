@fuse_analysis_read
@fuse_analysis_image
@fuse_analysis_plot
@fuse_analysis_cursor
@fuse_analysis_blink
@xreadfuse
pro fuse_analysis_event,event
@fuse_analysis_common
	widget_control,event.id,get_uvalue=uvalue
	wset,widget.plot_id
	tvlct,rsave,gsave,bsave
;
; erase previous overlay
;
	
	fuse_analysis_rmoverlay,state
	case uvalue of
		'FILE': fuse_analysis_file,event
		'OPTIONS': fuse_analysis_options,event
		'PLOT_SELECT': fuse_analysis_plot
		'LINE_TABLE': fuse_analysis_line,event,linetab,widget.line_base
		'LINE_LIST': begin
			if linetab.nlines ne 0 then begin
				linetab.iselect = event.index
				w = linetab.wlines(linetab.iselect)
				widget_control,widget.vmin_field,get_v=vmin
				widget_control,widget.vmax_field,get_v=vmax
				widget_control,widget.v_ism_field,get_v=vism
				if (vism lt vmin) or $
				   (vism gt vmax) or $
				   (vmin eq vmax) then begin
					vmin = -options.vrange/2.0
					vmax = options.vrange/2.0
				end
			    	wmin = vmin/2.997925d5*w+w
			    	wmax = vmax/2.997925d5*w+w
				widget_control,widget.wmin_field,set_v=wmin
				widget_control,widget.wmax_field,set_v=wmax
				fuse_analysis_plot
			end
			end
		'FIT': fuse_analysis_gfit
		'LINEPLOT': fuse_analysis_lineplot
		'EQWIDTH': fuse_analysis_eqw
		'NORM': begin
			fuse_analysis_norm
			fuse_analysis_plot
			end
;
; wavelength ranges
;
		'WMIN_FIELD' : fuse_analysis_plot
		'WMAX_FIELD' : fuse_analysis_plot
		'V_ISM_FIELD': fuse_analysis_plot
		'VRANGE': begin
			if linetab.iselect ge 0 then begin
			    widget_control,widget.vmin_field,get_v = vmin
			    widget_control,widget.vmax_field,get_v = vmax
			    wline = linetab.wlines(linetab.iselect)
			    wmin = vmin/2.997925d5*wline+wline
			    wmax = vmax/2.997925d5*wline+wline
			    widget_control,widget.wmin_field,set_v = wmin
			    widget_control,widget.wmax_field,set_v = wmax
			    fuse_analysis_plot
			end
			end		
		'WBIGMINUS': begin
			dw = widget.wrange(1) - widget.wrange(0)
			w1 = (widget.wrange(0) - dw*0.8)>900
			w2 = w1 + dw
			widget_control,widget.wmin_field,set_value=w1
			widget_control,widget.wmax_field,set_value=w2
			fuse_analysis_plot
			end
		'WMINUS': begin
			dw = widget.wrange(1) - widget.wrange(0)
			w1 = (widget.wrange(0) - dw*0.2)>900
			w2 = w1 + dw
			widget_control,widget.wmin_field,set_value=w1
			widget_control,widget.wmax_field,set_value=w2
			fuse_analysis_plot
			end
		'WPLUS': begin
			dw = widget.wrange(1) - widget.wrange(0)
			w2 = (widget.wrange(1) + dw*0.2)<1200
			w1 = w2 - dw
			widget_control,widget.wmin_field,set_value=w1
			widget_control,widget.wmax_field,set_value=w2
			fuse_analysis_plot
			end
		'WBIGPLUS': begin
			dw = widget.wrange(1) - widget.wrange(0)
			w2 = (widget.wrange(1) + dw*0.8)<1200
			w1 = w2 - dw
			widget_control,widget.wmin_field,set_value=w1
			widget_control,widget.wmax_field,set_value=w2
			fuse_analysis_plot
			end
		'PLOT_ZOOM': begin
			state.type = 'ZOOM'
			state.save(0) = 0
			widget_control,widget.text_base,set_value = $
			    'Push Mouse button at first corner in plot window'
			return
			end
		'PLOT_ZOOMX': begin
			state.type = 'ZOOMX'
			state.save(0) = 0
			widget_control,widget.text_base,set_value = 'Push ' + $
			   'mouse button at minimum wavelength in plot window'
			return
			end
		'PLOT_ZOOMY': begin
			state.type = 'ZOOMY'
			state.save(0) = 0
			widget_control,widget.text_base,set_value = 'Push ' + $
			   'mouse button at first y-range value in plot window'
			return
			end

		'PLOT_UNZOOM': begin
			widget_control,widget.wmin_field,get_value=w1
			w1 = double(w1(0))
			widget_control,widget.wmax_field,get_value=w2
			w2 = double(w2(0))
			wmid = (w1+w2)/2.0
			wrange = abs((w2-w1))
			w1 = (wmid - wrange)>900
			w2 = (wmid +wrange)<1200
			widget_control,widget.wmin_field,set_value=w1
			widget_control,widget.wmax_field,set_value=w2
			plotpar.ymin = 0
			plotpar.ymax = 0
			fuse_analysis_plot
			end
		'ZOOM_SLIDER': fuse_analysis_zoom,zoom.xim,zoom.yim,zoom.imname

		'PLOT_BLINK': begin
			fuse_analysis_blink,group=event.top
			end
			
;
; cursor events
;			
		'PLOT_WINDOW': fuse_analysis_cursor,event,uvalue
		'PIC1': fuse_analysis_cursor,event,uvalue
		'PIC2': fuse_analysis_cursor,event,uvalue
		'PIC3': fuse_analysis_cursor,event,uvalue
		'PIC4': fuse_analysis_cursor,event,uvalue
		'ZOOM_WINDOW': fuse_analysis_cursor,event,uvalue
;
; Row Sum
;
		'ROW_SUM': begin
			state.type = 'ROW_SUM'
			state.save(0) = 0
			state.imname = ''
			widget_control,widget.text_base,set_value = 'Push ' + $
			   'mouse button at first row in image'
			end	
		else:
	endcase
return
end
;--------------------------------------------------------- FUSE_ANALYIS_FILE
; Event driver for file button
;
pro fuse_analysis_file,event
@fuse_analysis_common

   case event.value of

	'FILE.EXIT': begin
		sic1a=0 & sic1b=0 & sic2a=0 & sic2b=0 
		lif1a=0 & lif1b=0 & lif2a=0 & lif2b=0
		spec_sic1a=0 & spec_sic1b=0 & spec_sic2a=0 & spec_sic2b=0 
		spec_lif1a=0 & spec_lif1b=0 & spec_lif2a=0 & spec_lif2b=0 
		widget_control,event.top,/destroy
		loadct,0
		set_viewport
		return
		end

	'FILE.Read': begin
		fuse_analysis_read,istat,group=event.top
		if istat eq 1 then return
		widget.wrange = [0.0,0.0]
		fuse_analysis_scaleall
		fuse_analysis_plot
		end
	'FILE.PS output.B/W': begin
		fuse_analysis_plot,ps=1
		fuse_analysis_plot
		end
	'FILE.PS output.B/W Reversed': begin
		fuse_analysis_plot,ps=2
		fuse_analysis_plot
		end
	'FILE.PS output.Color': begin
		fuse_analysis_plot,ps=3
		fuse_analysis_plot
		end
	'FILE.Save Session': begin
		widget_control,/hourglass
		file = dialog_pickfile(file='fuse_analysis.idl', $
					filter='*.idl',/write)
		if file eq '' then return	;no file selected
		wrange = widget.wrange
		scale_type = widget.scale_type
		directory = widget.directory
		rootname = widget.rootname
		widget_control,widget.v_ism_field,get_value=vism
		widget_control,widget.text_base,get_value=textfield
		widget_control,widget.file_label,get_value=file_label
		save,f=file,plotpar,options,linetab,state,zoom, $
			sic1a,sic1b,sic2a,sic2b,lif1a,lif1b,lif2a,lif2b, $
			spec_sic1a, spec_sic1b, spec_sic2a, spec_sic2b, $
			spec_lif1a, spec_lif1b, spec_lif2a, spec_lif2b, $
			coadd,sp1,sp2,sp3,sp4,rsave,gsave,bsave, $
			wrange,scale_type,directory,rootname, $
			textfield,vism,file_label
		widget_control,/hourglass
		end
	
	'FILE.Restore Session': begin
		file = dialog_pickfile(file='fuse_analysis.idl', $
					filter='*.idl',/must_exist)
		if file eq '' then return	;no file selected
		widget_control,/hourglass
		restore,file
		widget.wrange= wrange
		widget.scale_type = scale_type
		widget.directory = directory
		widget_control,widget.wmin_field,set_v=wrange(0)
		widget_control,widget.wmax_field,set_v=wrange(1)
		widget_control,widget.v_ism_field,set_value=vism
		widget_control,widget.text_base,set_value=textfield
		widget_control,widget.file_label,set_value=file_label
		if linetab.nlines gt 0 then begin
			st = strtrim(indgen(linetab.nlines)+1,2) + '  '+ $
			string(linetab.wlines,'(F9.4)')+'    '+linetab.ids
		end else st = ''
		widget_control,widget.line_base,set_value = st
		if linetab.iselect ge 0 then $
		    		widget_control,widget.line_base, $
					set_list_select = linetab.iselect
		widget_control,/hourglass
		fuse_analysis_plot
		fuse_analysis_dispall
		tvlct,rsave,gsave,bsave
		end

	else:
	endcase
return
end
;---------------------------------------------------------FUSE_ANALYSIS_OPTIONS
;
; Routine to process Options event
;
pro fuse_analysis_options,event
@fuse_analysis_common
   case event.value of
 	'OPTIONS.Color Table': begin
			xloadct,bottom=12,group=event.top,/modal
			tvlct,rsave,gsave,bsave,/get
			end
	'OPTIONS.Image Contrast': begin
			fuse_analysis_contrast,group=event.top
			end
	'OPTIONS.Plot Properties': fuse_analysis_plotpar
	'OPTIONS.Global Parameters': fuse_analysis_opt,group=event.top
	'OPTIONS.Save': begin
		file = dialog_pickfile(file='fuse_analysis.opt', $
					filter='*.opt',/write)
		if file eq '' then return	;no file selected
		scale_type = widget.scale_type
		save,f=file,scale_type,plotpar,options, $
				rsave,gsave,bsave
		end
	'OPTIONS.Restore': begin
		file = dialog_pickfile(file='fuse_analysis.opt', $
					filter='*.opt',/must_exist)
		if file eq '' then return	;no file selected
		scale_type = widget.scale_type
		restore,file
		widget.scale_type = scale_type
		fuse_analysis_plot
		tvlct,rsave,gsave,bsave
		end
   endcase
   return
end
;------------------------------------------------------- FUSE_ANALYSIS_GFIT
; Routine to fit gaussains
;
	pro fuse_analysis_gfit
@fuse_analysis_common
	fuse_analysis_getspec,spec
	if spec.ns eq 0 then return
	widget_control,widget.wmin_field,get_value=wmin
	wmin = double(wmin(0))
	widget_control,widget.wmax_field,get_value=wmax
	wmax = double(wmax(0))
	good = where((spec.wave ge wmin) and (spec.wave le wmax),ngood)
	if ngood lt 5 then begin
		istat = dialog_message('Not enough data in plot range to fit', $
			/error)
		return
	end
	x = spec.wave(good)
	y = spec.flux(good)
	fdecomp,spec.filename,disk,dir,name
	xgaussfit,x,y,title=name+'  '+spec.title
	return
	end
;------------------------------------------------------- FUSE_ANALYSIS_LINEPLOT
; Routine to call lineplot widget
;
	pro fuse_analysis_lineplot
@fuse_analysis_common
	fuse_analysis_getspec,spec
	if spec.ns eq 0 then return
	widget_control,widget.wmin_field,get_value=wmin
	wmin = double(wmin(0))
	widget_control,widget.wmax_field,get_value=wmax
	wmax = double(wmax(0))
	good = where((spec.wave ge wmin) and (spec.wave le wmax),ngood)
	if ngood lt 5 then begin
		fmax = max(spec.flux)
		fmin = min(spec.flux)
	   end else begin
	   	fmax = max(spec.flux(good))
		fmin = min(spec.flux(good))
	end
	

	fdecomp,spec.filename,disk,dir,name
	lineplot,spec.wave,spec.flux,title=name+'  '+spec.title, $
			xrange=[wmin,wmax],yrange=[fmin,fmax]
	return
	end
;------------------------------------------------------- FUSE_ANALYSIS_EQW
; Routine to call lineplot widget
;
	pro fuse_analysis_eqw
@fuse_analysis_common
	fuse_analysis_getspec,spec
	if spec.ns eq 0 then return
	widget_control,widget.wmin_field,get_value=wmin
	wmin = double(wmin(0))
	widget_control,widget.wmax_field,get_value=wmax
	wmax = double(wmax(0))
	good = where((spec.wave ge wmin) and (spec.wave le wmax),ngood)
	if ngood lt 5 then begin
		istat = dialog_message('Not enough data in plot range to fit', $
			/error)
		return
	end
	x = spec.wave(good)
	y = spec.flux(good)
	fdecomp,spec.filename,disk,dir,name
	if linetab.iselect ge 0 then begin
		wrest = linetab.wlines(linetab.iselect)
		lineid = linetab.ids(linetab.iselect)
	   end else begin
	   	wrest = 0
		lineid = ''
	end
	line_eqwidth,x,y,title=name+'  '+spec.title,group=widget.base,/modal, $
		lineid=lineid,wrest=wrest
	return
	end
;------------------------------------------------------- FUSE_ANALYSIS_NORM
; Routine to call LINE_NORM
;
	pro fuse_analysis_norm,spec
@fuse_analysis_common
	fuse_analysis_getspec,spec
	if spec.ns eq 0 then return
	widget_control,widget.wmin_field,get_value=wmin
	wmin = double(wmin(0))
	widget_control,widget.wmax_field,get_value=wmax
	wmax = double(wmax(0))
	fdecomp,spec.filename,disk,dir,name
	line_norm,spec.wave,spec.flux,ynorm,title=name+'  '+spec.title, $
		group = widget.base,/modal
	spec.flux = ynorm
	fuse_analysis_osel,isel,group=widget.base
	if isel eq 4 then return
	case isel of 
		0: sp1 = spec
		1: sp2 = spec
		2: sp3 = spec
		3: sp4 = spec
	endcase
	widget_control, widget.plot_select, set_value=isel+6
	return
	end
;=========================================================== FUSE_ANALYSIS_OSEL
;
; Select which output plot to use
;
pro fuse_analysis_osel,isel,group=group

	title='Select Output Plot Position'	
	main = widget_base(/col,title=title,xsize=400,/modal,group=group)
	buttons = lonarr(5)
	for i=0,3 do buttons(i) = $
		widget_button(main,value = 'SP'+strtrim(i+1,2))
	buttons(4) = widget_button(main,value='NONE')
	
	widget_control,main,/realize
	ptr = ptr_new({buttons:buttons,isel:0})
	widget_control,main,set_uvalue=ptr
	xmanager,'fuse_analysis_osel',main
	isel = (*ptr).isel
	ptr_free,ptr
end
pro fuse_analysis_osel_event,event
	widget_control,event.top,get_uvalue=ptr
	good = where((*ptr).buttons eq event.id)
	(*ptr).isel = good(0)
	widget_control,event.top,/destroy
	return
end
;-------------------------------------------------------  FUSE_ANALYSIS_GETSPEC
;
; Get spectral structure for selected window and central wavelength
;
	pro fuse_analysis_getspec,spec
@fuse_analysis_common
	widget_control,widget.plot_select,get_value=isel
	if isel eq 0 then begin
		istat = dialog_message('Must select plot other than "ALL"', $
				/ERROR)
		spec = {ns:0}
		return
	end

	widget_control,widget.wmin_field,get_value=wmin
	wmin = double(wmin(0))
	widget_control,widget.wmax_field,get_value=wmax
	wmax = double(wmax(0))
	wcent = (wmin+wmax)/2.0
	case isel of
		1: if wcent lt 998 then spec = spec_sic1b $
				   else spec = spec_sic1a
		2: if wcent lt 1012 then spec = spec_sic2a $
				   else spec = spec_sic2b
		3: if wcent lt 1088 then spec = spec_lif1a $
				   else spec = spec_lif1b
		4: if wcent lt 1080 then spec = spec_lif2b $
				   else spec = spec_lif2a
		5: spec = coadd
		6: spec = sp1
		7: spec = sp2
		8: spec = sp3
		9: spec = sp4
	endcase
	if spec.ns eq 0 then begin
		istat = dialog_message('Selected spectrum does not exist', $
				/ERROR)
		return
	end

	
	return
end
		
;------------------------------------------------------- FUSE_ANALYSIS_LINE
;
; Routine to process Line table event
;
pro fuse_analysis_line,event,linetab,linebase

    changed = 0
    case event.value of
;
; add line
;
    	'Line Table.Add Line': begin
		fuse_analysis_getline,wline,id
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
	fuse_analysis_plot
	
    end

return
end
;-------------------------------------------------------- FUSE_ANALYSIS_GETLINE	
;
; Routine to get a line to add to the line ID table
;
pro fuse_analysis_getline,wline,id,group=group

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
	xmanager,'fuse_analysis_getline',mainbase
	wline = (*info).wline
	id = (*info).id
	ptr_free,info
	return
end

pro fuse_analysis_getline_event,event

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

;--------------------------------------------------------- FUSE_ANALYSIS_INIT
;
; Initialization routine
;
pro fuse_analysis_init
@fuse_analysis_common
	sic1a = {nx:0,filename:''}
	sic1b = {nx:0,filename:''}
	sic2a = {nx:0,filename:''}
	sic2b = {nx:0,filename:''}
	lif1a = {nx:0,filename:''}
	lif1b = {nx:0,filename:''}
	lif2a = {nx:0,filename:''}
	lif2b = {nx:0,filename:''}
	spec_sic1a = {ns:0,filename:'',woffset:0.0}
	spec_sic1b = {ns:0,filename:'',woffset:0.0}
	spec_sic2a = {ns:0,filename:'',woffset:0.0}
	spec_sic2b = {ns:0,filename:'',woffset:0.0}
	spec_lif1a = {ns:0,filename:'',woffset:0.0}
	spec_lif1b = {ns:0,filename:'',woffset:0.0}
	spec_lif2a = {ns:0,filename:'',woffset:0.0}
	spec_lif2b = {ns:0,filename:'',woffset:0.0}
	coadd = {ns:0,title:'',woffset:0.0}
	sp1 = {ns:0,title:'',woffset:0.0}
	sp2 = {ns:0,title:'',woffset:0.0}
	sp3 = {ns:0,title:'',woffset:0.0}
	sp4 = {ns:0,title:'',woffset:0.0}
	zoom = {imname:'',xim:0,yim:0}
	plotpar1 = {ymin:0.0,ymax:0.0,linetype:0,psym:0,pthick:1,nsum:1,red:0, $
			blue:0,green:0,datamin:0.0,datamax:0.0, $
			psymsize:1.0,index:0,yoffset:0.0}
	plotpar = replicate(plotpar1,10)
	plotpar.red   = [255,255,255,  0,  0,  0,180,240, 75,110]
	plotpar.green = [255,  0,  0,255,  0,  0, 80,140,180,110]
	plotpar.blue  = [255,255,  0,  0,255,  0,180,100, 40,185]
	plotpar.index = indgen(10)
	options = {wrange:20,vrange:800,nxbin:2,nybin:2,datadir:'./', $
		  offsets:intarr(8)}
	state = {type:'X/Y', save:fltarr(20), nlines:0, ids:intarr(200), $
		s1:intarr(200),s2:intarr(200),l1:intarr(200),l2:intarr(200), $
		imname:''}
	linetab = {nlines:0,iselect:-1}
return
end	
;===========================================================FUSE_ANALYSIS_OPT
;
; Routine to reset global options options
;
pro fuse_analysis_opt,group=group
@fuse_analysis_common

	main = widget_base(/col,group=group,/modal)
	button = widget_button(main,value='EXIT',uvalue='EXIT')
	binx = cw_field(main,uvalue='FIELD',value=options.nxbin,/integer, $
		xsize=6,title='X Binning Factor for TimeTag Data: ', $
		/return_events)	
	biny = cw_field(main,uvalue='FIELD',value=options.nybin,/integer, $
		xsize=6,title='Y Binning Factor for TimeTag Data: ', $
		/return_events)
	vrange = cw_field(main,uvalue='FIELD',value=options.vrange,/integer, $
		xsize=6,title='Default Velocity Range When Line Selected: ', $
		/return_events)
;	wrange = cw_field(main,uvalue='FIELD',value=options.wrange,/integer, $
;		xsize=6,title='Default Wavelength Range For UNZOOM:', $
;		/return_events)
	label = widget_label(main,value= $
		'Y-Offsets when extracting apertures from')
	label = widget_label(main,value='TimeTag data sets')
	offsets = lonarr(8)
	titles = ['SiC1a','LiF1a','SiC1b','LiF1b','Sic2a','LiF2a', $
		'SiC2b','LiF2b']
	for i=0,7 do offsets(i) = cw_field(main,uvalue='FIELD',/integer, $
		value=options.offsets(i),xsize=6,title=titles(i),/return_events)
	widget_control,main,/realize
	base = {main:main,binx:binx,biny:biny,vrange:vrange, $
		offsets:offsets}
	widget_control,main,set_uvalue=base
	xmanager,'fuse_analysis_opt',main
	return
end
pro fuse_analysis_opt_event,event
@fuse_analysis_common

	widget_control,event.top,get_uvalue=base
	widget_control,base.binx, get_v=v & options.nxbin = v
	widget_control,base.biny, get_v=v & options.nybin = v
	widget_control,base.vrange, get_v=v & options.vrange = v
;	widget_control,base.wrange, get_v=v & options.wrange = v
	for i=0,7 do begin
		widget_control,base.offsets(i),get_v = v
		options.offsets(i) = v
	end
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'EXIT' then widget_control,event.top,/destroy
	return
end


;--------------------------------------------------------- FUSE_ANALYSIS
;
; Main routine
;
pro fuse_analysis
;
; 
@fuse_analysis_common
;
;
; Initialization
;
	if xregistered('fuse_analysis') then return
	fuse_analysis_init
;
; create widget layout
;
	base = widget_base(/col,group=0,/tracking,uvalue='MAIN')
	widget_control,base,/managed
;
; menu bar
;
	menubar = widget_base(base,/row)
	desc = ['1\FILE','0\Read','1\PS output','0\B/W','0\B/W Reversed', $
		'2\Color','0\Save Session','0\Restore Session','2\EXIT']
	button = cw_pdmenu(menubar,desc,uvalue='FILE',/return_full_name)
	desc = ['1\OPTIONS','0\Color Table','0\Image Contrast', $
		'0\Plot Properties','0\Global Parameters','0\Save', $
		'2\Restore']
	button = cw_pdmenu(menubar,desc,uvalue='OPTIONS',/return_full_name)
	button = widget_button(menubar,value='Row Sum',uvalue='ROW_SUM')
	desc = ['1\Line Table','0\Add Line','0\Delete Selected Line', $
		'0\Read Table','0\Clear Selection','2\Delete Table']
	button = cw_pdmenu(menubar,desc,uvalue='LINE_TABLE',/return_full_name)
	desc = ['1\FIT','2\Gaussians']
	button = cw_pdmenu(menubar,desc,uvalue='FIT',/return_full_name)
	button = widget_button(menubar,uvalue='LINEPLOT',value='LinePlot')
	button = widget_button(menubar,uvalue='NORM',value='Norm')
	button = widget_button(menubar,uvalue='EQWIDTH',value='EQWidth')

;
; create two columns (base1 and base2)
;
	basex = widget_base(base,/row)
	base1 = widget_base(basex,/col)
	base2 = widget_base(basex,/col)
;
; column 1 contains draw widgets
;
	
	base1a = widget_base(base1,/col,/frame)
	plot_base = widget_draw(base1a,uvalue='PLOT_WINDOW', $
			xsize=700,ysize=420,/button_events,/motion)
	text_base = widget_text(base1,xsize=74,/frame)
	base1b = widget_base(base1,/col,/frame)
	pic1_base = widget_draw(base1b,uvalue='PIC1',/button_events,/motion, $
			xsize=700,ysize=70)
	pic2_base = widget_draw(base1b,uvalue='PIC2',/button_events,/motion, $
			xsize=700,ysize=70)
	pic3_base = widget_draw(base1b,uvalue='PIC3',/button_events,/motion, $
			xsize=700,ysize=70)
	pic4_base = widget_draw(base1b,uvalue='PIC4',/button_events,/motion, $
			xsize=700,ysize=70)
;
; Column 2 contains everything else
;
	base2split = widget_base(base2,/row)
	base2a = widget_base(base2split,/col,/frame)
	plot_select = cw_bgroup(base2a, $
			['All','Sic1','Sic2','Lif1','Lif2', $
			 'Coadd','SP1','SP2','SP3','SP4'], $
			/col,/no_release,set_value=0,uvalue='PLOT_SELECT', $
			/exclusive)
	button = widget_button(base2a,uvalue='PLOT_BLINK',value='Blink')
	button = widget_button(base2a,uvalue='PLOT_ZOOM',value='Zoom')
	button = widget_button(base2a,uvalue='PLOT_ZOOMX',value='WZoom')
	button = widget_button(base2a,uvalue='PLOT_ZOOMY',value='FZoom')
	button = widget_button(base2a,uvalue='PLOT_UNZOOM',value='UnZoom')
	
	base2b = widget_base(base2split,/col)
	file_label = widget_label(base2b,value='XXXXXXXXXX   LWRS     ')
	basex = widget_base(base2b,/row)
	vmin_field = cw_field(basex,/row,uvalue='VRANGE', $
		value=0,title='Velocity Range: ',xsize=7,/return_events,/long)
	vmax_field = cw_field(basex,/row,uvalue='VRANGE', $
		value=0,title=' ',xsize=7,/return_events,/long)
	basex = widget_base(base2b,/row)
	wmin_field = cw_field(basex,/row,uvalue='WMIN_FIELD',value='900.000', $
		title='WRange: ',xsize=10,/return_events)
	wmax_field = cw_field(basex,/row,uvalue='WMAX_FIELD', $
		value='1200.000',title=' ',xsize=10,/return_events)
	v_ism_field = cw_field(base2b,/row,uvalue='V_ISM_FIELD', $
		value=0.0,title='Velocity Shift: ',xsize=7,/return_events, $
		/float)
	
 ;
 ; line id table
 ;	
	line_base = widget_list(base2b,uvalue='LINE_LIST',xsize=20,ysize=10, $
			value = replicate(' ',50))
;
; Wavelength Advance
;
	basex = widget_base(base2b,/row)
	button = widget_button(basex,value='<----',uvalue='WBIGMINUS')
	button = widget_button(basex,value='<--',uvalue='WMINUS')
	label = widget_label(basex,value='  Wavelength  ')
	button = widget_button(basex,value='-->',uvalue='WPLUS')
	button = widget_button(basex,value='---->',uvalue='WBIGPLUS')
;
; zoom window
;
	basex = widget_base(base2,/col,/frame)
	basex1 = widget_base(basex,/row)
	label = widget_label(basex1,value='Zoom Factor')
	zoom_slider = widget_slider(basex1,uvalue='ZOOM_SLIDER',min=1,max=20, $
			title='',xsize=300,value=8)
	zoom_base = widget_draw(basex,uvalue='ZOOM_WINDOW',/button_events, $
			/motion,xsize=420,ysize=260)
;
;
; create widget
;
	widget_control,base,/realize
;
; get window ids
;
	widget_control,zoom_base,get_value=zoom_id
	widget_control,plot_base,get_value=plot_id
	widget_control,pic1_base,get_value=pic1_id
	widget_control,pic2_base,get_value=pic2_id
	widget_control,pic3_base,get_value=pic3_id
	widget_control,pic4_base,get_value=pic4_id

	n = !d.n_colors
	ctab = round(findgen(n-12)/(n-13)*255)
	tvlct,[plotpar.red,0,250,ctab], $
	      [plotpar.green,0,175,ctab], $
	      [plotpar.blue,0,175,ctab]
;
; save info in widget structure
;
	
	widget = {base:base, plot_select:plot_select, vmin_field:vmin_field, $
		vmax_field:vmax_field, wmin_field:wmin_field, $
		wmax_field:wmax_field, file_label:file_label, $
		v_ism_field: v_ism_field, line_base:line_base, $
		text_base:text_base, $
		zoom_slider:zoom_slider, plot_id:plot_id, zoom_id:zoom_id, $
		pic1_id:pic1_id, pic2_id:pic2_id, pic3_id:pic3_id, $
		pic4_id:pic4_id, pic_nx:700, pic_ny:70, zoom_nx:420, $
		zoom_ny:260, plot_nx:700, plot_ny: 420, directory:'', $
		rootname:'',wrange:[0.0d0,0.0],scale_type:'Log',yoffset:0.0}	
	fuse_analysis_plot
	tvlct,rsave,gsave,bsave,/get
	xmanager,'fuse_analysis',base,/no_block
	return
end
