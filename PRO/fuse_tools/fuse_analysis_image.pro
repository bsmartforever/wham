;------------------------------------------------------- FUSE_ANALYSIS_SCALEALL
;
; Routine to scale all eight images
;
	pro fuse_analysis_scaleall
	@fuse_analysis_common
	fuse_analysis_scale,sic1a,widget.scale_type
	fuse_analysis_scale,sic1b,widget.scale_type
	fuse_analysis_scale,sic2a,widget.scale_type
	fuse_analysis_scale,sic2b,widget.scale_type
	fuse_analysis_scale,lif1a,widget.scale_type
	fuse_analysis_scale,lif1b,widget.scale_type
	fuse_analysis_scale,lif2a,widget.scale_type
	fuse_analysis_scale,lif2b,widget.scale_type
return
end
;------------------------------------------------------- FUSE_ANALYSIS_SCALE
;
; routine to byte scale the image
;
	pro fuse_analysis_scale,data,scaletype
;
	if data.nx eq 0 then return
	dmax = (!d.n_colors-1)<255-12	;display maximum
	if data.fmax eq data.fmin then data.fmax = data.fmin+1
	case scaletype of
	    'Linear': data.scaled = bytscl(data.image,min=data.fmin, $
				max=data.fmax,top=dmax)+12b
	    'Log': 	begin
		tmin=data.fmax/1e4
		data.scaled = bytscl(alog((data.image-data.fmin)>tmin), $
					min=alog(tmin), $
					max=alog(data.fmax-data.fmin), $
					top=dmax)+12b
		end
	    'Sqrt': data.scaled = bytscl(sqrt((data.image-data.fmin)>0), $
				min=0,top = dmax, $
				max=sqrt(data.fmax-data.fmin))+12b

	    'Hist. Eq.': data.scaled = hist_equal(data.image,minv=data.fmin, $
					maxv=data.fmax, $
					top=dmax)+12b
	endcase
return
end
;------------------------------------------------------- FUSE_ANALYSIS_DISPALL
;
; Routine to display all images below the plot
;
	pro fuse_analysis_dispall,ps
@fuse_analysis_common
	if n_elements(ps) eq 0 then ps=0
;
; remove overlays
;
	fuse_analysis_rmoverlay,state
;
; SiC1
;
	fuse_analysis_imdisp,sic1a,spec_sic1a,widget,ps=ps, $
			id=widget.pic1_id,title='SiC 1',/erase
	fuse_analysis_imdisp,sic1b,spec_sic1b,widget,ps=ps, $
			id=widget.pic1_id,title='SiC 1'
;
; SiC2
;
	fuse_analysis_imdisp,sic2a,spec_sic2a,widget,ps=ps, $
			id=widget.pic2_id,title='SiC 2',/erase
	fuse_analysis_imdisp,sic2b,spec_sic2b,widget,ps=ps, $
			id=widget.pic2_id,title='SiC 2'
;
; Lif1
;
	fuse_analysis_imdisp,lif1a,spec_lif1a,widget,ps=ps, $
			id=widget.pic3_id,title='Lif 1',/erase
	fuse_analysis_imdisp,lif1b,spec_lif1b,widget,ps=ps, $
			id=widget.pic3_id,title='Lif 1'
;
; Lif2
;
	fuse_analysis_imdisp,lif2a,spec_lif2a,widget,ps=ps, $
			id=widget.pic4_id,title='Lif 2',/erase
	fuse_analysis_imdisp,lif2b,spec_lif2b,widget,ps=ps, $
			id=widget.pic4_id,title='Lif 2'
return
end	

;------------------------------------------------------- FUSE_ANALYSIS_IMDISP
;
; Routine to display portion of image below the plot
;
pro fuse_analysis_imdisp,data,spec,widget,ps=ps,id=id,title=title,erase=erase

    	case title of
		'SiC 1': ypos = 3
		'SiC 2': ypos = 2
		'Lif 1': ypos = 1
		'Lif 2': ypos = 0
	end
	pcolor = 4-ypos
	if ps eq 0 then begin
		!p.font = -1
		wset,id
		if keyword_set(erase) then erase
	    end else begin
		!p.font = 0
	end	
	    	
	    	
	if (data.nx eq 0) or (spec.ns eq 0) then goto,draw_border
;
; extract portion of image to use
;
	wrange= widget.wrange
	wave = congrid(spec.wave+spec.woffset,data.nx)  ;rescale to binned image
	good = where((wave gt wrange(0)) and (wave le wrange(1)),n)
	if n lt 2 then begin
		data.i1 = -1
		data.i2 = -1
		data.s1 = -1
		data.s2 = -1
		return
	end
	s1 = good(0)
	s2 = good(n-1)
	pic = data.scaled(s1:s2,*)
	w0 = wave(good(0))
	w1 = wave(good(n-1))
;
; determine where it goes
;
	i1 = ((w0-wrange(0))/(wrange(1)-wrange(0))*(0.98 - 0.1) + 0.1)* $
	  	  widget.pic_nx
	i2 = ((w1-wrange(0))/(wrange(1)-wrange(0))*(0.98 - 0.1) + 0.1)* $
	  	  widget.pic_nx
	i1 = round(i1)
	i2 = round(i2)
	nx = i2-i1+1
;
; scale region to correct size
;
	if ps eq 0 then begin
		pic = congrid(pic,nx,widget.pic_ny)
		tv,pic,i1,0
	    end else begin
	        if ps eq 1 then pic = bytscl(pic)
		if ps eq 2 then pic = 255b-bytscl(pic)
	    	tv,pic,float(i1)/widget.pic_nx*7.5,ypos*0.95,/inches, $
			xsize = float(nx)/widget.pic_nx*7.5,ysize=0.85
	end
;
; save region information
;
	data.i1 = i1	  
	data.i2 = i2
	data.s1 = s1
	data.s2 = s2
	 	
;
; draw border
;
draw_border:
	nx = widget.pic_nx
	ny = widget.pic_ny
	if ps eq 0 then begin
		plots,[0,nx-1,nx-1,0,0],[0,0,ny-1,ny-1,0],thick=3, $
				color=pcolor,/dev
		xyouts,10,ny*0.4,title,/dev,color=pcolor,size=2,charthick=2
	    end else begin
	    	set_viewport,0.0,1.0,0.0,1.0
		y1 = ypos*0.95/10.0
	        y2 = y1 + 0.85/10.0
		if ps lt 3 then color=0 else color=pcolor
	        plots,[0,1,1,0,0],[y1,y1,y2,y2,y1],/norm,color=color,thick=6
		xyouts,0.01,(y2+y1)/2,title,/norm,color=color,size=1.2	
	end
	return
end
;----------------------------------------------------- FUSE_ANALYSIS_CONTRAST
; Routine to change image contrast
;
pro fuse_analysis_contrast,group=group,modal=modal
@fuse_analysis_common
	if xregistered('fuse_analysis_contrast') then return
	
	mainbase = widget_base(/col,group=group,title='CONTRAST',modal=modal)
	button = widget_button(mainbase,value='DONE',uvalue='DONE')
	basea = widget_base(mainbase,/row)
	select = cw_bgroup(basea, $
			['All','Sic1a','Sic1b','Sic2a','Sic2b', $
			 'Lif1a','Lif1b','Lif2a','Lif2b'], $
			/col,set_value=0,uvalue='SELECT', $
			/exclusive)
	base1 = widget_base(basea,/col,/frame)
	buttons = ['Linear','Log','Sqrt','Hist. Eq.']
	good = where(buttons eq widget.scale_type)
	scale_select = cw_bgroup(base1,buttons,/col,/exclusive, $
			uvalue='SCALETYPE',set_value=good(0),/no_release)

	min_label = widget_label(base1,value='Image Min: '+strtrim(0.0,2), $
				/align_left)
	max_label = widget_label(base1,value='Image Max: '+strtrim(0.0,2), $
				/align_left)
	cmin_field = cw_field(base1,uvalue='CMIN_FIELD',xsize=14, $
			value=0.0,/float,title='MIN:',/return_events)
	cmax_field = cw_field(base1,uvalue='CMAX_FIELD',xsize=14, $
			value=0.0,/float,title='MAX:',/return_events)

	apply = widget_button(base1,value='APPLY',uvalue='APPLY')

	base = {select:select, min_label:min_label, max_label:max_label, $
		scale_select:scale_select, cmin_field:cmin_field, $
		cmax_field:cmax_field, apply:apply, change:0}
;
; create the widget
;
	widget_control,mainbase,/realize

	info = ptr_new({base:base})
	widget_control,mainbase,set_uvalue=info
	fuse_analysis_contrast_set,base

	xmanager,'fuse_analysis_contrast',mainbase,/no_block
	return
end
;----------------------------------------------- FUSE_ANALYSIS_CONTRAST_EVENT
;
; event driver for fuse_analysis_event
;
pro fuse_analysis_contrast_event,event
@fuse_analysis_common

    	widget_control,event.id,get_uvalue=uvalue
	widget_control,event.top,get_uvalue=info
	base = (*info).base
	widget_control,base.select,get_value=isel
	names = ['All','Sic1a','Sic1b','Sic2a','Sic2b', $
		       'Lif1a','Lif1b','Lif2a','Lif2b']	
	name = names(isel)
	apply = 0

	case uvalue of
	'DONE': begin
		fuse_analysis_plot
		if ptr_valid(info) then ptr_free,info
		widget_control,event.top,/destroy
		return
		end
	'SELECT': begin 
		if event.select eq 1 then begin
			fuse_analysis_contrast_set,base
			return
		end
		end
	'SCALETYPE': begin
		buttons = ['Linear','Log','Sqrt','Hist. Eq.']
		widget.scale_type = buttons(event.value)
		end
	'APPLY': apply = 1
	else:
	endcase
;
; update min and max
;

	if isel gt 0 then begin
		istat = execute('nx = '+names(isel)+'.nx')
		if nx gt 1 then begin
			widget_control,base.cmin_field,get_value=fmin
			widget_control,base.cmax_field,get_value=fmax
			istat = execute(name+'.fmin=fmin')
			istat = execute(name+'.fmax=fmax')	
		endif
	endif
	
	fuse_analysis_contrast_set,base

	if apply then begin
		if isel gt 0 then $
		     istat=execute('fuse_analysis_scale,'+name+ $
		     		   ',widget.scale_type') $
			else fuse_analysis_scaleall
		fuse_analysis_dispall
	endif

return
end
;------------------------------------------------ FUSE_ANALYSIS_CONTRAST_SET
;
; Routine to populate widget for given selection
;
pro fuse_analysis_contrast_set,base
@fuse_analysis_common
	widget_control,base.select,get_value=isel

	names = ['All','Sic1a','Sic1b','Sic2a','Sic2b', $
		       'Lif1a','Lif1b','Lif2a','Lif2b']	
	widget_control,base.min_label,set_value=' '
	widget_control,base.max_label,set_value=' '
	widget_control,base.cmin_field,sensitive=1
	widget_control,base.cmax_field,sensitive=1
	widget_control,base.apply,sensitive=1

	if isel gt 0 then begin
		istat = execute('nx = '+names(isel)+'.nx')
		if nx lt 2 then begin
			widget_control,base.cmin_field,set_value=0.0
			widget_control,base.cmax_field,set_value=0.0
			widget_control,base.cmin_field,sensitive=0
			widget_control,base.cmax_field,sensitive=0
			widget_control,base.apply,sensitive=0
			return
		end

		istat = execute('dmin = '+names(isel)+'.datamin')
		istat = execute('dmax = '+names(isel)+'.datamax')
		istat = execute('fmin = '+names(isel)+'.fmin')
		istat = execute('fmax = '+names(isel)+'.fmax')
		widget_control,base.min_label, $
				set_value='Datamin ='+strtrim(dmin,2)
		widget_control,base.max_label, $
				set_value='Datamax = '+strtrim(dmax,2)
		widget_control,base.cmin_field,set_value=fmin
		widget_control,base.cmax_field,set_value=fmax
	    end else begin
		widget_control,base.cmin_field,sensitive=0
		widget_control,base.cmax_field,sensitive=0
	end
	return
end

