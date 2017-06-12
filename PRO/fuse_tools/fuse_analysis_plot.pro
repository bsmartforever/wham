pro fuse_analysis_plot,keep_yrange=keep_yrange,ps=ps
;+
;
@fuse_analysis_common
    if n_elements(ps) eq 0 then ps=0
;
; erase any overlay
;
    if ps eq 0 then fuse_analysis_rmoverlay,state
;
; determine plot to produce
;
    widget_control, widget.plot_select, get_value=isel
;
; get wavelength range
;
    widget_control, widget.wmin_field, get_value=wmin
    widget_control, widget.wmax_field, get_value=wmax
    wmin = double(wmin(0))
    wmax = double(wmax(0))   
;
; update data mins and maxs if range change
;
    wrange_change = 0
    if (wmax le wmin) then begin
    	if wmin eq widget.wrange(0)  then wmin = wmax - options.wrange $
	                             else wmax = wmin + options.wrange
        widget_control, widget.wmin_field, set_value=wmin
        widget_control, widget.wmax_field, set_value=wmax 
	wrange_change = 1  
    end
    if (abs(float(wmin)-widget.wrange(0)) gt 0.01) or  $
       (abs(float(wmax)-widget.wrange(1)) gt 0.01) then begin
	wrange_change = 1
	widget.wrange = [wmin,wmax]
    end
;
; update plot y-ranges
;
    if (wrange_change eq 1) and (not keyword_set(keep_yrange))  then begin
    	fuse_anal_pminmax,widget.wrange,spec_sic1a,spec_sic1b,min0,max0,np0
	fuse_anal_pminmax,widget.wrange,spec_sic2a,spec_sic2b,min1,max1,np1
    	fuse_anal_pminmax,widget.wrange,spec_lif1a,spec_lif1b,min2,max2,np2
    	fuse_anal_pminmax,widget.wrange,spec_lif2a,spec_lif2b,min3,max3,np3
	fmins = [min0,min1,min2,min3]
	fmaxs = [max0,max1,max2,max3]
	n = [np0,np1,np2,np3]
	if max(n) gt 0 then begin
		good = where(n gt 0)
		plotpar(0).datamin = min(fmins(good))
		plotpar(0).datamax = max(fmaxs(good))
	endif
	plotpar([1,2,3,4]).datamin=fmins
	plotpar([1,2,3,4]).datamax=fmaxs
	plotpar.ymin = 0
	plotpar.ymax = 0
    end 
    if ps eq 0 then begin
    	wset,widget.plot_id
    	set_viewport,0.1,0.98,0.05,0.80
      end else begin
;
; set up postscript plots
;
	file = dialog_pickfile(file='idl.ps',filter='*.ps',/write, $
			dialog_parent=widget.base)
	if file eq '' then return	;no file selected
	device = !d.name
        set_plot,'ps'

	if ps eq 3 then color=1 else color=0
	device,/port,xoff=0.5,yoff=0.45,xsize=7.5,ysize=10,/inches,bits=8, $
		color=color,file=file
	if ps eq 3 then tvlct,rsave,gsave,bsave
	set_viewport,0.1,0.98,0.4,0.82
    end	
    case isel of
	0: begin
		yoff = plotpar(0).yoffset
		yrange = [plotpar(0).ymin,plotpar(0).ymax]
		if max(abs(yrange)) eq 0.0 then begin
			yrange = [plotpar(0).datamin,plotpar(0).datamax]
			yrange(1) = yrange(1) + yoff*3
			yrange(0) = yrange(0) - (yrange(1)-yrange(0))*0.05
		end
		over = 0
		fuse_anal_plot1,widget,spec_sic1a,plotpar(1),linetab, $
				yrange=yrange,yoff=yoff*3,over=over,ps=ps
		fuse_anal_plot1,widget,spec_sic1b,plotpar(1),linetab, $
				over=over,yoff=yoff*3,ps=ps
		fuse_anal_plot1,widget,spec_sic2a,plotpar(2),linetab, $
				over=over,yoff=yoff*2,ps=ps
		fuse_anal_plot1,widget,spec_sic2b,plotpar(2),linetab, $
				over=over,yoff=yoff*2,ps=ps
		fuse_anal_plot1,widget,spec_lif1a,plotpar(3),linetab, $
				over=over,yoff=yoff,ps=ps
		fuse_anal_plot1,widget,spec_lif1b,plotpar(3),linetab, $
				over=over,yoff=yoff,ps=ps
		fuse_anal_plot1,widget,spec_lif2a,plotpar(4),linetab, $
				over=over,yoff=0.0,ps=ps
		fuse_anal_plot1,widget,spec_lif2b,plotpar(4),linetab, $
				over=over,yoff=0.0,ps=ps
	   endcase
	1: begin
		over = 0
		fuse_anal_plot1,widget,spec_sic1a,plotpar(1),over=over, $
			ps=ps,linetab
		fuse_anal_plot1,widget,spec_sic1b,plotpar(1),over=over, $
			ps=ps,linetab
	   end
	2: begin
		over = 0
		fuse_anal_plot1,widget,spec_sic2a,plotpar(2),linetab, $
			ps=ps,over=over
		fuse_anal_plot1,widget,spec_sic2b,plotpar(2),linetab,over=over
	   end
	3: begin
		over = 0
		fuse_anal_plot1,widget,spec_lif1a,plotpar(3),linetab, $
			ps=ps,over=over
		fuse_anal_plot1,widget,spec_lif1b,plotpar(3),linetab, $
			ps=ps,over=over
	   end
	4: begin
		over = 0
		fuse_anal_plot1,widget,spec_lif2a,plotpar(4),linetab, $
			ps=ps,over=over
		fuse_anal_plot1,widget,spec_lif2b,plotpar(4),linetab, $
			ps=ps,over=over
	   end
	5: erase
		
	6: fuse_anal_plot1,widget,sp1,plotpar(6),linetab, $
			ps=ps,title=sp1.title
	7: fuse_anal_plot1,widget,sp2,plotpar(7),linetab, $
			ps=ps,title=sp2.title
	8: fuse_anal_plot1,widget,sp3,plotpar(8),linetab, $
			ps=ps,title=sp3.title
	9: fuse_anal_plot1,widget,sp4,plotpar(9),linetab, $
			ps=ps,title=sp4.title
	else:
	endcase
        if wrange_change or (ps gt 0) then $
			fuse_analysis_dispall,ps ;display with new wrange
	if ps gt 0 then begin
		set_viewport,0,1,0,1
		color=5
		!p.font=0
		if ps eq 3 then color=5 else color=0
		xyouts,0.5,0.985,widget.rootname,/norm,charsize=1.4, $
			color=color,align=0.5
		device,/close
		set_plot,device
		!p.font=-1
	end
return
end
;----------------------------------------------------- FUSE_ANAL_PLOT1
; Routine to plot single spectrum
;
pro fuse_anal_plot1,widget,spec,plotpar,linetab,ps=ps, $
			yrange=yrange,over=over,yoffset=yoffset,title=title
;
;
	if spec.ns lt 1 then return
	if n_elements(yoffset) eq 0 then yoffset=0.0
	if not keyword_set(over) then begin
		if n_elements(yrange) eq 0 then begin
		    	yrange = [plotpar.ymin,plotpar.ymax]
			if (yrange(0) eq 0) and (yrange(1) eq 0) then begin
				yrange = [plotpar.datamin,plotpar.datamax]
				yrange(0) = yrange(0) - $
						(yrange(1)-yrange(0))*0.05
			end				
			if (yrange(0) eq 0) and (yrange(1) eq 0) then begin
				good = where((spec.wave gt widget.wrange(0)) $
				       and (spec.wave lt widget.wrange(1)),ng)
				if ng gt 0 then yrange = $
					[min(spec.flux(good)), $
					 max(spec.flux(good))]
				yrange(0) = yrange(0) - $
						(yrange(1)-yrange(0))*0.05
			endif
		end
		
		if (linetab.iselect ge 0) and $
			not keyword_set(title) then xstyle=9 else xstyle=1
		plot,widget.wrange,yrange,xstyle=xstyle,ystyle=1,/nodata, $
			xtitle='',ytitle='',title=title,color=10
;
; plot velocity axis
;
		if (linetab.iselect ge 0) and not keyword_set(title) then begin
			wline = linetab.wlines(linetab.iselect)
			vmin = round((widget.wrange(0)-wline)/wline*2.997925d5)
			vmax = round((widget.wrange(1)-wline)/wline*2.997925d5)
			axis,/xaxis,color=10,xrange=[vmin,vmax],xstyle=1
			widget_control,widget.vmin_field,set_value=vmin
			widget_control,widget.vmax_field,set_value=vmax
;
; overplot line locations
;
			widget_control,widget.v_ism_field,get_value=v_ism
			waves = linetab.wlines
			waves = waves + v_ism/2.997925d5*waves
			good = where((waves gt widget.wrange(0))and $
				(waves lt widget.wrange(1)),n)
			if n gt 0 then begin
			    for i=0,n-1 do begin
			    	w = waves(good(i))
				dely = !y.crange(1)-!y.crange(0)
				ymax = !y.crange(1)
				if good(i) eq linetab.iselect then color=10 $
							      else color=11
			    	plots,[w,w],ymax+dely*[0.045,0.065],/data, $
						color=color,thick=2
			    	plots,[w,w],ymax+dely*[0,-0.08],/data, $
						color=color,thick=2
				xyouts,w+(!x.crange(1)-!x.crange(0))/200.0, $
					ymax+dely*0.075,charsize=1.2, $
					linetab.ids(good(i)), $
					orient=90,color=color
			    endfor
			end
				
		end
	end
	
	oplot,spec.wave+spec.woffset,spec.flux+yoffset,psym=plotpar.psym, $
		color=plotpar.index,symsize=plotpar.psymsize, $
		line=plotpar.linetype,thick=plotpar.pthick,nsum=plotpar.nsum
	oplot,!x.crange,[0,0],line=2,color=10
	over = 1
return
end
;------------------------------------------------------ FUSE_ANAL_PMINMAX
; Routine to update flux min/max for the given wavelength range
;
pro fuse_anal_pminmax,wrange,spec1,spec2,fmin,fmax,n

	if spec1.ns gt 0 then begin
		good = where((spec1.wave ge wrange(0)) and $
				(spec1.wave le wrange(1)),n1)
		if n1 gt 0 then begin
			fmin1 = min(spec1.flux(good),max=mx)
			fmax1 = mx
		end
	end else n1 = 0
	
	if spec2.ns gt 0 then begin
		good = where((spec2.wave ge wrange(0)) and $
			(spec2.wave le wrange(1)),n2)
		if n2 gt 0 then begin
			fmin2 = min(spec2.flux(good),max=mx)
			fmax2 = mx
		end
	end else n2 = 0

	if (n1 gt 0) and (n2 gt 0) then begin
		fmin = fmin1<fmin2
		fmax = fmax1>fmax2
	  end else begin
	  	fmin = 0
		fmax = 0
	  	if (n1 gt 0) then begin fmin=fmin1 & fmax=fmax1 & end
	  	if (n2 gt 0) then begin fmin=fmin2 & fmax=fmax2 & end
	end
	n = n1+n2
return
end
;---------------------------------------------------------- SLWID_PLOTPAR
;
; Routine to adjust the plotting parameters
;
;
pro fuse_analysis_plotpar_event,event

@fuse_analysis_common
    	widget_control,event.id,get_uvalue=uvalue
	widget_control,event.top,get_uvalue=info
	base = (*info).base
	widget_control,base.plot_select,get_value=i	;current selection
	apply = 0
	
	case uvalue of
	'DONE': begin
		fuse_analysis_plot
		if ptr_valid(info) then ptr_free,info
		widget_control,event.top,/destroy
		return
		end
	'PLOT_SELECT': begin
		if event.select eq 1 then begin
			fuse_analysis_plotpar_set,base,plotpar
			return
		end
		end
	'RED': plotpar(i).red = event.value
	'BLUE': plotpar(i).blue = event.value
	'GREEN': plotpar(i).green = event.value
	'LINESTYLE': plotpar(i).linetype = event.index
	'PSYM': begin
		psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
		plotpar(i).psym = psyms(event.index)
		end
	'THICK_FIELD': if i eq 0 then plotpar(*).pthick = event.value
	'SIZE_FIELD': if i eq 0 then plotpar(*).psymsize = event.value
	'NSUM': if i eq 0 then plotpar(*).nsum = event.value
	'APPLY': apply = 1
		
	else:
	endcase

	widget_control,base.offset_field,get_value=v
	plotpar(i).yoffset = v	

	widget_control,base.thick_field,get_value=v
	plotpar(i).pthick = v
	widget_control,base.size_field,get_value=v
	plotpar(i).psymsize = v	
	widget_control,base.nsum_field,get_value=v
	plotpar(i).nsum = v	

	fuse_analysis_plotpar_set,base,plotpar
	if apply then fuse_analysis_plot
    return
end
;---------------------------------------------------- FUSE_ANALYSIS_PLOTPAR_SET
; Routine to set plot paramters for the selected plot
;
pro fuse_analysis_plotpar_set,base,plotpar

	widget_control,base.plot_select,get_value=isel
	par = plotpar(isel)
	widget_control,base.red_slider,set_value=par.red
	widget_control,base.green_slider,set_value=par.green
	widget_control,base.blue_slider,set_value=par.blue
	widget_control,base.linestyle,set_droplist_select=par.linetype
	psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
	good = where(psyms eq par.psym)
	widget_control,base.psym,set_droplist_select=good(0)
	widget_control,base.size_field,set_value=par.psymsize
	widget_control,base.thick_field,set_value=par.pthick
	widget_control,base.offset_field,set_value=par.yoffset
	widget_control,base.nsum_field,set_value=par.nsum
;
; update plot
;	
	wset,base.window_id
	erase
	tvlct,rr,gg,bb,/get
	rr(0) = [plotpar.red,0,250]
	gg(0) = [plotpar.green,0,175]
	bb(0) = [plotpar.blue,0,175]
	tvlct,rr,gg,bb
	ps = par.psym
	if ps eq 10 then ps=0
    	plots,indgen(11)*30,replicate(25,11),/dev,psym=ps, $
		color=isel,line=par.linetype,thick=par.pthick, $
		symsize = par.psymsize
;
	if isel eq 0 then sens=0 else sens=1
	widget_control,base.red_slider,sensitive=sens
	widget_control,base.green_slider,sensitive=sens
	widget_control,base.blue_slider,sensitive=sens
	widget_control,base.linestyle,sensitive=sens
	widget_control,base.psym,sensitive=sens
	widget_control,base.offset_field,sensitive=1-sens

return
end

pro fuse_analysis_plotpar,group=group

@fuse_analysis_common
;
; if already active, return
;
	if xregistered('fuse_analysis_plotpar') then return
;
; set up widget layout
;
	
	mainbase = widget_base(/col,title='Plot Parameter Adjustment', $
		group=group)
		
	button = widget_button(mainbase,uvalue='DONE',value='Done')
	basex = widget_base(mainbase,/row)
	plot_select = cw_bgroup(basex, $
			['All','Sic1','Sic2','Lif1','Lif2', $
			 'Coadd','SP1','SP2','SP3','SP4'], $
			/col,set_value=0,uvalue='PLOT_SELECT', $
			/exclusive)
	base = widget_base(basex,/col,/frame)
	offset_field = cw_field(base,uvalue='OFFSET_FIELD',xsize=12, $
			value=0.0, $
			/float,title='Y Plot Offsets ',/return_events)
	window = widget_draw(base,xsize=300,ysize=50)
;
; color sliders
;
	red_slider = widget_slider(base,min=0,max=255,/drag,uvalue='RED', $
			title='Red',value=0,xsize=280)
	green_slider = widget_slider(base,min=0,max=255,/drag,uvalue='GREEN', $
			title='Green',value=0,xsize=280)
	blue_slider = widget_slider(base,min=0,max=255,/drag,uvalue='BLUE', $
			title='Blue',value=0,xsize=280)
;
; linestype
;
	base1 = widget_base(base,/row)
	label = widget_label(base1,value='Linestyle',xsize=80,/align_left)
	linestyle = widget_droplist(base1,uvalue='LINESTYLE', $
			value=['Solid','Dotted','Short Dash','Dash Dot Dash', $
				'Dash Dot Dot Dot Dash','Long Dash'])
	widget_control,linestyle,set_droplist_select=0
;
; Psym
;
	base2 = widget_base(base,/row)
	label = widget_label(base2,value='Psym',xsize=80,/align_left)
	psym = widget_droplist(base2,uvalue='PSYM', $
		value =["Connected X's","Connected Squares", $
			"Connected Triangles","Connected Diamonds", $
			"Connected Asterisks","Connected Plus Signs", $
			"No Symbol","Plus Signs","Asterisks","Dots", $
			"Diamonds","Triangles","Sqaures","X's","Histogram"])
	psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
;
; Psymsize, pthick
;
	size_field = cw_field(base,uvalue='SIZE_FIELD',xsize=8, $
			value=0, $
			/float,title='Symsize       ',/return_events)

	thick_field = cw_field(base,uvalue='THICK_FIELD',xsize=8, $
			value=0, $
			/float,title='Line Thickness',/return_events)
	nsum_field = cw_field(base,uvalue='NSUM',value=1,/long, $
			xsize=5,title='NSUM',/return_events)
	button = widget_button(base,uvalue='APPLY',value='APPLY')
;
; create and run the widget
;

	widget_control,mainbase,/realize
	widget_control,window,get_value=window_id
	base = {window_id:window_id, red_slider:red_slider, $
			green_slider:green_slider,blue_slider:blue_slider, $
			linestyle:linestyle,psym:psym,size_field:size_field, $
			thick_field:thick_field,offset_field:offset_field, $
			plot_select:plot_select,nsum_field:nsum_field}
	info = ptr_new({base:base})
	widget_control,mainbase,set_uvalue=info
	fuse_analysis_plotpar_set,base,plotpar
	xmanager,'fuse_analysis_plotpar',mainbase,/no_block
	
	return
end

