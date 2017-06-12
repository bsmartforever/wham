;+
;				line_eqwidth
;
; Widget to measure the essential attributes of a spectral feature 
; including equivalent width
;
; CALLING SEQUENCE:
;	line_eqwidth,wave,flux,results
;
; INPUTS:
;	wave - wavelength vector of region containing the line
;	flux - flux vector of region containing the line
; OUTPUTS:
;	results - vector of results
;                  results(0)= the original value for W0. 
;                  results(1)= short wavelength limit to integration
;                  results(2)= extremum of spectral line
;                  results(3)= long wavelength limit to integration
;                  results(4)= radial velocity for short wavelength limit
;                  		to integration.
;                  results(5)= radial velocity for extremum of spectral line
;                  results(6)= radial velocity for the long wavelength limit
;                  		to integration.
;                  results(7)= flux at the short wavelength limit to the
;                  		integration.
;                  results(8)= flux at the spectral line extremum
;                  results(9)= flux at the long wavelength limit to the 
;                  		integration.
;                  results(10)=continuum flux
;                  results(11)=residual flux at extremum
;                  results(12)=equivalent width
;                  results(13)=total flux in the feature. 
;                  results(14)=flux weighted wavelength for feature.
;                  results(15)=flux weighted sigma, which for a 
;                  		gaussian profile can be related to the full
;                  		width at half maximum.
;                  results(16)=net flux in feature
;                  results(17)=flux weighted wavelength, with continuum
;           	       		not included
;                  results(18)=flux weighted sigma for net flux only.
;
; OPTIONAL KEYWORD INPUTS:
;
;	wrest - Rest wavelength for the spectral line
;	lineid - Text line identification for output plot annotation
;	title - Title (e.g. filename)
;	wmin - minimum wavelength to plot
;	wmax - maximum wavelength to plot
;	fmin - minimum flux to plot
;	fmax - maximum flux to plot
;
; NOTES:
;	Uses the same computation as the old IUE FEATURE.PRO
;
; HISTORY
;	version 1  D. Lindler   Feb. 2000
;-
;========================================================= EVENT DRIVER
pro line_eqwidth_event,event
	widget_control,event.top,get_uvalue=info
	if event.top eq event.id then return
	widget_control,event.id,get_uvalue=uvalue
	compute = 0	;recompute equivalent widget
	wset,(*info).plot_id
	case uvalue of 
	    'EXIT': widget_control,event.top,/destroy
	    'RANGE': line_eqw_plot,(*info)
	    'PLOT': begin
		if event.press gt 0 then begin
		    npos = (*info).npos
		    if npos lt 3 then begin
		    	v = convert_coord(event.x,event.y,/dev,/to_data)
			plots,v(0),v(1),color=3,psym=2
			(*info).wpos(npos) = v(0)
			(*info).fpos(npos) = v(1)
			npos = npos + 1
		 	(*info).npos = npos
		     endif
		     if npos eq 3 then compute = 1
		 endif				
		end
	    'OVER': begin
	    	(*info).npos = 0
		widget_control,(*info).ps,sensitive=0
		line_eqw_plot,(*info)
		widget_control,(*info).log2,set_v=' '
		end
	    'PS': line_eqw_ps,(*info)
	    'WREST': begin
		if (*info).npos eq 3 then compute = 1
		widget_control,(*info).message,set_v= $
			'Click on Left and Right Edges (continuum) and'+ $
	   		' center of Feature (Extremum)'
		end
	endcase	
	
	if compute eq 1 then begin	
		widget_control,(*info).rest_base,get_v=wrest
		(*info).wrest = wrest
		if wrest le 0 then begin
		    r = dialog_message(dialog_parent = event.top, $
			'You must enter a valid rest wavelength',/error)
		    widget_control,(*info).message,set_value= $
			    	'Enter a valid rest wavelength'
		    widget_control,(*info).rest_base,set_v=0.0
		    return
		end
		line_eqw_compute,info
		line_eqw_plot,(*info)
		widget_control,(*info).ps,sensitive=1
	end
return
end
;============================================================= LINE_EQW_COMPUTE
;
; Routine to compute equivalent width and other output results using
; calculations from IUE FEATURE routine.
;
pro line_eqw_compute,info
;
; perform computations using formula from IUE FEATURE/FMEAS routines
;
	w0 = (*info).wrest
	wavef = (*info).wave
	fluxf = (*info).flux
	sub = sort((*info).wpos)
	w = [w0,(*info).wpos(sub)]
	f = [0.0,(*info).fpos(sub)]
	tabinv,wavef,w,datapt
	cont = (wavef-w(1))*(f(3)-f(1))/(w(3)-w(1)) + f(1)
   	dflux=fluxf-cont
   	integ,wavef,cont ,datapt(1),datapt(3),fcont
   	integ,wavef,fluxf,datapt(1), datapt(3), ftot
   	integ,wavef,dflux, datapt(1), datapt(3), fnet
   	integ,wavef,fluxf*wavef,datapt(1),datapt(3),wtot
   	integ,wavef,dflux*wavef,datapt(1),datapt(3),wnet
   	wnet=wnet/fnet
   	wtot=wtot/ftot
   	dw=wavef-wtot
   	integ,wavef,fluxf*dw*dw,datapt(1),datapt(3),widtot
   	widtot=sqrt(abs(widtot/ftot))
   	dw=wavef-wnet
   	integ,wavef,dflux*dw*dw,datapt(1),datapt(3),widnet
   	widnet=sqrt(abs(widnet/fnet))
   	if fcont le 0. then ew=0. else $
        	        integ,wavef,(1.-fluxf/cont),datapt(1),datapt(3),ew
	rv = (w-w0) * 2.997925e5 / w0
	fcon = fcont / (w(3)-w(1))
	resi2 = f(2)/fcon
  	result = [w,rv(1:3),f(1:3),fcon,resi2,ew,ftot,wtot,widtot,fnet, $
				wnet,widnet]
	st = strtrim(string(result))
	widget_control,(*info).log2,set_v=[st(0:3),' ',st(4:6),' ',st(7:9), $
			' ',st(10:11),' ',st(12),' ',st(13:15),' ',st(16:18)]
	(*info).results = result
return
end	
;============================================================= LINE_EQW_PLOT
;
; Routine to create plot
;
pro line_eqw_plot,info
;
; get plot range
;
	widget_control,info.xmin_base,get_value=xmin
	widget_control,info.xmax_base,get_value=xmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax	
	wset,info.plot_id
	wave = info.wave
	flux = info.flux
	set_viewport
	plot,wave,flux,xrange=[xmin,xmax],xstyle=1,yrange=[ymin,ymax], $
		color=1,/nodata,xtitle='',ytitle='',title=''
;
; Fill line
;
	if info.npos eq 3 then begin
	    sub = sort(info.wpos)
	    wpos = info.wpos(sub)
	    fpos = info.fpos(sub)
	    wpos = wpos([0,2])
	    fpos = fpos([0,2])
	    linterp,wave,flux,wpos,fpos1
  	    good = where((wave gt wpos(0)) and (wave lt wpos(1)),ngood)
  	    if ngood gt 0 then begin
	  	polyfill,[wpos(0),wpos(0),wave(good),wpos(1),wpos(1)], $
		   	[fpos(0),fpos1(0),flux(good),fpos1(1),fpos(1)], $
			color=2
     	       end else begin
          	polyfill,[wpos(0),wpos(0),wpos(1),wpos(1)], $
                   	[fpos(0),fpos1(0),fpos1(1),fpos(1)],color=2
  	    end
	end
;
; overplot data
;
	oplot,wave,flux,color=1
	for i=0,info.npos-1 do plots,info.wpos(i),info.fpos(i),color=3,psym=2
	return
end
;============================================================= LINE_EQW_PS
;
; Routine to generate postscript plot
;
pro line_eqw_ps,info
;
; get filename
;
	file = dialog_pickfile(file='idl.ps',filter='*.ps', $
		dialog_parent=info.main,/write)
	if file eq '' then return	;no file selected
;
; set up postscript
;
	orig_device = !d.name
	set_plot,'ps'
	device,/land,file=file,bits=8
	set_viewport,0.3,0.95,0.1,0.9
;
; get plot range
;
	widget_control,info.xmin_base,get_value=xmin
	widget_control,info.xmax_base,get_value=xmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax	
	wave = info.wave
	flux = info.flux
	plot,wave,flux,xrange=[xmin,xmax],xstyle=1,yrange=[ymin,ymax], $
		/nodata,xtitle='',ytitle='',title=''
;
; Fill line
;
	if info.npos eq 3 then begin
	    sub = sort(info.wpos)
	    wpos = info.wpos(sub)
	    fpos = info.fpos(sub)
	    wpos = wpos([0,2])
	    fpos = fpos([0,2])
	    linterp,wave,flux,wpos,fpos1
  	    good = where((wave gt wpos(0)) and (wave lt wpos(1)),ngood)
  	    if ngood gt 0 then begin
	  	polyfill,[wpos(0),wpos(0),wave(good),wpos(1),wpos(1)], $
		   	[fpos(0),fpos1(0),flux(good),fpos1(1),fpos(1)], $
			color=200
     	       end else begin
          	polyfill,[wpos(0),wpos(0),wpos(1),wpos(1)], $
                   	[fpos(0),fpos1(0),fpos1(1),fpos(1)],color=200
  	    end
	end
;
; overplot data
;
	oplot,wave,flux
	for i=0,info.npos-1 do plots,info.wpos(i),info.fpos(i),psym=2
;
; annotate
;
	widget_control,info.log1,get_value=st1
	widget_control,info.log2,get_value=st2
	set_viewport,0,1,0,1
	!p.font=0
	widget_control,info.id_base,get_value=line_id
	xyouts,0.5,0.92,/norm,info.title+'   '+line_id(0),charsize=1.3,align=0.5
	for i=0,n_elements(st1)-1 do begin
		xyouts,0.05,0.85-0.03*i,st1(i),/norm
		xyouts,0.11,0.85-0.03*i,st2(i),/norm
	end
	device,/close
	set_plot,orig_device
	return
end

;============================================================= LINE_EQWIDTH
;
; Main routine
;
pro line_eqwidth,wave,flux,results,wrest=wrest,lineid=lineid,title=title, $
	group=group,modal=modal,wmin=wmin,wmax=wmax,fmin=fmin,fmax=fmax

	if n_elements(title) eq 0 then title='Equivalent Width Computation'
	if n_elements(wmin) eq 0 then wmin = min(wave)
	if n_elements(wmax) eq 0 then wmax = max(wave)
	if n_elements(fmin) eq 0 then fmin = min(flux)
	if n_elements(fmax) eq 0 then fmax = max(flux)
	if n_elements(wrest) eq 0 then wrest = 0.0
	if n_elements(lineid) eq 0 then lineid = ''
;
; set up widget layout
;
	main =  widget_base(/col,title=title,group=group,modal=modal, $
		/tracking)
	menu = widget_base(main,/row,/frame)
	exit = widget_button(menu,uvalue='EXIT',value='EXIT')
	ps = widget_button(menu,value='Write PS file',uvalue='PS')
	over = widget_button(menu,value='Start Over',uvalue='OVER')
	message = widget_label(main,xsize=800,value=' ',/align_left)
	base = widget_base(main,/row)
	basea = widget_base(base,/col)
        id_base = cw_field(basea,/row,uvalue='LINEID',value=lineid, $
                title='Line ID: ',xsize=20)
        rest_base = cw_field(basea,/row,uvalue='WREST',value=wrest, $
                title='Rest Wave.: ',xsize=13,/return_events,/float)
	
	log = widget_base(basea,/row)
	log1 = widget_text(log,xsize=8,ysize=25, $
		value=['Wlab','w1','w2','w3',' ','rv1','rv2','rv3', $
			' ','f1','f2','f3',' ','fcont','resi2',' ','ew-a', $
			' ','ftot','wtot','widtot',' ','fnet','wnet','widnet'])
	log2 = widget_text(log,xsize=15,ysize=25)
	plot = widget_draw(base,uvalue='PLOT',xsize=650,ysize=512, $
			/button_events,/motion)
	basex = widget_base(main,/row)
        xmin_base = cw_field(basex,/row,uvalue='RANGE',value=wmin, $
                title='W Min: ',xsize=13,/return_events,/float)
        xmax_base = cw_field(basex,/row,uvalue='RANGE',value=wmax, $
                title='W Max: ',xsize=13,/return_events,/float)
        ymin_base = cw_field(basex,/row,uvalue='RANGE',value=fmin, $
                title='F Min: ',xsize=13,/return_events,/float)
        ymax_base = cw_field(basex,/row,uvalue='RANGE',value=fmax, $
                title='F Max: ',xsize=13,/return_events,/float)
	widget_control,main,/realize
;
; create structure to hold data
;
	widget_control,plot,get_value=plot_id
	tvlct,rsave,gsave,bsave,/get
	info = ptr_new({wave:wave,flux:flux,results:fltarr(19),wrest:wrest, $
		title:title,main:main,ps:ps,over:over,message:message, $
		id_base:id_base,rest_base:rest_base,log1:log1,log2:log2, $
		plot_id:plot_id,xmin_base:xmin_base,xmax_base:xmax_base, $
		ymin_base:ymin_base,ymax_base:ymax_base,rsave:rsave, $
		gsave:gsave,bsave:bsave,npos:0,wpos:fltarr(3),fpos:fltarr(3)})
	widget_control,main,set_uvalue=info
;
; desensitize buttons
;
	widget_control,ps,sensitive=0
	if wrest eq 0 then msg='ENTER Rest Wavelength before continuing ' $
	   else msg = 'Click on Left and Right Edges (continuum) and'+ $
	   		' center of Feature (Extremum)'
	widget_control,message,set_value=msg
;
; load color table
;
	tvlct,[255,0,50,255],[255,0,200,0],[255,0,50,0]
	line_eqw_plot,(*info)
;
; create and run widget
;
	xmanager,'line_eqwidth',main
	results = (*info).results
	tvlct,rsave,gsave,bsave
	ptr_free,info
	return
end
