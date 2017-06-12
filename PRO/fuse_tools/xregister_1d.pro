;+
;				xregister_1d
; Widget line plot tool.  If widget is already active, the routine will over
; plot the new x,y on the existing plot.
;
; CALLING SEQUENCE:
;	xregister_1d,x,y,yerr,mask,/init	;to load first spectrum
;	xregister_1d,x,y,yerr,mask		;to load subsequent spectra
;	xregister_1d,/process			;to begin processing
;
; INPUTS:
;	x - input x vector
;	y - input y vector
;
; OPTIONAL INPUTS:
;
;	yerr - error vector for y
;	mask - mask of good data values (1 for good data, 0 for bad),
;		default = all ones
;
; OPTIONAL INPUT KEYWORD PARAMETERS
;	weight - relative weight for spectrum when coadding. (default=1.0)
;	ptitle - main plot title
;	title - title of the plot or overplot vectors
;	xtitle - title for the xaxis
;	ytitle - title for the yaxis
;	xrange - initial xrange for the plot
;	yrange - initial yrange for the plot
;	group - group id of calling widget
;	modal - make the widget modal if called from another widget
;	/init - set when loading the first spectrum into the widget
;	/process - begin processing (after all spectra are loaded)
;
; OPTIONAL OUTPUT KEYWORD PARAMETERS:
;	wout - coadded wavelength vector
;	fout - coadded flux vector
;	errout - error vector for the coadded spectrum
;	woffset - vector of wavelength offsets (one per spectrum)
;	fscale - vector of scale factors for each spectrum (one per spectrum)
;
;	The output keywords are populated only on the call with the
;	input keyword /PROCESS supplied.
;
; INTERACTIVE INPUTS:
; 	Menu Bar:
;		EXIT - to exit the widget
;		Write/Postscript file - to generate a postscript file of the
;			current plot windows.
;		Write/FITS table - write the coadded spectrum to a binary fits
;			table with columns WAVE, FLUX, and ERROR
;		Unzoom All - unzoom the plot range to full scale
;		Unzoom - Unzoom the plot in a small increment
;		Zoom - begin plot zooming.  After pressing, center cursor
;			on one corner of the zoom region and press the
;			left mouse button.  Repeat on the opposite corner.
;			(Alternately, without using the zoom button, place the
;			cursor on one corner of the zoom region, push and
;			hold the center mouse button, drag the cursor to the
;			opposite corner and release the mouse button.
;		Linestyles - push to change the line styles and colors of
;			the plot.
;		Edit Mask - edit the data mask of the selected spectrum using
;			the LINE_EDIT widget.
;		Drag - press to manually adjust the wavelengths of the
;			selected reference spectrum by dragging its plot			
;		<--- - shift the plotted wavelength range to the left
;		---> - shift the plotted wavelength range to the right
;		
;	Spectrum Control Box
;
;		To the right of the main plot is a spectrum control box.
;		The first column contains Xcorr buttons (1 per spectrum).
;		The second column contains boxes showing the colors plotted
;		for each spectrum.  This boxes can be used to select the
;		spectrum as the reference spectrum by clicking the mouse
;		within the box.  
;		The third column contains buttons controlling whether or
;		not the spectrum is to be plotted or used when coadding.
;		The last two columns contain text fields giving the title
;		of each spectrum and the current wavelength offset.  
;		Both of these fields can be manually edited if desired.
;
;		To select a reference spectrum. Push the appropriate
;		selection box containing its plot color.  Once selected
;		the background of the box will change to yellow and the box
;		will be highlighted by a black border.  To unselect, push the
;		box again or select a different spectrum.  The reference
;		spectrum decides which spectrum is editted with the <Edit Mask>
;		button and or which spectrum is dragged with the <Drag>
;		button is selected.  
;
;		When a spectrum is selected it's plotted spectrum will be
;		highlighted.  You can manually adjust the wavelengths for
;		the selected spectrum pushing the <Drag> button on the menu
;		bar, placing the cursor in the main plot window, pushing down 
;		the left mouse button, draging the spectrum, and releasing 
;		the mouse button.
;
;		Alternately, you can use cross-correlation to register two
;		spectra.  Zoom the plot to the spectral range that you want
;		to cross-correlate. Select a reference spectrum with the 
;		selection box containing its color.  Next press the
;		<Xcorr> button for any of the other spectra that overlay the
;		reference spectrum in the currently plotted range.  The routine
;		will cross-correlate it with the reference spectrum and 
;		adjust the wavelength offset accordingly.  If the computed 
;		offset is more than 10% of the wavelength range plotted, it
;		will not be applied.  In this case, you must increase the
;		wavelength range plotted, or adjust the offset manually until
;		the offset is smaller.  You can then recompute the offset
;		using cross-correlation.
;
;	Coaddition control box.
;
;		Below the spectrum control box are spectral coaddition controls.
;		To coadd the spectra, push the <Coadd Spectra> button.
;		In the regions of overlay, coaddition will be done using
;		linear interpolation or nearest neighbor.  Use the
;		control buttons below the coadd spectra to make you choice
;		before pushing the <Coadd Spectra> button.  You can also
;		select the spectra weighting method between equal weights
;		for all spectra and weights computed from the input error
;		vectors.  Only spectra with there selection button pressed
;		(the ones that are plotted) are included in the coadd.
;
;		After you coadd the spectra, the coadded spectrum will be
;		plotted as dark black.  To remove it from the plot.  You
;		must press <Delete Coadded Spectrum>.
;
;		Once the spectra are coadded, you can save the results by
;		using the <Write FITS table> option of the <File> button on
;		the top menu bar.
;
; 	Plot Control Fields:
;
; 		Any of these fields can be manually editted by clicking the
;		cursor within the field.
;		X Min: X minimum of the plots
;		X Max: X maximum of the plots
;		Y Min: Y minimum of the plot above the text field
;		Y Max: Y maximum of the plot above the text field
;		N Smooth: Mean filter width applied to the data before
;			plotting.
;		Y Offset: offset between the spectra plotted in the upper
;			plot window.
;
; HISTORY:
;	version 1  D. Lindler  Apr 2001
;	Dec 2003, Lindler, Increased size of text fields
;-
;----------------------------------------------------------------------------  



;====================================================== XREGISTER_1D_EVENT
;
; Main event driver for xregister_1d
;
pro xregister_1d_event,event
	common xregister_1d_common,info,xarray,yarray,error_array,mask_array, $
		r,g,b,rsave,gsave,bsave,xcoadd,ycoadd,errcoadd,xsel,ysel
	tvlct,r,g,b
	set_xy
	set_viewport
	widget_control,event.id,get_uvalue=uval
	uvalue = gettok(uval,'_')
	number = fix(uval)

	case uvalue of

	'EXIT': begin
		xarray = 0
		yarray = 0
		mask_array = 0
		error_array = 0
		wdelete,info.pixid1
		wdelete,info.pixid2
		wdelete,info.pixid3
		tvlct,rsave,gsave,bsave
		for i=0,9 do begin
			widget_control,info.woff_base(i),get_value=v
			info.woffset(i) = v
		end
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
			device,/port,xoff=1,yoff=1,xsize=7,ysize=9,bits=8, $
						/color,/inches,file=file
			!p.font = 0
			xregister_1d_plot,info,xcoadd,ycoadd,errcoadd, $
				xarray,yarray,error_array,mask_array,/ps
			xregister_1d_annotate,info,/ps
			device,/close
			set_plot,orig_device
			!p.font = -1
			end
		'Write.FITS Table': begin
			if n_elements(xarray) lt 2 then return

			file = dialog_pickfile(file='coadd.fits', $
						filter='*.fits',/write)
			if file eq '' then return	;no file selected
			for i=0,info.n-1 do begin
			    widget_control,info.label_base(i),get_value=v
			    sxaddpar,h,'SPEC'+strtrim(i,2),v(0)
			end
			for i=0,info.n-1 do begin
			    widget_control,info.woff_base(i),get_value=v
			    sxaddpar,h,'WOFF'+strtrim(i,2),v(0)
			end
			for i=0,info.n-1 do begin
			    sxaddpar,h,'SCALE'+strtrim(i,2),info.scale(i)
			end
			sxaddpar,h,'TOTALW',info.totweight,'Total of Weights'	
			a = {wave:xcoadd,flux:ycoadd,error:errcoadd}
			mwrfits,a,file,h,/create
			end
		endcase
		end
	'ZOOM': info.state = 'ZOOM1'
	'UNZOOMALL': begin
		yoff = yarray*0
		i1 = 0
		widget_control,info.yoff_base,get_value=yoffval
		good = bytarr(n_elements(yarray))
		for i=0,info.n-1 do begin
			i2 = i1+info.ns(i)-1
			good(i1:i2) = (yarray(i1:i2) gt info.min_val(i)) and $
				      (yarray(i1:i2) lt info.max_val(i))
			yoff(i1:i2) = i*float(yoffval)
			i1 = i2+1
		end
		widget_control,info.xmin_base,set_value = min(xarray)
		widget_control,info.xmax_base,set_value = max(xarray)
		good = where(good)
		ygood = yarray(good) + yoff(good)
		widget_control,info.ymin_base,set_value = min(ygood)
		widget_control,info.ymax_base,set_value = max(ygood)
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
					error_array,mask_array
		end
	'UNZOOM': begin
		yoff = yarray*0
		i1 = 0
		widget_control,info.yoff_base,get_value=yoffval
		good = bytarr(n_elements(yarray))
		for i=0,info.n-1 do begin
			i2 = i1+info.ns(i)-1
			good(i1:i2) = (yarray(i1:i2) gt info.min_val(i)) and $
				      (yarray(i1:i2) lt info.max_val(i))
			yoff(i1:i2) = i*float(yoffval)
			i1 = i2+1
		end
		good = where(good)
		ygood = yarray(good) + yoff(good)
		widget_control,info.xmin_base,get_value = xmin
		widget_control,info.xmax_base,get_value = xmax
		widget_control,info.ymin_base,get_value = ymin
		widget_control,info.ymax_base,get_value = ymax
		xrange = (xmax-xmin)
		yrange = (ymax-ymin)
		xmin = (xmin - xrange*0.25) > min(xarray) < xmin
		xmax = (xmax + xrange*0.25) < max(xarray) > xmax
		ymin = (ymin - yrange*0.25) > min(ygood) < ymin
		ymax = (ymax + yrange*0.25) < max(ygood) > ymax
		widget_control,info.xmin_base,set_value = xmin
		widget_control,info.xmax_base,set_value = xmax
		widget_control,info.ymin_base,set_value = ymin
		widget_control,info.ymax_base,set_value = ymax
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
					error_array,mask_array
		end
	'DRAG': begin
		if info.plot_select lt 0 then begin
		    istat = dialog_message('No Reference Spectrum Selected', $
				/error,dialog_parent=event.top)
		    return
		end
		
		i = info.plot_select
	        i1 = 0
		if i gt 0 then i1 = round(total(info.ns(0:i-1)))
		i2 = i1 + info.ns(i)-1
		xsel = xarray(i1:i2)
		ysel = yarray(i1:i2)*info.scale(i)
		wrange = !y.crange(1) - !y.crange(0)
		wmin = !y.crange(0) - wrange
		wmax = !y.crange(1) + wrange
		good = where((xsel gt wmin) and (xsel lt wmax),ngood)
		if ngood gt 0 then begin
			xsel = xsel(good)
			ysel = ysel(good)
		end
		widget_control,info.smooth_base,get_value=nsmooth
		if nsmooth gt 1 then ysel = smooth(ysel,nsmooth)
		
		info.state = 'DRAG1'
		end
	'SCALE': begin
		if info.plot_select lt 0 then begin
		    istat = dialog_message('No Reference Spectrum Selected', $
				/error,dialog_parent=event.top)
		    return
		end
		info.state = 'SCALE1'
		end

	'WPLUS': begin
		widget_control,info.xmin_base,get_value = xmin
		widget_control,info.xmax_base,get_value = xmax
		xrange = (xmax-xmin)
		widget_control,info.xmin_base,set_value = xmax-xrange*0.1
		widget_control,info.xmax_base,set_value = xmax+xrange*0.9
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
					error_array,mask_array
		end
	
	'WMINUS': begin
		widget_control,info.xmin_base,get_value = xmin
		widget_control,info.xmax_base,get_value = xmax
		xrange = (xmax-xmin)
		widget_control,info.xmin_base,set_value = xmin-xrange*0.9
		widget_control,info.xmax_base,set_value = xmin+xrange*0.1
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
					error_array,mask_array
		end

	'RANGE': xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
				error_array,mask_array
	'LINESTYLES': xregister_1d_plotpar,group=event.top
			
	'PLOT1': begin
		xd = event.x	;device coordinates
		yd = event.y
		wset,info.plot2_id
		device,copy=[0,0,info.xsize1,info.ysize2,0,0,info.pixid2]
		plots,[xd,xd],[0,info.ysize2],color=1,/dev
		wset,info.plot1_id
		device,copy=[0,0,info.xsize1,info.ysize1,0,0,info.pixid1]
		
		if (info.state eq 'X/Y') or (info.state eq 'ZOOM1') then begin
		        plots,[xd,xd],[0,info.ysize1],color=1,/dev
		        plots,[0,info.xsize1],[yd,yd],color=1,/dev
			if event.press gt 1 then begin
			    	!x = info.xsave
				!y = info.ysave
				info.x1 = xd
				info.y1 = yd
				info.state = 'ZOOM2'
			end
		end

		if (info.state eq 'DRAG1') then begin
		    if event.press gt 0 then begin
		    	!x = info.xsave
			!y = info.ysave
			info.x1 = xd
			info.y1 = yd
			if (event.press eq 1) and (info.plot_select ge 0) $
						then info.state = 'DRAG2' 
		    endif
		    return
		end

		
		if (info.state eq 'DRAG2') then begin
		    dx = xd - info.x1
		    info.x1 = xd
		    v2 = convert_coord(xd,yd,/dev,/to_data)
		    v1 = convert_coord(xd-dx,yd,/dev,/to_data)
		    widget_control,info.woff_base(info.plot_select), $
		    		get_value = dw
		    dw = dw + v2(0) - v1(0)
		    widget_control,info.woff_base(info.plot_select), $
		    		    set_value = dw
		    
		    i = info.plot_select
		    widget_control,info.yoff_base,get_value=yoff
		    oplot,xsel+dw,ysel+yoff*i, $
				color=i+2,psym=info.psym(i), $
				line=info.linestyle(i),thick=info.thick(i), $
				symsize=info.symsize(i),nsum=info.nsum(i)

		    if event.release gt 0 then begin
		    	xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray, $
						yarray,error_array,mask_array
		    end
		end								

		if (info.state eq 'SCALE1') then begin
		    plots,[0,info.xsize1],[yd,yd],color=1,/dev
		    if (event.press gt 0) then begin
			info.x1 = xd
			info.y1 = yd
			info.state = 'SCALE2' 
		    endif
		    return
		end

		if (info.state eq 'SCALE2') then begin
		    plots,[0,info.xsize1],[info.y1,info.y1],color=1,/dev
		    plots,[0,info.xsize1],[yd,yd],color=1,/dev


		    if event.release gt 0 then begin
			!x = info.xsave
			!y = info.ysave
		    	v2 = convert_coord(xd,yd,/dev,/to_data)
		    	v1 = convert_coord(xd,info.y1,/dev,/to_data)
		    	factor = v2(1)/v1(1)
			i = info.plot_select
		    	info.scale(i) = info.scale(i)*factor
			widget_control,info.weight_base(i),get_value=w
			widget_control,info.weight_base(i),set_value=w(0)/factor
		    	xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray, $
						yarray,error_array,mask_array
		    end
		end								

		if info.state eq 'ZOOM1' and (event.press gt 0) then begin
		   	info.x1 = xd
			info.y1 = yd
			info.state = 'ZOOM2'
			return
		endif

		if (info.state eq 'ZOOM2') then begin
		    x = [info.x1,xd]
		    y = [info.y1,yd]
		    plots,[x(0),x(1),x(1),x(0),x(0)], $
			  [y(0),y(0),y(1),y(1),y(0)],/dev,color=1
		    
		    if (event.release gt 1) or (event.press eq 1) then begin
			!x = info.xsave
			!y = info.ysave
			v = convert_coord(x,y,/dev,/to_data)
			x = v(0,*)
			y = v(1,*)		    
		    	widget_control,info.xmin_base,set_value = min(x)
			widget_control,info.xmax_base,set_value = max(x)
			widget_control,info.ymin_base,set_value = min(y)
			widget_control,info.ymax_base,set_value = max(y)
			xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray, $
						yarray,error_array,mask_array
			xregister_1d_annotate,info
			return
		    end
		end	    
			
		end
	'YOFF': xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
						error_array,mask_array
	'WOFF': xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
						error_array,mask_array
	'DRAW': begin
		if event.press eq 0 then return
		if info.plot_select eq number then info.plot_select = -1 $
					      else info.plot_select = number
		xregister_1d_annotate,info
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
						error_array,mask_array
		end
	'SELECT': begin
		info.use(number) = event.select
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
						error_array,mask_array
		end
	'EDIT': begin
		n = info.plot_select
		if n lt 0 then return
		widget_control,info.xmin_base,get_value=xmin
		widget_control,info.xmax_base,get_value=xmax
		widget_control,info.ymin_base,get_value=ymin
		widget_control,info.ymax_base,get_value=ymax
		widget_control,info.label_base(n),get_value=title
		if n ne 0 then i1 = round(total(info.ns(0:n-1))) $
			  else i1 = 0
		i2 = i1 + info.ns(n)-1
		mask = mask_array(i1:i2)
		line_edit,xarray(i1:i2),yarray(i1:i2),mask,title=title(0), $
			xrange=[xmin,xmax],yrange=[ymin,ymax],group=event.top, $
			/modal
		mask_array(i1:i2) = mask
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
						error_array,mask_array
		end 
	'XCORR': begin
		widget_control,/hourglass
		xregister_1d_corr,info,number,xarray,yarray,mask_array,flag
		if flag gt 0 then xregister_1d_plot,info,xcoadd,ycoadd, $
					errcoadd,xarray,yarray, $
					error_array,mask_array
		end
	'INTERP': info.interp = 1
	'NEAREST': info.interp = 0
	'WEIGHT': info.weight = 'WEIGHT'
	'SWEIGHT': info.weight = 'SWEIGHT'
	'NOWEIGHT': info.weight = 'NOWEIGHT'
	'COADD': begin
		widget_control,/hourglass
		xregister_1d_coadd,info,xarray,yarray,error_array,mask_array, $
			xcoadd,ycoadd,errcoadd
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
			error_array,mask_array
		end
	'DELETECOADD': begin
		xcoadd = 0
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
			error_array,mask_array
		end
	'NSMOOTH': xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
			error_array,mask_array
	else:
	endcase
	return
	end
;============================================================ XREGISTER_1D_PLOT
;
; Routine to generate the plot
;
pro xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
			error_array,mask_array,ps=ps


	widget_control,info.xmin_base,get_value=xmin
	widget_control,info.xmax_base,get_value=xmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax
	widget_control,info.yoff_base,get_value=yoff
	widget_control,info.ymin_base2,get_value=ymin2
	widget_control,info.ymax_base2,get_value=ymax2
	widget_control,info.smooth_base,get_value=nsmooth
;
; error plot
;
	if not keyword_set(ps) then begin
		wset,info.plot2_id
		set_viewport,0.1,0.95,0.1,0.95
		noerase = 0
	    end else begin
	    	set_viewport,0.1,0.95,0.25,0.45
		noerase = 1
	end
	
	plot,[xmin,xmax],[0,ymax/5],/nodata,ytitle='Error', $
		xstyle=1,ystyle=1,color=1,xrange=[xmin,xmax], $
		yrange =[ymin2,ymax2]
	i1 = 0
	for i=0,info.n-1 do begin
		i2 = i1+info.ns(i)-1
		if info.use(i) eq 0 then goto,nexti2
		x = xarray(i1:i2)
		y = error_array(i1:i2)*info.scale(i)
		mask = mask_array(i1:i2)
		good = where(mask eq 1,ngood)
		if ngood lt 2 then goto,nexti2
		x = x(good)
		y = y(good)
		if nsmooth gt 1 then y = smooth(y,nsmooth)
		widget_control,info.woff_base(i),get_value=xoff
		thick = info.thick(i)
	
		oplot,x+xoff(0),y, $
			color=i+2,psym=info.psym(i), $
			line=info.linestyle(i),thick=thick, $
			symsize=info.symsize(i),nsum=info.nsum(i), $
			min_val = info.min_val(i)+yoff*i, $
			max_val = info.max_val(i)+yoff*i
nexti2:
		i1  = i2+1
	end
	if n_elements(xcoadd) gt 1 then begin
		y = errcoadd
		if nsmooth gt 1 then y=smooth(y,nsmooth)
		oplot,xcoadd,y,color=1,thick=2
	end

	if not keyword_set(ps) then begin
		wset,info.pixid2
		device,copy=[0,0,info.xsize1,info.ysize2,0,0,info.plot2_id]	
	end
;
; draw window 1
;
	if not keyword_set(ps) then begin
		wset,info.plot1_id
		set_viewport,0.1,0.95,0.1,0.9
		iselect = info.plot_select
	    end else begin
		iselect = -1
		set_viewport,0.1,0.95,0.5,0.9
	end
	plot,[xmin,xmax],[ymin,ymax],/nodata,ytitle=info.ytitle, $
		xtitle=info.xtitle,title=info.ptitle,xstyle=1,ystyle=1, $
		color=1,xrange=[xmin,xmax], $
		yrange =[ymin,ymax],noerase = noerase
	i1 = 0
	for i=0,info.n-1 do begin
		i2 = i1+info.ns(i)-1
		if info.use(i) eq 0 then goto,nexti
		x = xarray(i1:i2)
		y = yarray(i1:i2)*info.scale(i)
		if nsmooth gt 1 then y = smooth(y,nsmooth)
		mask = mask_array(i1:i2)
		good = where(mask eq 1,ngood)
		if ngood lt 2 then goto,nexti
		x = x(good)
		y = y(good)
		widget_control,info.woff_base(i),get_value=xoff
		thick = info.thick(i)

		if i eq iselect then begin
			thick = (thick+1)>2
			wset,info.pixid3
			oplot,x+xoff(0),y+yoff*i, $
				color=i+2,psym=info.psym(i), $
				line=info.linestyle(i),thick=info.thick(i), $
				symsize=info.symsize(i),nsum=info.nsum(i), $
				min_val = info.min_val(i)+yoff*i, $
				max_val = info.max_val(i)+yoff*i
			wset,info.plot1_id
		end
		
		oplot,x+xoff(0),y+yoff*i, $
			color=i+2,psym=info.psym(i), $
			line=info.linestyle(i),thick=thick, $
			symsize=info.symsize(i),nsum=info.nsum(i), $
			min_val = info.min_val(i)+yoff*i, $
			max_val = info.max_val(i)+yoff*i
nexti:			
		i1  = i2+1
	end
	if n_elements(xcoadd) gt 1 then begin
		flux = ycoadd
		if nsmooth gt 1 then flux=smooth(flux,nsmooth)
		oplot,xcoadd,flux,color=1,thick=2
	end
	
	info.xsave = !x
	info.ysave = !y
	xrange = !x.crange
	yrange = !y.crange
	widget_control,info.xmin_base,set_value=xrange(0)
	widget_control,info.xmax_base,set_value=xrange(1)
	widget_control,info.ymin_base,set_value=yrange(0)
	widget_control,info.ymax_base,set_value=yrange(1)
	if not keyword_set(ps) then begin
		wset,info.pixid1
		device,copy=[0,0,info.xsize1,info.ysize1,0,0,info.plot1_id]	
	end
	info.state = 'X/Y'
	
return
end

;======================================================= XREGISTER_1D_ANNOTATE
;
; Routine to fill annotation box
;
pro xregister_1d_annotate,info,ps=ps
	
	if keyword_set(ps) then begin
	    set_viewport,0,1.0,0,1.0
	    y1 = 0.2
	    dy = 0.027
	    x = [0.05,0.15,0.2]
	    charsize = 1.0
	    for i=0,info.n-1 do begin
	        if i eq 5 then begin
			x = x + 0.5
			y1 = 0.2
		end
		psym = info.psym(i)
		if psym eq 10 then psym=0
		plots,x(0:1),[y1,y1],psym=psym,color=i+2, $
				line=info.linestyle(i),thick=info.thick(i), $
				symsize=info.symsize(i),/norm
		widget_control,info.label_base(i),get_value=title
		xyouts,x(2),y1-dy*0.25,title(0),/norm,color=i+2, $
			charsize=charsize
		y1 = y1 - dy
	    end
	  end else begin
	    for i=0,info.n-1 do begin
	        wset,info.window_ids(i)
		erase

		if i eq info.plot_select then begin
			tv,replicate(12,40,25)
			plots,[1,38,38,1,1],[1,1,23,23,1],/dev,color=1 $
						,thick=4
		end
			
		psym = info.psym(i)
		if psym eq 10 then psym=0
		plots,[10,30],[12,12],psym=psym,color=i+2,$
				line=info.linestyle(i),thick=info.thick(i)>2, $
				symsize=info.symsize(i),/dev
	    end
	end
	return
	end
;============================================================ XREGISTER_1D_CORR
;
; Routine to cross correlate spectra
;
pro xregister_1d_corr,info,number,xarray,yarray,mask_array,flag

;
; determine region to cross correlate
;
	flag = 0			;set to one if successful
	xrange = info.xsave.crange
;
; get reference spectrum
;
	n = info.plot_select
	if n lt 0 then begin
		istat = dialog_message('No reference spectrum selected', $
				/error,dialog_parent=info.main)
		return
	end
	if (n lt 0) or (n eq number) then return
	if n ne 0 then i1 = round(total(info.ns(0:n-1))) $
		  else i1 = 0
	i2 = i1 + info.ns(n)-1
	x1 = xarray(i1:i2)
	y1 = yarray(i1:i2)
	mask1 = mask_array(i1:i2)
	widget_control,info.woff_base(n),get_value=woff
	x1 = x1 + woff
	good = where((mask1 ne 0) and (x1 ge xrange(0)) and $
		     (x1 le xrange(1)),ngood)
	if ngood lt 10 then return
	x1 = x1(good)
	y1 = y1(good)
;
; get spectrum to cross correlate to reference spectrum
;
	n = number
	if n ne 0 then i1 = round(total(info.ns(0:n-1))) $
		  else i1 = 0
	i2 = i1 + info.ns(n)-1
	x2 = xarray(i1:i2)
	y2 = yarray(i1:i2)
	mask2 = mask_array(i1:i2)
	widget_control,info.woff_base(n),get_value=woff
	x2 = x2 + woff
	good = where((mask2 ne 0) and (x2 ge xrange(0)) and $
		     (x2 le xrange(1)),ngood)
	if ngood lt 10 then return
	x2 = x2(good)
	y2 = y2(good)
;
; Extract overlap region
;
	wmin = min(x1)>min(x2)
	wmax = max(x1)<max(x2)
	good = where((x1 ge wmin) and (x1 le wmax),ngood1)
	if ngood1 lt 10 then return
	x1 = x1(good)
	y1 = y1(good)
	good = where((x2 ge wmin) and (x2 le wmax),ngood2)
	if ngood2 lt 10 then return
	x2 = x2(good)
	y2 = y2(good)
;
; Interpolate spectra to a linear wavelength scale
;
	n = ngood1>ngood2
	delw = (wmax-wmin)/(n-1)
	wave = findgen(n)*delw + wmin
	f1 = interpol(y1,x1,wave)
	f2 = interpol(y2,x2,wave)
;
; cross correlate
;
	cross_correlate,f1,f2,offset,width=n/5
	if offset eq 0.0 then return
;
; convert offset to wavelength units
;
	offset = offset*delw
	flag = 1
	widget_control,info.woff_base(number),set_value=woff+offset
return
end
;=========================================================== XREGISTER_1D_COADD
;
; Routine to coadd spectra
;
pro xregister_1d_coadd,info,xarray,yarray,error_array,mask_array, $
			xcoadd,ycoadd,errcoadd
;
; create 2-D arrays holding data
;
	good = where(info.use(0:info.n-1) eq 1,n)
	if n eq 0 then begin
		xcoadd = 0
		return
	end

	nmax = max(info.ns(good))
	xin = dblarr(nmax,n)
	yin = dblarr(nmax,n)
	errin = dblarr(nmax,n)
	epsin = intarr(nmax,n)
	weights = fltarr(n)
	i1 = 0
	iout = 0
	for i=0,info.n-1 do begin
		i2 = i1+info.ns(i)-1
		if info.use(i) eq 0 then goto,nexti
		widget_control,info.woff_base(i),get_value=woff
		x = xarray(i1:i2) + woff
		y = yarray(i1:i2) * info.scale(i)
		mask = mask_array(i1:i2)
		err = error_array(i1:i2) * info.scale(i)

		if x(1) lt x(0) then begin
			x = reverse(x)
			y = reverse(y)
			err = reverse(err)
			mask = reverse(mask)
		end
		widget_control,info.weight_base(i),get_value=w
		weights(iout)=w(0)

		xin(0,iout) = x
		yin(0,iout) = y
		errin(0,iout) = err
		epsin(0,iout) = (mask eq 0)*300
		iout = iout + 1
nexti:
		i1 = i2+1
	end
;
; compute weighting
;
	case info.weight of
	'NOWEIGHT': weight = 0
	'WEIGHT': begin
		widget_control,info.smooth_base,get_value=nsmooth
		var = double(errin^2)
		good = where(var gt 0)
		minvar = min(var(good))
		bad = where(var le 0,nbad)
		if nbad gt 0 then var(bad) = minvar
		if nsmooth gt 1 then for i=0,n_elements(var(0,*))-1 do $
					var(*,i) = smooth(var(*,i),nsmooth)
		weight = 1.0/var			
		end
	'SWEIGHT': weight = weights
	end
	if info.weight eq 'SWEIGHT' then info.totweight = total(weights) $
				    else info.totweight = float(n)
	hrs_merge,xin,yin,epsin,errin,xcoadd,ycoadd,eps,errcoadd, $
		weight = weight,/ignore
return
end

;========================================================= XREGISTER_1D_PLOTPAR
;
; Routine to adjust the plotting parameters
;
pro xregister_1d_plotpar,group=group
	common xregister_1d_common,info,xarray,yarray,error_array,mask_array, $
		r,g,b,rsave,gsave,bsave,xcoadd,ycoadd,errcoadd,xsel,ysel

;
; if already active, return
;
	if xregistered('xregister_1d_plotpar') then return
;
; set up widget layout
;
	
	mainbase = widget_base(/col,title='Plot Parameter Adjustment', $
		group=group)
	button = widget_button(mainbase,uvalue='DONE',value='Done')
	basex = widget_base(mainbase,/row)
	titles = strarr(10)
	for i=0,9 do begin
		widget_control,info.label_base(i),get_value=v
		titles(i) = v(0)
	end
	plot_select = cw_bgroup(basex,titles+' ', $
			/col,set_value=0,uvalue='PLOT_SELECT', $
			/exclusive)
	base = widget_base(basex,/col,/frame)
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
; linestyle
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
			thick_field:thick_field, $
			plot_select:plot_select,nsum_field:nsum_field}
	point = ptr_new({base:base})
	widget_control,mainbase,set_uvalue=point
	xregister_1d_plotpar_set,base,info,r,g,b
	xmanager,'xregister_1d_plotpar',mainbase,/no_block
	
	return
end

pro xregister_1d_plotpar_event,event
	common xregister_1d_common,info,xarray,yarray,error_array,mask_array, $
		r,g,b,rsave,gsave,bsave,xcoadd,ycoadd,errcoadd,xsel,ysel

    	widget_control,event.id,get_uvalue=uvalue
	widget_control,event.top,get_uvalue=point
	base = (*point).base
	widget_control,base.plot_select,get_value=i	;current selection
	apply = 0
	
	case uvalue of
	'DONE': begin
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
					error_array,mask_array
		xregister_1d_annotate,info
		if ptr_valid(point) then ptr_free,point
		widget_control,event.top,/destroy
		return
		end
	'PLOT_SELECT': begin
		if event.select eq 1 then begin
			xregister_1d_plotpar_set,base,info,r,g,b
			return
		end
		end
	'RED': r(i+2) = event.value
	'BLUE': b(i+2) = event.value
	'GREEN': g(i+2) = event.value
	'LINESTYLE': info.linestyle(i) = event.index
	'PSYM': begin
		psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
		info.psym(i) = psyms(event.index)
		end
	'APPLY': apply = 1
		
	else:
	endcase

	widget_control,base.thick_field,get_value=v
	info.thick(i) = v
	widget_control,base.size_field,get_value=v
	info.symsize(i) = v	
	widget_control,base.nsum_field,get_value=v
	info.nsum(i) = v	

	xregister_1d_plotpar_set,base,info,r,g,b
	if apply then begin
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray,yarray, $
				error_array,mask_array
		xregister_1d_annotate,info
	endif
	
    return
end
;==================================================== XREGISTER_1D_PLOTPAR_SET
;
; Routine to set plot paramters for the selected plot
;
pro xregister_1d_plotpar_set,base,info,red,green,blue

	widget_control,base.plot_select,get_value=isel
	widget_control,base.red_slider,set_value=red(isel+2)
	widget_control,base.green_slider,set_value=green(isel+2)
	widget_control,base.blue_slider,set_value=blue(isel+2)
	widget_control,base.linestyle,set_droplist_select=info.linestyle(isel)
	psyms = [-7,-6,-5,-4,-2,-1,0,1,2,3,4,5,6,7,10]
	good = where(psyms eq info.psym(isel))
	widget_control,base.psym,set_droplist_select=good(0)
	widget_control,base.size_field,set_value=info.symsize(isel)
	widget_control,base.thick_field,set_value=info.thick(isel)
	widget_control,base.nsum_field,set_value=info.nsum(isel)
;
; update plot
;	
	wset,base.window_id
	erase
	tvlct,red,green,blue
	ps = info.psym(isel)
	if ps eq 10 then ps=0
    	plots,indgen(11)*30,replicate(25,11),/dev,psym=ps, $
		color=isel+2,line=info.linestyle(isel), $
		thick=info.thick(isel),symsize = info.symsize(isel)

return
end
;================================================================= XREGISTER_1D
;
; Plot widget main routine	
	
pro xregister_1d,x,y,errin,maskin,title=title,xtitle=xtitle,ytitle=ytitle, $
	weight=weight,ptitle=ptitle,group=group,xrange=xrange,yrange=yrange, $
	min_val=min_val,max_val=max_val,process=process,init=init, $
	woffset = woffset,wout=wout,fout=fout,errout=errout, modal=modal
	common xregister_1d_common,info,xarray,yarray,error_array,mask_array, $
		r,g,b,rsave,gsave,bsave,xcoadd,ycoadd,errcoadd,xsel,ysel

;
; calling sequence
;
	if (n_params(0) lt 1) and not keyword_set(process) then begin
		print,'CALLING SEQUENCE:  xregister_1d,x,y,err,mask
		print,'Keyword INPUTS: title,xtitle,ytitle,ptitle,xrange,yrange
		print,'                /init, /process, weight
		print,'Keyword OUTPUTS: woffset, wout, fout, errout
		return
	end
;
; begin processing if keyword process is set
;
	if keyword_set(process) then begin
		xregister_1d_plot,info,xcoadd,ycoadd,errcoadd,xarray, $
					yarray,error_array,mask_array
		xregister_1d_annotate,info
		xmanager,'xregister_1d',info.main
		woffset = info.woffset
		wout = xcoadd
		fout = ycoadd
		errout = errcoadd
		xcoadd = 0
		err_coadd = 0
		ycoadd = 0
		return
	end

	if n_elements(title) eq 0 then title=''
	if n_elements(xtitle) eq 0 then xtitle=''
	if n_elements(ytitle) eq 0 then ytitle=''
	if n_elements(ptitle) eq 0 then ptitle=''
;
; add new x and y to common block
;
	ns = n_elements(x)
	if n_elements(errin) eq 0 then errin=fltarr(ns)
	if n_elements(maskin) eq 0 then maskin=replicate(1B,ns)
	if n_elements(weight) eq 0 then weight = 1.0
	if not keyword_set(init) then begin
		n = info.n
		if n eq 10 then return
		widget_control,info.xcor_base(n),sensitive=1
		widget_control,info.draw_base(n),sensitive=1
		widget_control,info.select_base(n),sensitive=1
		widget_control,info.label_base(n),sensitive=1
		widget_control,info.woff_base(n),sensitive=1
		widget_control,info.select_base(n),set_button=1
		widget_control,info.weight_base(n),sensitive=1
		info.use(n) = 1
		if n_elements(xrange) gt 0 then begin
			widget_control,info.xmin_base,set_v=xrange(0)
			widget_control,info.xmax_base,set_v=xrange(1)
		end
		if n_elements(yrange) gt 0 then begin
			widget_control,info.ymin_base,set_v=yrange(0)
			widget_control,info.ymax_base,set_v=yrange(1)
		end
		xarray = [xarray,x]
		yarray = [yarray,y(0:ns-1)]
		error_array = [error_array,errin(0:ns-1)]
		mask_array = [mask_array,maskin(0:ns-1)]
				
		info.ns(n) = ns
		widget_control,info.label_base(n),set_value=title
		widget_control,info.weight_base(n),set_value=weight
		if n_elements(min_val) gt 0 then info.min_val(n) = min_val
		if n_elements(max_val) gt 0 then info.max_val(n) = max_val
		info.n = info.n + 1
		if xtitle ne '' then info.xtitle=xtitle
		if ytitle ne '' then info.ytitle=ytitle
		xregister_1d_annotate,info

		return
	endif
;
; initilization	
;
	xarray = x
	yarray = y
	mask_array = maskin
	error_array = errin
	xcoadd = 0
	ycoadd = 0
	errcoadd = 0
	tvlct,r,g,b,/get
	rsave = r
	gsave = g
	bsave = b
	r(0) = [255,0,100,255,255,  0,  0,180,240, 75,110,150,250]
	g(0) = [255,0,100,  0,  0,220,  0, 80,140,180,110,120,250]
	b(0) = [255,0,100,255,  0,  0,255,180,100, 40,185,150,  0]
	tvlct,r,g,b 

	main = widget_base(/col,group=group,/tracking,uvalue='MAIN',modal=modal)
;
; Menu bar
;
	menu = widget_base(main,/row,/frame)
	exit = widget_button(menu,value='EXIT',uvalue='EXIT')
	desc = ['1\Write','0\Postscript File','2\FITS Table']
	button = cw_pdmenu(menu,desc,uvalue='WRITE',/return_full_name)
	button = widget_button(menu,uvalue='UNZOOMALL',value='UnZoom All')
	unzoom = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
	zoom = widget_button(menu,uvalue='ZOOM',value='Zoom')
	button = widget_button(menu,uvalue='LINESTYLES',value='Linestyles')
	button = widget_button(menu,uvalue='EDIT',value='Edit Mask')
	button = widget_button(menu,uvalue='DRAG',value='Drag')
	button = widget_button(menu,uvalue='SCALE',value='Scale')
	button = widget_button(menu,uvalue='WMINUS',value='<---')
	button = widget_button(menu,uvalue='WPLUS',value='--->')
;
; draw window 1
;
	base1 = widget_base(main,/row)
	base2 = widget_base(base1,/col)
	xsize1 = 650
	ysize1 = 400
	plot1 = widget_draw(base2,uvalue='PLOT1',retain=2, $
			xsize=xsize1,ysize=ysize1,/button_events,/motion)
	basex = widget_base(base2,/row)
        xmin_base = cw_field(basex,/row,uvalue='RANGE',value=min(x), $
                title='X Min: ',xsize=13,/return_events,/float)
        xmax_base = cw_field(basex,/row,uvalue='RANGE',value=max(x), $
                title='X Max: ',xsize=13,/return_events,/float)
	smooth_base = cw_field(basex,/row,uvalue='NSMOOTH',value=1, $
		title='N Smooth:',xsize=11,/return_events,/integer)
 	basex = widget_base(base2,/row)
        ymin_base = cw_field(basex,/row,uvalue='RANGE',value=min(y), $
                title='Y Min: ',xsize=13,/return_events,/float)
        ymax_base = cw_field(basex,/row,uvalue='RANGE',value=max(y), $
                title='Y Max: ',xsize=13,/return_events,/float)
	yoff_base = cw_field(basex,/row,uvalue='YOFF',value=0.0, $
		title='Y Offsets:',xsize=11,/return_events,/float)
;
; Draw window 2 (errors)
;
	ysize2 = 180
	plot2 = widget_draw(base2,uvalue='PLOT2',retain=2, $
			xsize=xsize1,ysize=ysize2,/button_events,/motion)
	basex = widget_base(base2,/row)
        ymin_base2 = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Min: ',xsize=13,/return_events,/float)
        ymax_base2 = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Max: ',xsize=13,/return_events,/float)

	base3 = widget_base(base1,/col)
;
; Plot control panel
;
	xcor_base = lonarr(10)
	draw_base = lonarr(10)
	label_base = lonarr(10)
	select_base = lonarr(10)
	woff_base = lonarr(10)
	weight_base = lonarr(10)
	for i=0,9 do begin
		bb = widget_base(base3,/row,/frame)
		xcor_base(i) = widget_button(bb,Value='Xcorr',uvalue= $
				'XCORR_'+strtrim(i,2))
		draw_base(i) = widget_draw(bb,uvalue='DRAW_'+strtrim(i,2), $
				retain=2,xsize=40,ysize=25,/button_events)
		b1 = widget_base(bb,/nonexclusive)
		select_base(i) = widget_button(b1,uvalue='SELECT_'+ $
				strtrim(i,2),value=' ')
		label_base(i) = widget_text(bb,uvalue='LABEL',value=' ', $
				/editable,xsize=15)
		woff_base(i) = cw_field(bb,/row,uvalue='WOFF',value=0.0, $
				title=' ',xsize=12,/return_events,/float)
		weight_base(i) = cw_field(bb,/row,uvalue='WEIGHT', $
				value=0.0,title=' ',xsize=10, $
				/return_events,/float)
	end	
	for i=1,9 do begin
		widget_control,xcor_base(i),sensitive=0
		widget_control,draw_base(i),sensitive=0
		widget_control,select_base(i),sensitive=0
		widget_control,label_base(i),sensitive=0
		widget_control,woff_base(i),sensitive=0
		widget_control,weight_base(i),sensitive=0
		
	end
;
; Coaddition panel
;
	base4 = widget_base(base3,/col,frame=2)
	button = widget_button(base4,Value = 'Coadd Spectra',uvalue='COADD')
	bb = widget_base(base4,/exclusive,/row,/frame)
	interp = widget_button(bb,value='Interpolate      ',uvalue='INTERP', $
		/no_release)
	nearest = widget_button(bb,value='Nearest Neighbor',uvalue='NEAREST', $
		/no_release)
	bb = widget_base(base4,/exclusive,/col,/frame)
	wbase1 = widget_button(bb,value='Use scalar weights',uvalue='SWEIGHT', $
		/no_release)
	wbase2 = widget_button(bb,value='Weight using Errors',uvalue='WEIGHT', $
		/no_release)
	wbase3 = widget_button(bb,value='Equal Weighting',uvalue='NOWEIGHT', $
		/no_release)
	button = widget_button(base4,value='Delete Coadded Spectrum', $
		uvalue='DELETECOADD')
;
; create pixmap
;
	window,xs=xsize1,ys=ysize1,/pixmap,/free	;main plot
	pixid1 = !d.window
	window,xs=xsize1,ys=ysize2,/pixmap,/free	;error plot
	pixid2 = !d.window
	window,xs=xsize1,ys=ysize1,/pixmap,/free	;reference overplot
	pixid3 = !d.window
;
; save widget info in structure
;
	widget_control,main,/realize
	widget_control,plot1,get_value=plot1_id
	widget_control,plot2,get_value=plot2_id
	window_ids = lonarr(10)
	for i=0,9 do begin
		widget_control,draw_base(i),get_value=v
		window_ids(i) = v
	end
	ns = lonarr(10)
	ns(0) = n_elements(x)
	widget_control,interp,set_button=1
	widget_control,select_base(0),set_button=1
	widget_control,wbase1,set_button=1
	use = intarr(10)
	use(0) = 1

	info = {n:1,ns:ns,psym:intarr(10),thick:intarr(10), $
		symsize:replicate(1.0,10),linestyle:intarr(10), $
		pixid1:pixid1,pixid2:pixid2,pixid3:pixid3, $
		xtitle:xtitle, $
		ytitle:ytitle,ptitle:ptitle,plot1:plot1,yoff_base:yoff_base, $
		main:main,plot2:plot2,xmin:min(x),ymin:min(y), $
		xmax:max(x),ymax:max(y),xmin_base:xmin_base, $
		ymin_base:ymin_base,xmax_base:xmax_base,ymax_base:ymax_base, $
		ymin_base2:ymin_base2,ymax_base2:ymax_base2, $
		xsave:!x,ysave:!y,state:'X/Y',smooth_base:smooth_base, $
		x1:0, y1:0, plot1_id:plot1_id,plot2_id:plot2_id, $
		nsum:intarr(10),min_val:replicate(-1e37,10), $
		max_val:replicate(1e37,10), xcor_base:xcor_base, $
		draw_base:draw_base,label_base:label_base, $
		select_base:select_base,woff_base:woff_base,use:use, $
		xsize1:xsize1,ysize1:ysize1,ysize2:ysize2,interp:1, $
		weight:'SWEIGHT',weight_base:weight_base, $
		window_ids:window_ids,plot_select:-1,woffset:fltarr(10), $
		scale:replicate(1.0,10),totweight:0.0}

	if n_elements(min_val) gt 0 then info.min_val(0) = min_val
	if n_elements(max_val) gt 0 then info.max_val(0) = max_val

	widget_control,info.label_base(0),set_value=title
	widget_control,info.weight_base(0),set_value=weight
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
	
	return
	end
