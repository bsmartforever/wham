;--------------------------------------------------  FUSE_ANALYSIS_BLINK
;
; Routine to blink plots
;
;
pro fuse_analysis_blink,group=group
@fuse_analysis_common
common temp,pixids
;
; get previous wavelength offsets and determine which detector is visible
; in the plots
;
	modes = strarr(10)
	cenwave = total(widget.wrange)/2.0
	woffsets = fltarr(10)
	if cenwave gt 996.0 then modes(1) = 'Sic1a' else modes(1) = 'SiC1b'		
	if cenwave lt 1012. then modes(2) = 'SiC2a' else modes(2) = 'SiC2b'
	if cenwave lt 1088. then modes(3) = 'LiF1a' else modes(3) = 'LiF1b'
	if cenwave gt 1098. then modes(4) = 'LiF2a' else modes(4) = 'LiF2b'
	modes(5:9) = ['coadd','sp1','sp2','sp3','sp4']
	modes(1:4) = 'spec_'+modes(1:4)
	for i=1,9 do begin
		istat = execute('ns = '+modes(i)+'.ns')
		if ns gt 0 then istat = $
			execute('woffsets(i) = '+modes(i)+'.woffset')
	end
;
; make ten pixmaps
;
	pixids = lonarr(10)
	for i=0,9 do begin
	    window,xs=widget.plot_nx,ys=widget.plot_ny,/pixmap,/free
	    pixids(i) = !d.window
	end
;
; create widget
;		
	base = widget_base(/col,group=group,/modal)
	menu = widget_base(base,/row)
	button = widget_button(menu,value='DONE',uvalue='DONE')
	blink = widget_button(menu,value='Start Blink',uvalue='BLINK')
	faster = widget_button(menu,value='Faster',uvalue='FASTER')
	slower = widget_button(menu,value='Slower',uvalue='SLOWER')
	overlay = widget_button(menu,value='Overlay On  ',uvalue='OVERLAY')
	button_base = lonarr(10)
	slider_base = lonarr(10)
	woffset_base = lonarr(10)
	labels = ['All','SiC 1','SiC 2','Lif 1','Lif 2','Coadd', $
		  'SP1','SP2','SP3','SP4']
	for i=0,9 do begin
	    bb = widget_base(base,/row,/frame)
	    b1 = widget_base(bb,/nonexclusive,xsize=80)
	    button_base(i) = widget_button(b1,uvalue='BUTTON_'+ $
				strtrim(i,2), value = labels(i))
	    if i ne 0 then begin
		slider_base(i) = widget_slider(bb,min=-100,max=100,xsize=220, $
				uvalue='SLIDER_'+strtrim(i,2),value=0,/drag, $
				/suppress_value)
		woffset_base(i) = cw_field(bb,uvalue='WOFFSET_'+strtrim(i,2), $
				value=woffsets(i),/float,title=' ',xsize=9, $
				/return_events)
	      end else begin
	      	basex = widget_label(bb,value='Spectrum Sliders',xsize=220)
		basex = widget_label(bb,value='        Woffsets')
	    end 
	end
;
; locate widget at right spot
;
	widget_control,group,tlb_get_offset=pos
	widget_control,base,tlb_set_xoffset=pos(0)+725, $
			    tlb_set_yoffset=pos(1)
	widget_control,base,/realize
;
; load pixmaps
;
    	for i=0,9 do begin
	    widget_control, widget.plot_select, set_value=i
	    fuse_analysis_plot
	    wset,pixids(i)
	    device,copy=[0,0,widget.plot_nx,widget.plot_ny,0,0,widget.plot_id]
	end
;
; start widget
;
	info = {button_base:button_base, slider_base:slider_base, $
		woffset_base:woffset_base, woffsets:woffsets, modes:modes, $
		pixids:pixids,type:'ONE',select:intarr(10), blink:blink, $
		faster:faster,slower:slower,overlay:overlay,rate:0.1,last:0}
	widget_control,base,set_uvalue=info
	xmanager,'fuse_analysis_blink',base
;
; clean up
;
	for i=0,9 do wdelete,pixids(i)
	widget_control, widget.plot_select, set_value=0
	widget.wrange = [0,0]
	fuse_analysis_plot,/keep_yrange
	
	return
end
pro fuse_analysis_blink_event,event
@fuse_analysis_common

	if event.id ne event.top then $
		widget_control,event.id,get_uvalue=uvalue $
		else uvalue = 'TIMER'
	widget_control,event.top,get_uvalue=info
	value = gettok(uvalue,'_')
	number = fix(uvalue)
	case value of
	
	'DONE': begin
		for i=1,9 do begin
			widget_control,info.woffset_base(i),get_value=v
			istat = execute(info.modes(i)+'.woffset=v(0)')
		end
		widget_control,event.top,/destroy
		return
		end

	'BUTTON': info.select(number) = event.select
	'WOFFSET': begin
		widget_control,info.woffset_base(number),get_value=v
		widget_control,info.slider_base(number),set_value=0
		istat = execute(info.modes(number)+'.woffset=v(0)')
		info.woffsets(number) = v(0)

		if number lt 5 then begin
	    		widget_control, widget.plot_select, set_value=0
	    		fuse_analysis_plot
			wset,info.pixids(0)
	    		device,copy= $
			  [0,0,widget.plot_nx,widget.plot_ny,0,0,widget.plot_id]
		end
		
	    	widget_control, widget.plot_select, set_value=number
	    	fuse_analysis_plot
		wset,info.pixids(number)
	    	device,copy= $
			[0,0,widget.plot_nx,widget.plot_ny,0,0,widget.plot_id]
		end
	'SLIDER': begin
		v = convert_coord([500,501],!y.crange,/device,/data)
		dwdx = v(0,1)-v(0,0)
		widget_control,info.slider_base(number),get_value=v
		widget_control,info.woffset_base(number), set_value = $
				info.woffsets(number) + v(0)*dwdx
		end
	'OVERLAY': begin
		if info.type ne 'OVERLAY' then begin
			info.type = 'OVERLAY'
			widget_control,info.overlay,set_value='Overlay Off'
		   end else begin
		   	info.type = 'ONE'
			widget_control,info.overlay,set_value='Overlay On'
		end
		widget_control,info.blink,set_value='Start Blink'
		end
	'BLINK': begin
		if info.type eq 'BLINK' then begin
			info.type = 'ONE'
			widget_control,info.blink,set_value='Start Blink'
		   end else begin
		   	info.type = 'BLINK'
			widget_control,info.blink,set_value='Stop Blink'
			widget_control,event.top,timer=info.rate
		end
		end
	'FASTER': info.rate = (info.rate*0.8 )>0.01
	'SLOWER': info.rate = info.rate/0.8
	'TIMER': if info.type eq 'BLINK' then $
			widget_control,event.top,time=info.rate
			
	else:
	endcase
;
; update display
;
	if info.type eq 'ONE' then begin
		i1 = number
		i2 = number
	end
	if info.type eq 'OVERLAY' then begin
	        i1 = 0
		i2 = 9
	end
	if info.type eq 'BLINK' then begin
		good = where(info.select,ngood)
		if ngood lt 2 then return
		ilast = where(good eq info.last) & ilast = ilast(0)
		if ilast eq ngood-1 then i1 = good(0) else i1 = good(ilast+1)
		info.last = i1
		i2 = i1
	end 
	
	if info.type eq 'OVERLAY' then $
			combined = bytarr(widget.plot_nx,widget.plot_ny)
	for i=i1,i2 do begin
;
; find portion of pixmap to copy and location to copy to
;
	    	if i gt 0 then begin
			widget_control,info.slider_base(i),get_value=v		
			if v(0) ge 0 then begin
				s1 = 0
				ns = widget.plot_nx-v(0)
				sout = v(0)
			   end else begin
			   	s1 = -v(0)
				ns = widget.plot_nx+v(0)
				sout = 0
			end
		    end else begin
	       		s1 = 0
			ns = widget.plot_nx
			sout = 0
		end
		if info.select(i) then begin
		   if info.type ne 'OVERLAY' then begin
		    	wset,widget.plot_id
;		   	if info.type eq 'BLINK' then erase	    
		    	device,copy= $
				[s1,0,ns,widget.plot_ny,sout,0,info.pixids(i)]
		     end else begin
		     	wset,info.pixids(i)
			pic = tvrd(s1,0,ns,widget.plot_ny)
			combined(sout:sout+ns-1,*) =  $
						combined(sout:sout+ns-1,*)>pic
		   end
		end
	endfor

	if info.type eq 'OVERLAY' then begin
		wset,widget.plot_id
		tv,combined
	end
	widget_control,event.top,set_uvalue=info
		
	return
	end
