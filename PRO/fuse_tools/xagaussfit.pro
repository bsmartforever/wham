;+
;			xagaussfit
;
; Interactive widget to fit multiple multiplicative absorption Gaussians
;
; 	I.E. the routine fits Fits
;		y = C*A1*A2*...*A8
;
; 	where:
;
;   	C is the continuum flux defined by:
;		C = b0 + b1*x + b2*x^2 
;
;   	Ai are the absorption lines with the form (i = 0 to 7)
;		Ai = exp(-a0*Gi)
;
;   	Gi are Gaussians:
;		Gi = exp(-[(x-a1)/a2]^2/2.0)
;
; CALLING SEQUENCE
;	xagaussfit,xvector,yvector,bcoef,gcoef,fit,mask=mask
; 
; INPUTS: 
;	xvector - X vector
;	yvector - Y vector
;
; OUTPUTS:
;	bcoef - coefficients for the polynomial baseline
;	gcoef - 3 x ngaussian coefficients for the fitted Gaussians
;		gcoef(0,*) = a0
;		gcoef(1,*) = a1 (centers)
;		gcoef(2,*) = a2 (Gaussian sigmas)
;	fit - fit evaluated at x vector
;
; OPTIONAL KEYWORD INPUTS:
;	title - plot title
;	xtitle - x plot title
;	ytitle - y plot title
;	group - widget id of the group leader
;	modal - set to make widget modal
;
; OPTIONAL KEYWORD INPUT/OUTPUTS:
;	mask - mask vector (1 for good data, 0 for bad data)
; HISTORY:
;	version 1  D. Lindler  Aug 1999
;	Aug 2000, D. Lindler, Changed from curvefit to lmfit.  added
;		manual continuum button, added ability to fix coefficients
;-
;-------------------------------------------------------------------------
;================================================================= FUNCT_MAGAUSS
;
; Function to compute multiple gaussian with polynomial baseline and
; optional partial derivatives
;
function funct_magauss,X,A,nopder=nopder
	common funct_mgauss_common,npoly,ngauss
	if (npoly eq 0) and (ngauss eq 0) then begin
		f = x*0+1.0
		return,f
	end	
	nx = n_elements(x)
	case npoly of
		0: P = replicate(1.0,nx)
		1: P = replicate(a(0),nx)
		2: P = a(0) + a(1)*x
		3: P = a(0) + a(1)*x + a(2)*x*x
	end
	pos = npoly
	g = replicate(1.0,nx)
	if ngauss gt 0 then begin
		for i=0,ngauss-1 do begin
			a(pos+2) = a(pos+2)>((max(x)-min(x))/1e10)
			z = (x-a(pos+1))/a(pos+2)
			g = g*exp(-a(pos)*exp(-z^2/2.0))
			pos = pos+3
		end
	end
	f = p*g
;
; partial derivatives
;
	if keyword_set(nopder) then return,f
	pder = dblarr(nx,n_elements(a))
	if npoly ge 1 then pder(*,0) = g
	if npoly ge 2 then pder(*,1) = x*g
	if npoly ge 3 then pder(*,2) = x^2*g
	if npoly ge 4 then pder(*,3) = x^3*g
	pos = npoly
	if ngauss gt 0 then begin
	    for i=0,ngauss-1 do begin
	        z = (x-a(pos+1))/a(pos+2)
		ez = exp(-z^2/2.0)
	    	pder(*,pos) = -f*ez
		pder(*,pos+1) = -f*a(pos)*ez*z/a(pos+2)
		pder(*,pos+2) = pder(*,pos+1) * z
		pos = pos+3
	    end
	end
	return,[[f],[pder]]
end
;========================================================= XAGAUSSFIT_EVENT
;
; Main event driver
;
pro xagaussfit_event,event
	common xagaussfit_common,base,x,y,yfit,coef,ymask,norm,fitflag
	common funct_mgauss_common,npoly,ngauss
	wset,base.plot1_id
	tvlct,base.red,base.green,base.blue
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
	'DONE': begin
		wdelete,base.pixmap1
		wdelete,base.pixmap2
		x=0
		y=0
		yfit=0
		base=0
		widget_control,event.top,/destroy
		end
	'COEF': begin
		xagaussfit_getcoef,base,coef,ngauss,npoly,fitflag
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
		end
	'WRITE': begin
		if npoly eq 0 then bcoef = 1.0D0 $
				       else bcoef=coef(0:npoly-1)
		if ngauss gt 0 then $
		  	   gcoef=reform(coef(npoly:npoly+ngauss*3-1),3,ngauss) $
			   else gcoef = 0.0d0
		funct_magauss,x,coef,yfit
		case event.value of
		'Write.Postscript File': begin
			file = dialog_pickfile(file='xagaussfit.ps', $
					filter='*.ps',/write)
			if file eq '' then return	;no file selected
			orig_device = !d.name
			set_plot,'ps'
			device,/port,xoff=1,yoff=1,xsize=7,ysize=9,bits=8, $
						/color,/inches,file=file
			
			xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly,/ps
			device,/close
			set_plot,orig_device
			!p.font = -1
			end
		'Write.ASCII Table': begin
			file = dialog_pickfile(file='xagaussfit.txt', $
					filter='*.txt',/write)
			if file eq '' then return	;no file selected
			openw,unit,file,/get_lun
			printf,unit,'; '+base.title
			printf,unit,'; '+base.ytitle
			printf,unit,'; '+base.xtitle
			printf,unit,'; Baseline Polynomial Coefficients: '
			for i=0,n_elements(bcoef)-1 do $
				printf,unit,';'+string(bcoef(i))
			printf,unit,';         Depth            Center' + $
					'          Sigma'
			if ngauss gt 0 then $
			   for i=0,ngauss-1 do printf,unit, '; '+$
			   	string(gcoef(*,i),'(3G16.8)')
			printf,unit,';'
			printf,unit,';          X               Y       ' + $
				  '        YFIT'
			for i=0,n_elements(x)-1 do  $
				printf,unit,x(i),y(i),yfit(i)
			free_lun,unit
			end
		'Write.FITS Table': begin
			file = dialog_pickfile(file='xagaussfit.fits', $
						filter='*.fits',/write)
			if file eq '' then return	;no file selected
			sxaddpar,h,'TITLE',base.title
			sxaddpar,h,'XTITLE',base.xtitle
			sxaddpar,h,'YTITLE',base.ytitle
			a = {x:x,y:y,yfit:yfit,bcoef:bcoef,gcoef:gcoef}
			mwrfits,a,file,h,/create
			end
		end
		end
	'EDIT': begin
		line_edit,x,y,ymask,group=event.top,/modal
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
		end
	'NORM': begin
		base.state = ''
		line_norm,x,y,ynorm,norm1,group=event.top,/modal
		norm = norm*norm1
		y = ynorm
		if ngauss gt 0 then begin
			coef = coef(npoly:npoly+ngauss*3-1)
			fitflag = fitflag(npoly:npoly+ngauss*3-1)
		   end else begin
			coef = 10.0
			fitflag = [0]
		end
		npoly = 0
		widget_control,base.npoly_base,set_value=0
		xagaussfit_getcoef,base,coef,ngauss,npoly,fitflag
		xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag	
		widget_control,base.ymin1,set_value=min(y)
		widget_control,base.ymax1,set_value=max(y)
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
		end
	'FIT': begin
		if base.state eq 'ADD' then begin
			base.state = ''
			ngauss = ngauss + 1
			widget_control,base.slider,sensitive=0,set_v=1
			widget_control,base.dslider,sensitive=0,set_v=10
			widget_control,base.basex1,sensitive=1
			widget_control,base.basefit,sensitive=1
			widget_control,base.cbase,sensitive=1
			widget_control,base.row1,sensitive=1
			widget_control,base.row2,sensitive=1
			widget_control,base.del_button,sensitive=1
			widget_control,base.help,set_v=' '
		end
		xagaussfit_getcoef,base,coef,ngauss,npoly,fitflag	
		xagaussfit_fit,x,y,ymask,coef,npoly,ngauss,fitflag
		xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag	
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
	       end
	'NBASE': begin
		oldbcoef = 1.0
		if npoly gt 0 then begin
			oldbcoef = coef(0:npoly-1)
			oldbfit = fitflag(0:npoly-1)
		end
		if ngauss gt 0 then begin
			oldgauss = coef(npoly:npoly+ngauss*3-1)
			oldgfit = fitflag(npoly:npoly+ngauss*3-1)
		end
		n = event.value
		newbcoef = 0.0
		newbfit = 0
		if (n gt 0) then begin
		    newbcoef = dblarr(n) & newbcoef(0) = 1.0
		    newbfit = intarr(n)
		    if (n lt npoly) then begin
		       c = [oldbcoef,0,0,0,0]
		       fit = c(0) + c(1)*x + c(2)*x*x + c(3)*x*x*x
		       if n eq 1 then newbcoef(0) = total(fit)/n_elements(fit) $
				 else newbcoef(0)=transpose(poly_fit(x,fit,n-1))
		    end else if npoly gt 0 then newbcoef(0) = oldbcoef
		endif
		npoly = n
		fitflag = [0]
		if (npoly+ngauss eq 0) then begin
			coef = 10.0 
		   end else begin
			if npoly eq 0 then begin
				coef = oldgauss
				fitflag = oldgfit
			end 
			if ngauss eq 0 then begin
				coef = newbcoef
				fitflag = intarr(npoly)
			end
		end
		if (ngauss gt 0) and (npoly gt 0) then begin
			coef = [newbcoef,oldgauss]
			fitflag = [oldbfit,oldgfit]
		end
		xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
		end
	'PLOT_RANGE': xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly
	
	'ADD': begin
		if base.state eq '' then begin
			widget_control,base.xmin,get_value=xmin
			widget_control,base.xmax,get_value=xmax
			sig = (xmax-xmin)/50.0
			if npoly+ngauss eq 0 then begin
				coef = [1.0,0.0,sig]
				fitflag = [0,0,0]
			   end else begin
				coef = [coef,1.0,0.0,sig]
				fitflag = [fitflag,0,0,0]
			end
			xagaussfit_setcoef,base,coef,ngauss+1,npoly,fitflag
			val = (sig/(xmax-xmin)*3000)>10<500
			widget_control,base.slider,sensitive=1,set_v=val
			depth = 1.0
			val = (depth*25)>1<500
			widget_control,base.dslider,sensitive=1,set_v=val
			base.state = 'ADD'
			st = ['Find approximate center and depth then', $
			      'push left the left mouse button', $
			      'Use sliders to adjust', $
			      'When done, Hit "FIT" or "Add Gaussian"'+ $
				' button again']
			widget_control,base.help,set_v=st
			widget_control,base.basex1,sensitive=0
			widget_control,base.basefit,sensitive=0
			widget_control,base.cbase,sensitive=0
			widget_control,base.row1,sensitive=0
			widget_control,base.row2,sensitive=0
			widget_control,base.del_button,sensitive=0
		   end else begin
		        ngauss = ngauss + 1
			base.state = ''
			widget_control,base.slider,sensitive=0,set_v=1
			widget_control,base.dslider,sensitive=0,set_v=10
			widget_control,base.basex1,sensitive=1
			widget_control,base.basefit,sensitive=1
			widget_control,base.cbase,sensitive=1
			widget_control,base.row1,sensitive=1
			widget_control,base.row2,sensitive=1
			widget_control,base.del_button,sensitive=1
			widget_control,base.help,set_v=' '
			xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly		
		end
		end
		
	'DELETE': begin
		xagaussfit_getcoef,base,coef,ngauss,npoly,fitflag
		isel = event.value - 1
		if isel ge ngauss then return
		coef(npoly+isel*3:npoly+isel*3+2) = 1e-35
		keep = where(coef ne 1e-35,nkeep)
		if nkeep gt 0 then begin
			coef = coef(keep) 
			fitflag = fitflag(keep)
		    end else begin
		    	coef=0.0
			fitflag = [0]
		end
		ngauss = ngauss - 1
		xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag	
		xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly		
		end
	'PLOT1': begin
		xd = event.x
		yd = event.y
		if (base.state eq 'ADD') and (event.press eq 1) then begin
			wset,base.plot1_id
			!x = base.xsave1
			!y = base.ysave1
			v = convert_coord(xd,yd,/dev,/to_data)
			coef(npoly + ngauss*3 +1) = v(0)
			yfit = funct_magauss(v(0),coef(0:(npoly+ngauss*3-1)>0))
			xagaussfit_setcoef,base,coef,ngauss+1,npoly,fitflag
			xagaussfit_gplot,base,x,y,coef
			widget_control,base.basefit,sensitive=1
		end
		if base.state eq '' then begin
			wset,base.plot2_id
			device,copy=[0,0,700,200,0,0,base.pixmap2]
			plots,[xd,xd],[0,200],/dev,color=1
			wset,base.plot1_id
			device,copy=[0,0,700,400,0,0,base.pixmap1]
			plots,[xd,xd],[0,400],/dev,color=1
			plots,[0,700],[yd,yd],/dev,color=1
		end
		end
	'PLOT2': begin
		xd = event.x
		yd = event.y
		if base.state eq '' then begin
			wset,base.plot1_id
			device,copy=[0,0,700,400,0,0,base.pixmap1]
			plots,[xd,xd],[0,400],/dev,color=1
			wset,base.plot2_id
			device,copy=[0,0,700,200,0,0,base.pixmap2]
			plots,[xd,xd],[0,200],/dev,color=1
			plots,[0,700],[yd,yd],/dev,color=1
		end
		if (event.press eq 1) and (base.state eq 'ADD') then begin
			wset,base.plot2_id
			!x = base.xsave2
			!y = base.ysave2
			v = convert_coord(xd,yd,/dev,/to_data)
			coef(npoly + ngauss*3 +1) = v(0)
			xagaussfit_setcoef,base,coef,ngauss+1,npoly,fitflag
			xagaussfit_gplot,base,x,y,coef
			widget_control,base.basefit,sensitive=1
		endif
		end
	'SIGMA_SLIDER': begin
		widget_control,base.xmin,get_value=xmin
		widget_control,base.xmax,get_value=xmax
		coef(npoly+ngauss*3+2) = event.value/3000.0*(xmax-xmin) 
		xagaussfit_setcoef,base,coef,ngauss+1,npoly,fitflag
		xagaussfit_gplot,base,x,y,coef
		end
	'DEPTH_SLIDER': begin
		coef(npoly+ngauss*3) = event.value/25.0
		xagaussfit_setcoef,base,coef,ngauss+1,npoly,fitflag
		xagaussfit_gplot,base,x,y,coef
		end
	else:
	endcase
return
end
;========================================================= XAGAUSSFIT_GETCOEF
;
; Routine to read coefficients from the widget
;
pro xagaussfit_getcoef,base,coef,ngauss,npoly,fitflag

	coef = dblarr((3*ngauss+npoly)>1)
	fitflag = intarr((3*ngauss+npoly)>1)
	if ngauss gt 0 then begin
	    for i=0,ngauss-1 do begin
	    	pos = npoly + i*3
		widget_control,base.centers(i),get_v=val
		xagaussfit_parse,val(0),val,flag
		fitflag(pos+1) = flag
		coef(pos+1) = val
		widget_control,base.peaks(i),get_v=val
		xagaussfit_parse,val(0),val,flag
		fitflag(pos) = flag
		coef(pos) = val
		widget_control,base.sigmas(i),get_v=val
		xagaussfit_parse,val(0),val,flag
		fitflag(pos+2) = flag
		coef(pos+2) = val
	    end
	end
		
	if npoly gt 0 then begin
	    for i=0,npoly-1 do begin
		widget_control,base.poly(i),get_v = val
		xagaussfit_parse,val(0),val,flag
		fitflag(i) = flag
		coef(i) = val
	    end
	end
	return
end
;=========================================================== XAGAUSSFIT_PARSE
;
; Routine to extract "X" from coefficient value
;
pro xagaussfit_parse,st,val,flag

	st = strupcase(st)
	if strpos(st,'X') ge 0 then flag = 1 else flag = 0
	remchar,st,'X'
	val = st
	return
end
;=========================================================== XAGAUSSFIT_FIT
;
; Routine to perform the least squares fit
;
	pro xagaussfit_fit,x,y,mask,coef,npoly,ngauss,fitflag
	good = where(mask,ngood)
	if ngood lt 5 then begin
		coef = 0.0
		npoly = 1
		ngauss = 0
		return
	end
	xx = double(x(good))
	yy = double(y(good))
	if (npoly eq 0) and (ngauss eq 0) then begin
		coef = 0.0
		return
	end
;
; transform coordinates to 0-1 in x and 
; -1 to 1 range in y to minimize under/overflows
;
	minx = min(xx)
	xrange = max(xx)-min(xx)
	xx = (xx - minx)/xrange
	maxy = max(abs(yy))
	if npoly eq 0 then maxy = 1.0
	yy = yy/maxy
	for i=0,ngauss-1 do begin
		coef(npoly+i*3) = coef(npoly+i*3)
		coef(npoly+i*3+1) = (coef(npoly+i*3+1)-minx)/xrange
		coef(npoly+i*3+2) = coef(npoly+i*3+2)/xrange
	end
	a = dblarr(3)
	if npoly gt 0 then begin
		c = [coef(0:npoly-1),0.0,0.0]
		a(0) = c(0) + c(1)*minx + c(2)*minx*minx
		a(1) = c(1)*xrange + 2*c(2)*xrange*minx
		a(2) = c(2)*xrange*xrange
	end
	for i=0,npoly-1 do coef(i) = a(i)/maxy
;
; perform fit
;
	fit = lmfit(xx,yy,coef,function_name='funct_magauss',/double, $
		iter=iter,itmin=10,fita=1-fitflag)
;
; transform coeficients back
;
	for i=0,ngauss-1 do begin
		coef(npoly+i*3) = coef(npoly+i*3)
		coef(npoly+i*3+1) = coef(npoly+i*3+1)*xrange + minx
		coef(npoly+i*3+2) = coef(npoly+i*3+2)*xrange
	end
	c = dblarr(3)
	if npoly gt 0 then begin
		a = [coef(0:npoly-1),0.0,0.0]
		c(0) = a(0) - a(1)/xrange*minx + a(2)/xrange/xrange*minx*minx
		c(1) = a(1)/xrange - 2*a(2)/xrange/xrange*minx
		c(2) = a(2)/xrange/xrange
	end
	for i=0,npoly-1 do coef(i) = c(i)*maxy
return
end


;========================================================= XAGAUSSFIT_PLOT
;
; Routine to plot results
;
pro xagaussfit_plot,base,x,y,mask,coef,ngauss,npoly,ps=ps

	if keyword_set(ps) then begin
		!p.font = 0
		set_viewport,0.1,0.98,0.5,0.9
		symsize=0.5
		csize = 1
	   end else begin
		widget_control,base.plot1,get_value=window_id
		wset,window_id
		set_viewport
		symsize= 0.7
		csize = 1.5
	end
;
; get plot limits
;
	widget_control,base.xmin,get_value=xmin
	widget_control,base.xmax,get_value=xmax
	widget_control,base.ymin1,get_value=ymin1
	widget_control,base.ymax1,get_value=ymax1
	widget_control,base.ymin2,get_value=ymin2
	widget_control,base.ymax2,get_value=ymax2
;
; plot data
;
	xx = findgen(1000)*(max(x)-min(x))/999.0+min(x)
	yfit = funct_magauss(x,coef,/nopder) 
	yyfit = funct_magauss(xx,coef,/nopder) 
	c = [1.0d0,0.0,0.0,0.0]
	if npoly gt 0 then c(0) = coef(0:npoly-1)
	ybase = c(0) + c(1)*xx + c(2)*xx*xx 
	plot,x,y,xrange=[xmin,xmax],xstyle=1,yrange=[ymin1,ymax1],ystyle=1, $
		xtitle=base.xtitle,ytitle=base.ytitle,title='',/nodata,color=4
;
	if ngauss gt 0 then begin
	    for i=0,ngauss-1 do begin
		pos = npoly+i*3
		gaussx,xx,coef(pos+1),coef(pos+2),coef(pos),g
		oplot,xx,exp(-g)*ybase,color=5
		oplot,coef(pos+1)+fltarr(2),!y.crange,color=5
		xyouts,coef(pos+1), $
			!y.crange(1)+0.01*(!y.crange(1)-!y.crange(0)), /data, $
			strtrim(i+1,2),align=0.5,size=csize,color=4
	    end
	end
	oplot,xx,ybase,color=2
	oplot,x,y,color=4
	oplot,x,y,color=7,psym=4,symsize=symsize
	good = where(mask,ngood)
	if ngood gt 0 then oplot,x(good),y(good),color=4,psym=4,symsize=symsize
	oplot,xx,yyfit,color=1,thick=2
	if not keyword_set(ps) then begin
		base.xsave1 = !x
		base.ysave1 = !y
		base.psave1 = !p
	
;
; copy to pixmap
;
		wset,base.pixmap1
		device,copy=[0,0,700,400,0,0,base.plot1_id]
	end
;
; plot residuals
;
	if keyword_set(ps) then begin
		set_viewport,0.1,0.98,0.3,0.45	
		noerase = 1
		symsize = 0.2
	    end else begin
	    	noerase = 0
		widget_control,base.plot2,get_value=window_id
		wset,window_id
		symsize = 0.4
	end
	plot,x,y-yfit,xrange=[xmin,xmax],yrange=[ymin2,ymax2],xstyle=1,$
		ystyle=1,color=4,psym=4,symsize=symsize,noerase=noerase, $
		ytitle='Residual',/nodata
	if ngood gt 0 then oplot,x(good),y(good)-yfit(good),psym=-4, $
			symsize=symsize,color=4
	bad = where(mask eq 0,nbad)
	if nbad gt 0 then oplot,x(bad),y(bad)-yfit(bad),psym=4, $
					symsize=symsize,color=7
	oplot,x,yfit*0,color=3
;
; copy to pixmap
;
	if not keyword_set(ps) then begin
		base.xsave2 = !x
		base.ysave2 = !y
		base.psave2 = !p
		wset,base.pixmap2
		device,copy=[0,0,700,200,0,0,base.plot2_id]
	end
;
; annotate postscript plot
;
	if keyword_set(ps) then begin
		xyouts,0.50,0.95,base.title,/norm,color=4,align=0.5,size= 1.3
		if npoly gt 0 then begin
			xyouts,0.1,0.2,'Baseline Coefficients',color=4,/norm
			for i=0,npoly-1 do xyouts,0.15,0.17-i*0.02,/norm, $
						color=4,strtrim(coef(i),2)
		end
		if ngauss gt 0 then begin
		    xyouts,0.45,0.2,/norm,color=4,  $
			'           Depth            Center           FWHM'
		    for i=0,ngauss-1 do xyouts,0.45,0.17-i*0.02, $
				/norm,color=4,strtrim(i+1,2)+ $
				string(coef(npoly+i*3:npoly+i*3+2) * $ 
					[1.0,1.0,2.3548],'(3G14.8)')
		end
	endif	
	return
	end
;========================================================= XAGAUSSFIT_GPLOT
;
; to overplot current guess plot results
;
pro xagaussfit_gplot,base,x,y,coef
	common funct_mgauss_common,npoly,ngauss
	wset,base.plot1_id
	!x = base.xsave1
	!y = base.ysave1
	!p = base.psave1
	device,copy=[0,0,700,400,0,0,base.pixmap1]
;
; overplot on data plot
;
	ngauss = ngauss + 1
	yfit = funct_magauss(x,coef,/nopder)
	oplot,x,yfit,color=6,thick=2
;
; overplot residuals
;
	wset,base.plot2_id
	device,copy=[0,0,700,200,0,0,base.pixmap2]
	yfit = funct_magauss(x,coef,/nopder)
	!x = base.xsave2
	!y = base.ysave2
	!p = base.psave2
	oplot,x,y-yfit,color=6,thick=2
	ngauss = ngauss - 1
	
	return
	end

;========================================================= XAGAUSSFIT_SETCOEF
;
; Routine to write coefficients to the widget
;
pro xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag


	for i=0,7 do begin
		widget_control,base.centers(i),set_v='0.0',sensitive=0
		widget_control,base.peaks(i),set_v='0.0',sensitive=0
		widget_control,base.sigmas(i),set_v='0.0',sensitive=0
	end

	widget_control,base.ngauss_label,set_v='Number of Gaussians to Fit: '+ $
			strtrim(ngauss,2)

	if ngauss gt 0 then begin
	    for i=0,ngauss-1 do begin
	    	pos = npoly + i*3
		if fitflag(pos+1) eq 1 then st='X' else st=''
		widget_control,base.centers(i),set_v=string(coef(pos+1))+st, $
				sensitive=1
		if fitflag(pos) eq 1 then st='X' else st=''
		widget_control,base.peaks(i),set_v=string(coef(pos))+st, $
				sensitive=1
		if fitflag(pos+2) eq 1 then st='X' else st=''
		widget_control,base.sigmas(i),set_v=string(coef(pos+2))+st, $
			sensitive=1
	    end
	end
	widget_control,base.poly(0),set_v = 1.0, sensitive = 0		
	for i=1,2 do widget_control,base.poly(i),set_v = '0', sensitive = 0
	if npoly gt 0 then for i=0,npoly-1 do begin
		if fitflag(i) eq 1 then st='X' else st=''		
		widget_control,base.poly(i),set_v = string(coef(i))+st, $
				sensitive = 1
	end
		
	return
end


;================================================================= XAGAUSSFIT
pro xagaussfit,xvector,yvector,bcoef,gcoef,fit,group=group,title=title, $
	xtitle=xtitle,ytitle=ytitle,mask=mask,modal=modal,ynorm=ynorm

	common xagaussfit_common,base,x,y,yfit,coef,ymask,norm,fitflag
	common funct_mgauss_common,npoly,ngauss

	if n_params(0) eq 0 then begin
		print,'CALLING SEQUENCE: xagaussfit,x,y,bcoef,gcoef,fit
		print,'INPUTS: x, y       OUTPUTS: bcoef, gcoef, fit
		print,'OPTIONAL KEYWORD INPUTS: title, xtitle, ytitle, group'
		return
	end
;
; initialization
;
	if n_elements(title) eq 0 then title='XAGAUSSFIT'
	if n_elements(xtitle) eq 0 then xtitle=''
	if n_elements(ytitle) eq 0 then ytitle=''
	x = double(xvector)
	y = double(yvector)
	if n_elements(mask) eq 0 then mask=replicate(1,n_elements(xvector))
	ymask = mask
	norm = y*0+1
	red =  [255,200,  0,  0, 0, 255, 240, 200]
	green =[255,  0,200,  0, 0, 175, 100, 200]
	blue = [255,  0,  0,255, 0,   0, 240,   0]
	fitflag = intarr(10)
	tvlct,red,green,blue
;
; create widget layout
;
;;	widget_control,default_font  = $
;;		 '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

	main = widget_base(/col,group=group,title=title,/tracking, $
			uvalue='MAIN',modal=modal)
;
; menu bar
;
	menu = widget_base(main,/row)
	done = widget_button(menu,value='Done',uvalue='DONE')
	desc = ['1\Write','0\Postscript File','0\ASCII Table','2\FITS Table']
	button = cw_pdmenu(menu,desc,uvalue='WRITE',/return_full_name)
	button = widget_button(menu,value='Edit Data Mask',UVALUE='EDIT')
	button = widget_button(menu,value='Normalize',UVALUE='NORM')
;
; plot windows
;
	split = widget_base(main,/row)
	splita = widget_base(split,/col)
	splitb = widget_base(split,/col)
	plot1 = widget_draw(splita,xsize=700,ysize=400,/button_events, $
		uvalue='PLOT1',/motion)
	row1 = widget_base(splita,/row)
	xmin = cw_field(row1,/row,uvalue='PLOT_RANGE',value=min(x), $
		title='Xmin:',xsize=10,/return_events,/float)
	xmax = cw_field(row1,/row,uvalue='PLOT_RANGE',value=max(x), $
		title='Xmax:',xsize=10,/return_events,/float)

	ymin1 = cw_field(row1,/row,uvalue='PLOT_RANGE',value=min(y), $
		title='Ymin:',xsize=12,/return_events,/float)
	ymax1 = cw_field(row1,/row,uvalue='PLOT_RANGE',value=max(y), $
		title='Ymax:',xsize=12,/return_events,/float)
	

	plot2 = widget_draw(splita,xsize=700,ysize=200,/button_events, $
		uvalue='PLOT2',/motion)
	row2 = widget_base(splita,/row)
	label = widget_label(row2,value='   Residuals:')
	ymin2 = cw_field(row2,/row,uvalue='PLOT_RANGE',value=0.0, $
		title='Ymin:',xsize=12,/return_events,/float)
	ymax2 = cw_field(row2,/row,uvalue='PLOT_RANGE',value=0.0, $
		title='Ymax:',xsize=12,/return_events,/float)
;
; coefficient widgets
;
	help = widget_text(splitb,xsize=40,ysize=4,uvalue='MESSAGE')
	basefit = widget_button(splitb,value='FIT',uvalue='FIT')
;
;   baseline
;
	basex1 = widget_base(splitb,/frame,/col)
	label = widget_label(basex1,value='Baseline Polynomial Order')
	names = ['None','0','1','2']
	npoly_base = cw_bgroup(basex1,names,/row,/exclusive,uvalue='NBASE', $
		set_v=1)
	row = widget_base(basex1,/row)
	ipoly = lonarr(3)
	ipoly(0) = cw_field(row,/row,uvalue='COEF',value=0.0, $
		title='B0:',xsize=12,/return_events)
	ipoly(2) = cw_field(row,/row,uvalue='COEF',value=0.0, $
		title='B2:',xsize=12,/return_events)
	row = widget_base(basex1,/row)
	ipoly(1) = cw_field(row,/row,uvalue='COEF',value=0.0, $
		title='B1:',xsize=12,/return_events)
;
; Gaussians
;
	basex = widget_base(splitb,/row)
	label = widget_label(basex,value='Width SLIDER')
	slider = widget_slider(basex,min=10,max=500,/drag,/suppress_value, $
		uvalue='SIGMA_SLIDER',xsize=300)
	basex = widget_base(splitb,/row)
	label = widget_label(basex,value='Depth SLIDER')
	dslider = widget_slider(basex,min=1,max=500,/drag,/suppress_value, $
		uvalue='DEPTH_SLIDER',xsize=300)
	basex = widget_base(splitb,/frame,/col)
	ngauss_label = widget_label(basex,value='Number of Gaussians to Fit: 1')
	menu = widget_base(basex,/row)
	add_button = widget_button(menu,value='Add Gaussian',uvalue='ADD')
	desc = ['1\Delete Gaussian','0\1','0\2','0\3','0\4','0\5','0\6', $
								'0\7','2\8']
	desc(1:8) = desc(1:8)+'          '
	del_button = cw_pdmenu(menu,desc,uvalue='DELETE')
	cbase = widget_base(basex,/row)
	base1 = widget_base(cbase,/col)
	label = widget_label(base1,value='Centers')
	centers = lonarr(8)
	for i=0,7 do centers(i)=cw_field(base1,xsize=11, $
			title=strtrim(i+1,2),/row, $
			uvalue='COEF',value=0.0,/return_events)
	base1 = widget_base(cbase,/col)
	label = widget_label(base1,value='Sigmas')
	sigmas = lonarr(8)
	for i=0,7 do sigmas(i)=cw_field(base1,xsize=11,title=' ',/row, $
			uvalue='COEF',value=0.0,/return_events)
	base1 = widget_base(cbase,/col)
	label = widget_label(base1,value='Depth')
	Peaks = lonarr(8)
	for i=0,7 do Peaks(i)=cw_field(base1,xsize=13,title=' ',/row, $
			uvalue='COEF',value=0.0,/return_events)
	widget_control,main,/realize
;
; get 2 pixmaps
;
	window,xs=700,ys=400,/free,/pixmap
	pixmap1 = !d.window
	window,xs=700,ys=200,/free,/pixmap
	pixmap2 = !d.window
	
;
; create information structure
;
	widget_control,plot1,get_v=plot1_id
	widget_control,plot2,get_v=plot2_id

	base = {plot1:plot1,xmin:xmin,xmax:xmax,ymin1:ymin1,ymin2:ymin2, $
		ymax1:ymax1,ymax2:ymax2,plot2:plot2, help:help, slider:slider, $
	 	npoly_base:npoly_base,poly:ipoly,centers:centers, $
		sigmas:sigmas,peaks:peaks,plot1_id:plot1_id,plot2_id:plot2_id, $
		ngauss_label:ngauss_label,pixmap1:pixmap1,pixmap2:pixmap2, $
		title:title,xtitle:xtitle,ytitle:ytitle,red:red,blue:blue, $
		green:green,state:'',add_button:add_button,basefit:basefit, $
		del_button:del_button,basex1:basex1,cbase:cbase,row1:row1, $
		row2:row2,xsave1:!x,xsave2:!x,ysave1:!y,ysave2:!y, $
		psave1:!p, psave2:!p, dslider:dslider}
;
; set intial fit
;
	ngauss = 0
	npoly = 1
	coef = avg(y)
	xagaussfit_plot,base,x,y,ymask,coef,ngauss,npoly

;
; populate widget
;
	widget_control,slider,sensitive=0
	widget_control,dslider,sensitive=0
	xagaussfit_setcoef,base,coef,ngauss,npoly,fitflag	
;
; start widget
;
	xmanager,'xagaussfit',main
	fit = funct_magauss(xvector,coef,/nopder)*norm
	if npoly gt 0 then bcoef = coef(0:npoly-1) else bcoef = 0
	if ngauss gt 0 then $
		gcoef = reform(coef(npoly:npoly+ngauss*3-1),3,ngauss) $
			else gcoef = 0
	mask = ymask
	ymask = 0
	ynorm = norm
	norm = 0
return
end
