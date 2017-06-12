; Widget to fit FUSE dispersion coefficients.
; Version 1. D. Lindler, Dec. 1999
;
;=========================================================FUSE_LINEFIT_EVENT
;
; Main event driver
;
pro fuse_linefit_event,event
    common fuse_linefit,info,data,results	
	
    tvlct,info.red,info.green,info.blue
    if event.id eq info.main then return
    widget_control,event.id,get_uvalue=uvalue
    case uvalue of 
	'FILE': begin
		case event.value of
		'FILE.Exit': Begin
			tvlct,info.rsave,info.gsave,info.bsave
			wdelete,info.pixid2
			wdelete,info.pixid1
			info = 0
			data = 0
			results = 0
			widget_control,event.top,/destroy
			return
			end
		'FILE.Read fuse_linefind wlog': begin
			file = dialog_pickfile(file='fuse.wlog', $
						filter='*.wlog',/must_exist)
			if file eq '' then return	;no file selected
			readcol,file,xtab,wtab,vel
			wtab = wtab+vel/2.997925d5*wtab
			ntab = n_elements(wtab)
			results = {wtab:wtab,xtab:xtab,fit:fltarr(ntab), $
				good:replicate(1,ntab),type:results.type, $
				norder:0,coef:0.0d0,rms:0.0}
			fuse_linefit_plot,info,data,results
			end
		'FILE.Read Spectrum.From CALFUSE output': begin
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
			x = findgen(ns)*16384/ns
			data = {ns:ns,filename:name,directory:directory, $
				wave:wave,x:x,flux:flux,h:h}
			fuse_linefit_plot,info,data,results
			end
		'FILE.Read Spectrum.From ASCII X and Y text file': begin
			file = dialog_pickfile(title='Select text data file', $
				/must_exist,filter=data.directory+'*.txt')
			if file eq '' then return
			fdecomp,file,disk,directory,name,ext
			readcol,file,x,flux,/silent
			ns = n_elements(x)
			wave = x
			data = {ns:ns,filename:name,directory:directory, $
				wave:wave,x:x,flux:flux,h:['END     ']}
			fuse_linefit_plot,info,data,results
			end
		'FILE.Read Spectrum.From Rowsum Fits Table': begin
			file = dialog_pickfile(title='Select FITS data file', $
				/must_exist,filter=data.directory+'*.fits')
			if file eq '' then return
			fdecomp,file,disk,directory,name,ext
			a = mrdfits(file,1,h,/silent)
			x = a.x
			flux = a.y
			ns = n_elements(x)
			wave = x
			data = {ns:ns,filename:name,directory:directory, $
				wave:wave,x:x,flux:flux,h:h}
			fuse_linefit_plot,info,data,results
			end
	
		'FILE.Write Log': begin
			file = dialog_pickfile(file='linefit.log', $
						filter='*.log',/write)
			if file eq '' then return	;no file selected
			widget_control,info.log,get_value=v
			openw,unit,file,/get_lun,/append
			for i=0,n_elements(v)-1 do printf,unit,v(i)
			free_lun,unit
			end
		'FILE.Clear Log': widget_control,info.log,set_value=''
		'FILE.Write Plot': begin
			file = dialog_pickfile(file='idl.ps', $
						filter='*.ps',/write)
			if file eq '' then return	;no file selected
			orig_device = !d.name
			set_plot,'ps'
			device,/port,bits=8,xsize=7,ysize=9.5,xoff=0.8, $
				yoff=1.0,/inches,/color,file=file
			tvlct,info.red,info.green,info.blue
			fuse_linefit_plot,info,data,results,/ps
			device,/close
			set_plot,orig_device
			end
		'FILE.Write Results': begin
			if n_elements(results.coef) lt 2 then return
			file = dialog_pickfile(file='linefit.txt', $
						filter='*.txt',/write)
			if file eq '' then return	;no file selected
			openw,unit,file,/get_lun
    			printf,unit,data.filename
			printf,unit,results.type+' Fit: order = '+ $
					strtrim(results.norder,2)
			printf,unit,'RMS of Fit = '+ $
					strtrim(string(results.rms,'(F10.3)'),2)
			for i=0,results.norder do printf,unit, $
			    '           C'+strtrim(i,2)+' =  '+ $
					string(results.coef(i))+ '  +/-  '+ $
					string(results.sigma(i))
			printf,unit,'       X         W      Residual'
			good = where(results.good,ngood)
			xtab = results.xtab(good)
			wtab = results.wtab(good)
			fitgood = results.fit(good)
    			for i=0,ngood-1 do printf,unit, $
    				string(xtab(i),'(F9.2)') + $
				string(wtab(i),'(F12.3)') + $
				string(wtab(i)-fitgood(i),'(F10.3)')
			free_lun,unit
			end

		endcase
		end
	'ORDER': begin
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'POLY0': begin
		results.type = 'Polynomial (X vs W)'
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'POLY1': begin
		results.type = 'Polynomial (W vs X)'
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'POLY2': begin
		results.type = 'Polynomial (W vs X/8192-1)'
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'CHEBYSHEV': begin
		results.type = 'Chebyshev Polynomials'
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'LEGENDRE': begin
		results.type = 'Legendre Polynomials'
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'MINUS': begin
	    	widget_control,info.order_field,get_value=v
	    	widget_control,info.order_field,set_value=(v(0)-1)>1
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'PLUS': begin
	    	widget_control,info.order_field,get_value=v
	    	widget_control,info.order_field,set_value=v(0)+1
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
		
	'ZOOM': begin
		widget_control,info.message,set_value= $
			'Place Cursor on first corner'+ $
			' and push left mouse button'
		info.state = 'ZOOM1'
		end
	'RANGE': fuse_linefit_plot,info,data,results
	'DELETE': begin
		widget_control,info.message,set_value= $
			'Place Cursor on line to be deleted'+ $
			' and push left mouse button'
		info.state = 'DELETE'
		end
	'RESTORE': begin
		widget_control,info.message,set_value= $
			'Place Cursor on line to be restored'+ $
			' and push left mouse button'
		info.state = 'RESTORE'
		end
	'RESTORE_ALL': begin
		results.good(*) = 1
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'DELETE_ALL': begin
		results.good(*) = 0
		fuse_linefit_fit,info,results
		fuse_linefit_plot,info,data,results
		end
	'UNZOOM_ALL': begin
		widget_control,info.xmin_base,set_value = min(data.x)
		widget_control,info.xmax_base,set_value = max(data.x)
		widget_control,info.ymin_base,set_value = min(data.flux)
		widget_control,info.ymax_base,set_value = max(data.flux)
		fuse_linefit_plot,info,data,results
		end
	'UNZOOM': begin
		widget_control,info.xmin_base,get_value = xmin
		widget_control,info.xmax_base,get_value = xmax
		widget_control,info.ymin_base,get_value = ymin
		widget_control,info.ymax_base,get_value = ymax
		xrange = (xmax-xmin)
		yrange = (ymax-ymin)
		xmin = (xmin - xrange*0.25) > min(data.x) < xmin
		xmax = (xmax + xrange*0.25) < max(data.x) > xmax
		ymin = (ymin - yrange*0.25) > min(data.flux) < ymin
		ymax = (ymax + yrange*0.25) < max(data.flux) > ymax
		widget_control,info.xmin_base,set_value = xmin
		widget_control,info.xmax_base,set_value = xmax
		widget_control,info.ymin_base,set_value = ymin
		widget_control,info.ymax_base,set_value = ymax
		fuse_linefit_plot,info,data,results
		end
	'PLOT1': begin
		xd = event.x	;device coordinates
		yd = event.y
		nxpix = info.nxpix
		nypix1 = info.nypix1
		nypix2 = info.nypix2
		wset,info.plot_id2
		device,copy=[0,0,nxpix,nypix2,0,0,info.pixid2]		
		wset,info.plot_id1
		device,copy=[0,0,nxpix,nypix1,0,0,info.pixid1]
		if (info.state eq 'RESTORE') or (info.state eq 'DELETE') or $
	 	   (info.state eq 'X/Y') then begin
		    wset,info.plot_id2
		    plots,[xd,xd],[0,nypix2],color=1,/dev
		    wset,info.plot_id1
		    plots,[xd,xd],[0,nypix1],color=1,/dev
		    plots,[0,nxpix],[yd,yd],color=1,/dev
		    if (event.press eq 2) and (info.state eq 'X/Y') then begin
				info.x1 = xd
				info.y1 = yd
				info.state = 'ZOOM2'
				return
		    endif
		end
		if (info.state eq 'DELETE') and (event.press gt 0) then begin
			fuse_linefit_delete,info,results,xd
			fuse_linefit_fit,info,results
			fuse_linefit_plot,info,data,results
		end			
		if (info.state eq 'RESTORE') and (event.press gt 0) then begin
			fuse_linefit_delete,info,results,xd,/restorex
			fuse_linefit_fit,info,results
			fuse_linefit_plot,info,data,results
		end			

		if info.state eq 'ZOOM1' and (event.press eq 1) then begin
		        plots,[xd,xd],[0,nypix1],color=1,/dev
		    	plots,[0,nxpix],[yd,yd],color=1,/dev
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
		    		fuse_linefit_plot,info,data,results
				return
		    	end
			!x = info.xsave1
			!y = info.ysave1
			v = convert_coord(x,y,/dev,/to_data)
			x = v(0,*)
			y = v(1,*)		    
		    	widget_control,info.xmin_base,set_value = min(x)
			widget_control,info.xmax_base,set_value = max(x)
			widget_control,info.ymin_base,set_value = min(y)
			widget_control,info.ymax_base,set_value = max(y)
			widget_control,info.message,set_value=' '
			fuse_linefit_plot,info,data,results
			return
		    end
		end	    			
		end
	'PLOT2': begin
		xd = event.x	;device coordinates
		yd = event.y
		nxpix = info.nxpix
		nypix1 = info.nypix1
		nypix2 = info.nypix2
		wset,info.plot_id2
		device,copy=[0,0,nxpix,nypix2,0,0,info.pixid2]		
		wset,info.plot_id1
		device,copy=[0,0,nxpix,nypix1,0,0,info.pixid1]
		if (info.state eq 'RESTORE') or (info.state eq 'DELETE') or $
		   (info.state eq 'X/Y') then begin
		    wset,info.plot_id2
		    plots,[xd,xd],[0,nypix2],color=1,/dev
		    plots,[0,nxpix],[yd,yd],color=1,/dev
		    wset,info.plot_id1
		    plots,[xd,xd],[0,nypix1],color=1,/dev
		end
		if (info.state eq 'DELETE') and (event.press gt 0) then begin
			fuse_linefit_delete,info,results,xd,yd
			fuse_linefit_fit,info,results
			fuse_linefit_plot,info,data,results
		end			
		if (info.state eq 'RESTORE') and (event.press gt 0) then begin
			fuse_linefit_delete,info,results,xd,yd,/restorex
			fuse_linefit_fit,info,results
			fuse_linefit_plot,info,data,results
		end
		end
	else:
    endcase
    return
end
;==================================================================== FREGRESS
FUNCTION FREGRESS,X,Y,Weights,YFIT,Const,SIGMA,FTEST,R,RMUL,CHISQ,STATUS, $
                 RELATIVE_weight=relative_weight,sigma0=sigma0
; NAME:
;	FREGRESS
;
; PURPOSE:
;	Perform a multiple linear regression fit.
;
;	REGRESS fits the function:
;		Y[i] = Const + A[0]*X[0,i] + A[1]*X[1,i] + ... + 
;                      A[Nterms-1]*X[Nterms-1,i]
;
; CATEGORY:
;       G2 - Correlation and regression analysis.
;
; CALLING SEQUENCE:
;    Result = FREGRESS(X, Y, Weights, Yfit, Const, Sigma, Ftest, R, Rmul, Chisq)
;
; INPUTS:
;       X:	The array of independent variable data.  X must 
;		be dimensioned as an array of Nterms by Npoints, where 
;		there are Nterms coefficients (independent variables) to be 
;		found and Npoints of samples.
;
;       Y:	The vector of dependent variable points.  Y must have Npoints 
;		elements.
;
; Weights:	The vector of weights for each equation.  Weights must be a vector
;		of Npoints elements.  For instrumental (Gaussian) weighting, 
;		W[i] = 1/standard_deviation(Y[i])^2.  For statistical  (Poisson)
;		weighting, w[i] = 1./Y[i].  For no weighting, set w[i]=1,
;		and also set the RELATIVE_WEIGHT keyword.
;
; OUTPUTS:
;	REGRESS returns a column vector of coefficients that has Nterms 
;	elements.
;
; OPTIONAL OUTPUT PARAMETERS:
;	Yfit:	Vector of calculated values of Y with Npoints elements.
;
;      Const:	Constant term. (A0)
;
;	Sigma:	Vector of standard deviations for coefficients.
;
;	Ftest:	The value of F for test of fit.
;
;	Rmul:	The multiple linear correlation coefficient.
;
;	R:	Vector of linear correlation coefficients.
;
;	Rmul:   The multiple linear correlation coefficient.
;
;	Chisq:	Reduced, weighted chi squared.
;
;       Status:  A named variable to receive the status of the INVERT 
;                (array inversion) computation. A value of 0 indicates 
;                a successful computation. A value of 1 indicates the 
;                inversion is invalid due to a singular array. A value 
;                of 2 indicates the possibility of an inaccurate result 
;                due to the use of a small pivot element.
;
; KEYWORDS:
; RELATIVE_WEIGHT:  If this keyword is set, the input weights
;		(W vector) are assumed to be relative values, and not based
;		on known uncertainties in the Y vector.  Set this keyword in 
;		the case of no weighting, W[*] = 1.
; sigma0 - output sigma for constant term
; PROCEDURE:
;	Adapted from the program REGRES, Page 172, 
;	Bevington, Data Reduction and Error Analysis for the 
;	Physical Sciences, 1969.
;
; MODIFICATION HISTORY:
;	Written, DMS, RSI, September, 1982.
;	Added RELATIVE_WEIGHT keyword    W. Landsman   August 1991
;       Fixed bug in invert  Bobby Candey 1991 April 22
;       Added STATUS argument.  GGS, RSI, August 1996
;	Dec, 1999, DJL, added sigma for constant term
;
On_error,2              ;Return to caller if an error occurs 
SY = SIZE(Y)            ;Get dimensions of x and y.  
SX = SIZE(X)
IF (N_ELEMENTS(Weights) NE SY[1]) OR (SX[0] NE 2) OR (SY[1] NE SX[2]) THEN $
  message, 'Incompatible arrays.'
;
NTERM = SX[1]           ;# OF TERMS
NPTS = SY[1]            ;# OF OBSERVATIONS
;
SW = TOTAL(Weights)           ;SUM OF WEIGHTS
YMEAN = TOTAL(Y*Weights)/SW   ;Y MEAN
XMEAN = (X * (REPLICATE(1.,NTERM) # Weights)) # REPLICATE(1./SW,NPTS)
WMEAN = SW/NPTS
WW = Weights/WMEAN
;
NFREE = NPTS-1          ;DEGS OF FREEDOM
SIGMAY = SQRT(TOTAL(WW * (Y-YMEAN)^2)/NFREE) ;Weights*(Y(I)-YMEAN)
XX = X- XMEAN # REPLICATE(1.,NPTS)      ;X(J,I) - XMEAN(I)
WX = REPLICATE(1.,NTERM) # WW * XX      ;Weights(I)*(X(J,I)-XMEAN(I))
SIGMAX = SQRT( XX*WX # REPLICATE(1./NFREE,NPTS)) ;Weights(I)*(X(J,I)-XM)*(X(K,I)-XM)
R = WX #(Y - YMEAN) / (SIGMAX * SIGMAY * NFREE)
ARRAY = (WX # TRANSPOSE(XX))/(NFREE * SIGMAX #SIGMAX)
IF (SX[1] EQ 1) THEN ARRAY = 1 / ARRAY ELSE begin
  ARRAY = INVERT(Array, status)
  if status eq 1L then MESSAGE, "Inversion Failed due to singular array."  
  endelse
A = (R # ARRAY)*(SIGMAY/SIGMAX)         ;GET COEFFICIENTS
YFIT = A # X                            ;COMPUTE FIT
Const = YMEAN - TOTAL(A*XMEAN)             ;CONSTANT TERM
YFIT = YFIT + Const                        ;ADD IT IN
FREEN = NPTS-NTERM-1 > 1                ;DEGS OF FREEDOM, AT LEAST 1.
CHISQ = TOTAL(WW*(Y-YFIT)^2)*WMEAN/FREEN ;WEIGHTED CHI SQUARED
;
; If all the weights are 1 then
; chisq = variance 
;
IF KEYWORD_SET(relative_weight) then varnce = chisq $
                                else varnce = 1./wmean
sigma = sqrt(array[indgen(nterm)*(nterm+1)]*varnce/(nfree*sigmax^2)) ;Error term
RMUL = TOTAL(A*R*SIGMAX/SIGMAY)         ;MULTIPLE LIN REG COEFF
IF RMUL LT 1. THEN FTEST = RMUL/NTERM / ((1.-RMUL)/FREEN) ELSE FTEST=1.E6
RMUL = SQRT(RMUL)
;
; sigma for constant term (added by DJL, Dec 99) from Bevington
;
sigma0 = varnce/npts
for j=0,nterm-1 do $
   for k=0,nterm-1 do $
   	sigma0 = sigma0 + varnce*xmean(j)*xmean(k)*array(j,k) / $
			   (nfree*sigmax(j)*sigmax(k))
sigma0 = sqrt(sigma0)

RETURN,A
END


;=========================================================== FUSE_LINEFIT_PLOT
;
; Routine to plot the data
;
pro fuse_linefit_plot,info,data,results,ps=ps
;
; get plot range
;
	set_xy
	widget_control,info.xmin_base,get_value=xmin
	widget_control,info.xmax_base,get_value=xmax
	widget_control,info.ymin_base,get_value=ymin
	widget_control,info.ymax_base,get_value=ymax
	widget_control,info.message,set_value=' '
;
; plot data
;
	if keyword_set(ps) then begin
		set_viewport,0.1,0.95,0.6,0.95
	   end else begin
		wset,info.plot_id1
		set_viewport,0.1,0.95,0.1,0.9
	end
	plot,data.x,data.flux,xrange=[xmin,xmax],yrange=[ymin,ymax], $
		xstyle=9,ystyle=1,color=1,xtickform='(I6)'
	if not keyword_set(ps) then begin
		info.xsave1 = !x
		info.ysave1 = !y
	end
;
; overplot line positions
;
	for i = 0,n_elements(results.xtab)-1 do begin
	    if results.good(i) then color=6 else color=4
	    oplot,[0,0]+results.xtab(i),!y.crange,color=color,thick=1
	end
;
; plot wave axis
;
	fuse_linefit_wave,[xmin,xmax],results,wrange
	axis,/xaxis,color=1,xrange=wrange,xstyle=1,xtickformat='(I6)'
;
; copy plot to pixmap
;
	if not keyword_set(ps) then begin
	   	wset,info.pixid1
	   	device,copy=[0,0,info.nxpix,info.nypix1,0,0,info.plot_id1]
	end
	info.state = 'X/Y'
;
; annotate postscript plot
;
	if keyword_set(ps) then begin
		!p.font = 0
	    	xyouts,0.5,0.08,data.filename, $
				/norm,align=0.5,color=1,charsize=1.3
    		xyouts,0.1,0.05,/norm,color=1, $
			results.type+' Fit: order = '+strtrim(results.norder,2)
	end
;
; plot residuals
;
	if n_elements(results.coef) lt 2 then return
	if keyword_set(ps) then begin
		set_viewport,0.1,0.95,0.35,0.5
		noeras=1
		xyouts,0.5,0.53,color=1,'Residuals (Angstroms)',/norm,align=0.5
	   end else begin
		wset,info.plot_id2
		set_viewport,0.1,0.95,0.1,0.9
		noeras=0
	end
	good = where(results.good,ngood)
	if ngood gt 2 then residuals = results.wtab(good) - results.fit(good) $
	              else residuals = results.wtab - results.fit
	miny = min(residuals)
	maxy = max(residuals)
	dy = maxy-miny
	yrange = [miny-dy/20,maxy+dy/20]
	plot,results.xtab,residuals,xstyle=9,ystyle=3,noeras=noeras, $
		yrange=yrange,xrange=[xmin,xmax],color=1,/nodata, $
		xtickformat='(I6)'
	oplot,results.xtab,(results.wtab-results.fit)<yrange(1)>yrange(0), $
		psym=4,color=4,symsize=1
	if ngood gt 0 then begin
		if ngood eq 1 then good=[good,good]	;need 2 points to plot
		xtab = results.xtab(good)
		wtab = results.wtab(good)
		fit  = results.fit(good)
		sub = sort(xtab)
	        oplot,xtab(sub),wtab(sub)-fit(sub),psym=-4, $
				color=1,line=1,symsize=1
	end
	oplot,!x.crange,[0,0],color=6
	if not keyword_set(ps) then begin
		info.xsave2 = !x
		info.ysave2 = !y
	end
;
; plot pixel axis
;
	axis,/xaxis,color=1,xrange=wrange,xstyle=1,xtickformat='(I6)'
;
; plot difference of fit and straight line fit
;
	if keyword_set(ps) then begin
		set_viewport,0.1,0.95,0.15,0.28
		x = dindgen(16384)
		fuse_linefit_wave,x,results,wave
		c = poly_fit(results.xtab,results.wtab,1)
		wave_line = c(0) + c(1)*x
		plot,x,wave-wave_line,noeras=1,color=1,xrange=[xmin,xmax], $
			xstyle=1,xtickform='(I6)'		
		oplot,[0,16384],[0,0],color=1,line=2
		xyouts,0.5,0.29,color=1,align=0.5,/norm, $
			'Deviation of Fit from Linear Fit (Angstroms)'
	end
	
	
;
; copy plot to pixmap
;
	if not keyword_set(ps) then begin
	   	wset,info.pixid2
	   	device,copy=[0,0,info.nxpix,info.nypix2,0,0,info.plot_id2]
	end else device,/close
	return
end
;============================================================= FUSE_LINEFIT_FIT
;
; Routine to perform least sqaures fit
;
pro fuse_linefit_fit,info,results
;
;
    widget_control,info.log,/append,set_value=' -------------------------------'
;
; extract good points
;
    good = where(results.good,ngood)
    if ngood lt 2 then return
    n = n_elements(results.wtab)
    wtab = double(results.wtab(good))
    xtab = double(results.xtab(good))
    widget_control,info.order_field,get_value=v
    norder=v(0)<(ngood-1)>1
    widget_control,info.order_field,set_value=norder
;
; Polynomial fit of X vs Wavelength
;
    if results.type eq 'Polynomial (X vs W)' then begin
       	x = dblarr(ngood,norder)
       	for i=0,norder-1 do x(*,i) = wtab^(i+1)
       	c = fregress(transpose(x),xtab,xtab*0+1,fitgood,c0,sigma, $
			/relative_weight,sigma0=sigma0)
	sigma = [sigma0,sigma]
       	coef = [c0,transpose(c)]
	minw = min(results.wtab)
	maxw = max(results.wtab)
	wv = dindgen(5001)*(maxw-minw)/5000.0+minw
	xv = coef(0)
	a = wv
	for i=1,norder do begin
		xv = xv + coef(i)*a
		a = a*wv
	end
	fitgood = interpol(wv,xv,xtab)
    end
;
; polynomial fit of Wavelength vs X
;
    if results.type eq 'Polynomial (W vs X)' then begin
       	x = dblarr(ngood,norder)
       	for i=0,norder-1 do x(*,i) = xtab^(i+1)
       	c = fregress(transpose(x),wtab,wtab*0+1,fitgood,c0,sigma, $
			/relative_weight,sigma0=sigma0)
	sigma = [sigma0,sigma]
       	coef = [c0,transpose(c)]
    end

;
; polynomial fit of Wavelength vs (X/8192 - 1)
;
    if results.type eq 'Polynomial (W vs X/8192-1)' then begin
       	x = dblarr(ngood,norder)
	xv = xtab/8192-1
       	for i=0,norder-1 do x(*,i) = xv^(i+1)
       	c = fregress(transpose(x),wtab,wtab*0+1,fitgood,c0,sigma, $
			/relative_weight,sigma0=sigma0)
	sigma = [sigma0,sigma]
       	coef = [c0,transpose(c)]
    end
;
; Chebyshev
;
    if results.type eq 'Chebyshev Polynomials' then begin
    	x = dblarr(ngood,norder)
	xv = (xtab/8192 - 1)
	for i=0,norder-1 do begin
		if i eq 0 then x(*,i) = xv
		if i eq 1 then x(*,i) = 2*xv^2 - 1
		if i gt 1 then x(*,i) = 2*xv*x(*,i-1)-x(*,i-2)
	end
	c = fregress(transpose(x),wtab,wtab*0+1,fitgood,c0,sigma, $
			/relative_weight,sigma0=sigma0)
	coef = [c0,transpose(c)]
	sigma = [sigma0,sigma]
    end    	
;
; Legendre
;
    if results.type eq 'Legendre Polynomials' then begin
    	x = dblarr(ngood,norder)
	xv = (xtab/8192 - 1)
	for i=0,norder-1 do begin
		m = double(i+1)
		if i eq 0 then x(*,i) = xv
		if i eq 1 then x(*,i) = (3*xv^2 - 1)/2
		if i gt 1 then x(*,i) = (2*m-1)/m*xv*x(*,i-1) - $
					(m-1)/m*x(*,i-2)
	end
	c = fregress(transpose(x),wtab,wtab*0+1,fitgood,c0,sigma, $
			/relative_weight,sigma0=sigma0)
	sigma = [sigma0,sigma]
	coef = [c0,transpose(c)]
    end
;
; process results
;
    rms = sqrt(total((wtab-fitgood)^2)/ngood)
    results = {wtab:results.wtab,xtab:results.xtab,fit:results.wtab*0.0d0, $
			type:results.type,good:results.good, $
			norder:norder,coef:transpose(coef),rms:rms, $
			sigma:sigma}
    fuse_linefit_wave,double(results.xtab),results,fit
    results.fit = fit
    widget_control,info.log,/append,set_value= $
		results.type+' Fit: order = '+strtrim(norder,2)

    for i=0,norder do widget_control,info.log,/append,set_value= $
		'          C'+strtrim(i,2)+' =  '+string(coef(i))+ $
		'  +/-  '+string(sigma(i))
    widget_control,info.log,/append,set_value= $
		'       X         W      Residual'
    for i=0,ngood-1 do widget_control,info.log,/app,set_v= $
    		string(xtab(i),'(F9.2)') + $
		string(wtab(i),'(F12.3)') + $
		string(wtab(i)-fitgood(i),'(F10.3)')
    widget_control,info.log,/append,set_value= $
    		'RMS of Fit = '+strtrim(string(rms,'(F10.3)'),2)
    return
	
end
;======================================================== FUSE_LINEFIT_WAVE
;
; Routine to compute wavelength predicted wavelengths given x values
;
pro fuse_linefit_wave,x,results,wave


    norder = results.norder
    n = n_elements(x)
    xx = double(x)
    if norder eq 0 then begin
    	wave = xx
	return
    end
    c = results.coef
    case results.type of
	'Polynomial (X vs W)': begin
		minw = min(results.wtab)
		maxw = max(results.wtab)
		wv = dindgen(5001)*(maxw-minw)/5000.0+minw
		xv = c(0)
		a = wv
		for i=1,norder do begin
			xv = xv + c(i)*a
			a = a*wv
		end
		wave = interpol(wv,xv,xx)
		end
	'Polynomial (W vs X)': begin
		wave = replicate(c(0),n)
		for i=1,norder do wave = wave + c(i)*xx^i
		end
	'Polynomial (W vs X/8192-1)': begin
		xx = (xx/8192-1)
		wave = replicate(c(0),n)
		for i=1,norder do wave = wave + c(i)*xx^i
		end
	'Chebyshev Polynomials': begin
		wave = replicate(c(0),n)
		xv = (xx/8192-1)
		p = dblarr(n,norder)
		for i=0,norder-1 do begin
			if i eq 0 then p(*,i) = xv
			if i eq 1 then p(*,i) = 2*xv^2 - 1
			if i gt 1 then p(*,i) = 2*xv*p(*,i-1)-p(*,i-2)
		end
		for i=0,norder-1 do wave = wave + c(i+1)*p(*,i)
		end
	'Legendre Polynomials': begin
		wave = replicate(c(0),n)
		xv = (xx/8192-1)
		p = dblarr(n,norder)
		for i=0,norder-1 do begin
			m = double(i+1)
			if i eq 0 then p(*,i) = xv
			if i eq 1 then p(*,i) = (3*xv^2 - 1)/2
			if i gt 1 then p(*,i) = (2*m-1)/m*xv*p(*,i-1) - $
							(m-1)/m*p(*,i-2)
		end
		for i=0,norder-1 do wave = wave + c(i+1)*p(*,i)
		end
   endcase
   return
end



;==========================================================  FUSE_LINEFIT_DELETE
;
; Routine to delete a line from the analysis
;
pro fuse_linefit_delete,info,results,xd,yd,restorex=restorex
;
; extract current good points
;
    if keyword_set(restorex) then good = where(results.good eq 0,ngood) $
        		    else good = where(results.good,ngood)
    if ngood eq 0 then return
    n = n_elements(results.wtab)
    wtab = results.wtab(good)
    xtab = results.xtab(good)
    residuals = wtab - results.fit(good)
;
; find closest point
;
    if n_elements(yd) eq 0 then begin
	wset,info.plot_id1
    	!x = info.xsave1
	!y = info.ysave1
	v = convert_coord(xtab,0.0,/data,/to_device)
	diff = abs(xd - transpose(v(0,*)))
      end else begin
	wset,info.plot_id2
    	!x = info.xsave2
	!y = info.ysave2
	residuals = residuals>!y.crange(0)<!y.crange(1)
	v = convert_coord(xtab,residuals,/data,/to_device)
	diff = (xd-transpose(v(0,*)))^2 + (yd-transpose(v(1,*)))^2
    end
    bad = where(diff eq min(diff))
    bad = bad(0)
    widget_control,info.log,/app,set_v='-------------------------------'
    widget_control,info.log,/append,set_v= $
    			'X='+strtrim(string(xtab(bad),'(F12.2)'),2)+ $
    			'  W='+strtrim(string(wtab(bad),'(F12.2)'),2)+ $
			'  Deleted'
    if keyword_set(restorex) then results.good(good(bad)) = 1 $
    			    else results.good(good(bad)) = 0
    return
end        

;============================================================== FUSE_LINEFIT
;
; Main Routine
;
pro fuse_linefit,wave,flux,x,wtab,xtab,header,filename,group=group,modal=modal

	common fuse_linefit,info,data,results	
;
; initialization
;
	if n_elements(wave) eq 0 then wave = findgen(1024)*16
	if n_elements(flux) eq 0 then flux = fltarr(1024)*16
	if n_elements(x) eq 0 then x = findgen(1024)*16
	if n_elements(xtab) eq 0 then xtab = 0.0
	if n_elements(wtab) eq 0 then wtab = 0.0
	if n_elements(header) eq 0 then header = ['END           ']
	if n_elements(filename) eq 0 then filename=' '
	if xregistered('fuse_linefit') then begin
		widget_control,info.main,/destroy
		wdelete,info.pixid1
		wdelete,info.pixid2
	end
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
		wave:wave,x:x,flux:flux,h:header}
	ntab = n_elements(xtab)
	results = {wtab:wtab,xtab:xtab,fit:fltarr(ntab), $
			good:replicate(1,ntab),type:'Polynomial (W vs X)', $
			norder:0,coef:0.0d0,rms:0.0,sigma:0.0d0}
	main = widget_base(/col,group=group,/tracking)
	menu = widget_base(main,/row,/frame)
	desc = ['1\FILE','0\Read fuse_linefind wlog', $
		'1\Read Spectrum','0\From CALFUSE output', $
		'0\From Rowsum Fits Table', $
		'2\From ASCII X and Y text file', $
		'0\Write Results', $
		'0\Write Plot','0\Write Log','0\Clear Log','2\Exit']
	button = cw_pdmenu(menu,desc,uvalue='FILE',/return_full_name)
	button = widget_button(menu,uvalue='UNZOOM_ALL',value='UnZoom All')
	button = widget_button(menu,uvalue='UNZOOM',value='UnZoom')
	button = widget_button(menu,uvalue='ZOOM',value='Zoom')
	button = widget_button(menu,uvalue='DELETE',value='Delete Line')
	button = widget_button(menu,uvalue='RESTORE',value='Restore Line')
	button = widget_button(menu,uvalue='DELETE_ALL', $
				value ='Delete All Lines')
	button = widget_button(menu,uvalue='RESTORE_ALL', $
				value='Restore All Lines')
	message = widget_label(main,value='       ',xsize=800)
	split = widget_base(main,/row)
	splita = widget_base(split,/col)
	splitb = widget_base(split,/col)
;
; plot windows
;	
	nxpix = 700
	nypix1 = 350
	nypix2 = 200
	plot1 = widget_draw(splita,uvalue='PLOT1',retain=2, $
				xsize=nxpix,ysize=nypix1,/button_events,/motion)
	plot2 = widget_draw(splita,uvalue='PLOT2',retain=2, $
				xsize=nxpix,ysize=nypix2,/button_events,/motion)
;
; plot range
;
	basex = widget_base(splitb,/row,/frame)
        xmin_base = cw_field(basex,/row,uvalue='RANGE',value=min(x), $
                title='X Min:',xsize=12,/return_events,/float)
        xmax_base = cw_field(basex,/row,uvalue='RANGE',value=max(x), $
                title='X Max:',xsize=12,/return_events,/float)
 	basex = widget_base(splitb,/row,/frame)
        ymin_base = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Min:',xsize=12,/return_events,/float)
        ymax_base = cw_field(basex,/row,uvalue='RANGE',value=0.0, $
                title='Y Max:',xsize=12,/return_events,/float)
;
; Fit button and order
;
	button = widget_button(splitb,value='Fit Polynomial (X vs W)', $
			uvalue='POLY0')
	button = widget_button(splitb,value='Fit Polynomial (W vs X)', $
			uvalue='POLY1')
	button = widget_button(splitb,value='Fit Polynomial (W vs X/8192-1)', $
			uvalue='POLY2')
	button = widget_button(splitb,value='Chebyshev Polynomials', $
			uvalue='CHEBYSHEV')
	button = widget_button(splitb,value='Legendre Polynomials', $
			uvalue='LEGENDRE')

	row = widget_base(splitb,/row,/frame)
	order_field = 	cw_field(row,/row,uvalue='ORDER',value=4, $
                title='Order:',xsize=12,/return_events,/integer)
	button = widget_button(row,value='   -1   ',uvalue='MINUS')
	button = widget_button(row,value='   +1   ',uvalue='PLUS')
;
; log window
;
	log = widget_text(splitb,xsize=40,ysize=20,/scroll, $
					uvalue='MESSAGE',/edit,font='6x13')

;
; save bases
;
	widget_control,main,/realize
	widget_control,plot1,get_value=plot_id1
	widget_control,plot2,get_value=plot_id2

	window,xs=nxpix,ys=nypix1,/pixmap,/free	;create pixmap
	pixid1 = !d.window
	window,xs=nxpix,ys=nypix2,/pixmap,/free	;create pixmap
	pixid2 = !d.window
	info = {main:main,plot_id1:plot_id1,plot_id2:plot_id2,log:log, $
		xmin_base:xmin_base,xmax_base:xmax_base, $
		ymin_base:ymin_base,ymax_base:ymax_base,rsave:rsave, $
		bsave:bsave,gsave:gsave,state:'X/Y',order_field:order_field, $
		xsave1:!x,ysave1:!y,xsave2:!x,ysave2:!y, $
		x1:0,y1:0,red:red,blue:blue,green:green,message:message, $
		pixid1:pixid1,pixid2:pixid2,nxpix:nxpix, $
		nypix1:nypix1,nypix2:nypix2}
;
; start widget
;
	fuse_linefit_plot,info,data,results
	xmanager,'fuse_linefit',main,/no_block
	return
end		
