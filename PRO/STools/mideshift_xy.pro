pro mideshift_xy,wavelength,flux,xmin,ymin,sigmax,npoints=npoints,order=order,$
interp=interp,minabs=minabs,noisy=noisy,noplot=noplot,tpb=tpb

;+
; Measuring the wavelength of the center of a line.
;
;
; IN:  wavelength - dblarr - wavelengths (angstroms); absolute values
;      flux 	  - fltarr - fluxes 
;
; OUT: xmin       - double  - wavelength of the line bottom
;      ymin       - double  - flux at xmin
;      sigmax	  - double  - sigma associated with the measurement of xmin
;
;
; KEYWORDS: npoints- number of pixels around the minimum to enter the fit.
;			(default: 7) It has to be an even number.
;	    order  - order of the polynomial (defult: 3 = third order)
;	    interp - when on, we use a spline interpolation to improve sampling
;			(step= 0.005 A)
;	    minabs - min. central absorption of a line in order to be considered
;			(default: 0.98)
;	    noisy  - if set, we measure line shifts of lines with irregular
;			shapes close to the line center. Otherwise, we don't.
;	    noplot - when set, it does not produce any plot
;	    tpb	   - two-point-bisector tecnique (Hamilton & Lester 1999)
;
; NOTE: when the measurement cannot be performed, -1000 is return in xmin,ymin
;	and sigmax 
;
; C. Allende Prieto, UT,  1999
; C. Allende Prieto, UT,  2001 included spline interp. to improve sampling 
; C. Allende Prieto, UT,  2001 more rigorous determination of error in lambda
;				implementation of 'security' checks
; C. Allende Prieto, UT,  2006 fixed a bug in the determination of the error
;				and added a 2nd error estimate, the average of
;				the two is kept as tests indicate it is more robust
;				than either method alone.
;-
npar = n_params()
if (npar eq 0) then begin
	print,'mideshift_xy,x,y,xmin,ymin,lsig'
	return
endif 

step=wavelength(1)-wavelength(0)

keepwavelength=wavelength & keepflux=flux

if keyword_set(interp) then begin
; spline interp. to improve sampling
	;if(step gt 5e-3) then begin
		step=5e-3
		nns=floor((max(wavelength)-min(wavelength))/step)
		nwavelength=findgen(nns)*step+min(wavelength)
	;endif
	nflux=spline(wavelength,flux,nwavelength)
	wavelength=nwavelength & flux=nflux
endif

if not keyword_set(npoints) then npoints=7
if not keyword_set(order) then order=3
if not keyword_set(minabs) then minabs=0.98
if not keyword_set(noisy) then begin
	noisy=0
endif else begin
	noisy=100
endelse
nhalf=(npoints-1)/2

data=wavelength(min(where(flux eq min(flux))))
big=size(flux)
pixelmin=fix(min(where(flux eq min(flux))))


; we  request at least nhalf points each side of the  minimum
;		a consistant slope for each group of nhalf points on each side 
;			(unless /noisy is set)
;	 	a significant absorption to exist (minabs of the continuum level)
cool=1
if (pixelmin+nhalf gt big(1)-1 or pixelmin-nhalf lt 0 or $
		min(flux) gt minabs) then begin
	cool=0
endif else begin
	if(max(deriv(flux(pixelmin-nhalf:pixelmin-1))) gt noisy or $
		min(deriv(flux(pixelmin+1:pixelmin+nhalf))) lt -noisy) then cool=0
endelse

if (cool eq 0) then begin			
		print,'% mideshift_xy: measurement failure'
		xmin=-1000
		ymin=-1000
		sigmax=-1000
		sigmay=-1000
		wavelength=keepwavelength & flux=keepflux
		if not keyword_set(noplot) then begin
			plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
		endif	
		return
endif else begin

if  keyword_set(tpb) then begin ; 2pd if

	first_blue=0.5*(wavelength(pixelmin-1)+$
	interpol(wavelength(pixelmin:n_elements(wavelength)-1),$
	flux(pixelmin:n_elements(wavelength)-1),flux(pixelmin-1)))
	first_red=0.5*(wavelength(pixelmin+1)+$
	interpol(wavelength(0:pixelmin),$
	flux(0:pixelmin),flux(pixelmin+1)))
	xmin=mean([first_blue,first_red])
	sigmax=0. ; unknown
	ymin=flux(pixelmin); approximately
	
endif else begin


coef=poly_fit(wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),$
flux(pixelmin-nhalf:pixelmin+nhalf),order,p1,p2,sigmay,p4)

; old style for getting the minimum and sigma
;ran=3*step;
;xgrid=dindgen(ran/0.000001)*0.000001+wavelength(pixelmin-1)-data(0)
;ygrid=coef(0)+coef(1)*xgrid+coef(2)*xgrid^2+coef(3)*xgrid^3;+coef(4)*xgrid^4
;xmin=xgrid(where(ygrid eq min(ygrid)))+data(0)
;ymin=min(ygrid(where(ygrid eq min(ygrid))))
;New error estimate; Carlos UT Feb 2001 
;P(x)=sum(i=1,2,3) A(i) X**i
; sigma^2(x)=sigma^2(P) / (sum[i=1,2,3] A^2(i) i^2 x^(2*(i-1)))
;ii=transpose(findgen(n_elements(coef)))
;sigmax=sigmay/sqrt(total((coef*ii*(xmin(0)-data(0))^((ii-1)))^2))

;New estimate of minimum (derivative of the polynomial=0) and error
; Carlos UT Sep 2001
derivativecoef=shift(coef,-1)
derivativecoef=derivativecoef(0:n_elements(coef)-2)*$
	(findgen(n_elements(coef)-1)+1)
	
if order eq 3 then begin
	roots=[(-coef(2)+sqrt(coef(2)^2-3.*coef(3)*coef(1)))/3./coef(3),$
	(-coef(2)-sqrt(coef(2)^2-3.*coef(3)*coef(1)))/3./coef(3)]; I'll doit analitically for order=2
	if (max(where(imaginary(roots) eq 0)) eq -1) then begin
		print,'% mideshift_xy: Complex roots! I quit!'
			print,'% mideshift_xy: measurement failure'
			xmin=-1000
			ymin=-1000
			sigmax=-1000
			sigmay=-1000
			wavelength=keepwavelength & flux=keepflux
			if not keyword_set(noplot) then begin
				plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
			endif
		return
	endif
	if (max(where(finite(roots))) eq -1) then begin
		print,'% mideshift_xy: Not finite roots! I quit!'
			print,'% mideshift_xy: measurement failure'
			xmin=-1000
			ymin=-1000
			sigmax=-1000
			sigmay=-1000
			wavelength=keepwavelength & flux=keepflux
			if not keyword_set(noplot) then begin
				plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
			endif
		return
	endif	
endif else begin
	roots=fz_roots(derivativecoef,/double) ; this one get's them numerically
	if (max(where(imaginary(roots) eq 0)) eq -1) then begin
		print,'% mideshift_xy: Complex roots! I quit!'
			print,'% mideshift_xy: measurement failure'
			xmin=-1000
			ymin=-1000
			sigmax=-1000
			sigmay=-1000
			wavelength=keepwavelength & flux=keepflux
			if not keyword_set(noplot) then begin
				plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
			endif
		return
	endif
	if (max(where(finite(roots))) eq -1) then begin
		print,'% mideshift_xy: Not finite roots! I quit!'
			print,'% mideshift_xy: measurement failure'
			xmin=-1000
			ymin=-1000
			sigmax=-1000
			sigmay=-1000
			wavelength=keepwavelength & flux=keepflux
			if not keyword_set(noplot) then begin
				plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
			endif
		return
	endif	
	roots=double(roots)
endelse

xmin=roots(where(min(abs(roots)) eq abs(roots)))+data(0)
ymin=poly(xmin-data(0),coef)

;now error bars, two ways:
;1) sigma(x) directly from the rms scatter in the x axis     -> sigmax
;2) 1./sigma(x_i)^2 = sum_i (1./sigma(y_i)^2) (@y_i/@x_i)^2  -> sigmax2
; then we keep the mean of the two in sigmax

xfit=dblarr(npoints)  
for i=0,npoints-1 do begin
	obs=flux(pixelmin-nhalf+i)
	if (flux(pixelmin-nhalf+i) lt ymin(0)) then begin
		; if the observation is lower than the minimum of the line
		obs=obs+2.*(ymin-flux(pixelmin-nhalf+i))
	endif  
	coef2=coef
	coef2[0]=coef2[0]-obs[0]
	roots=fz_roots(transpose(coef2),/double)
	;plot,wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),$
	;flux(pixelmin-nhalf:pixelmin+nhalf)
	;oplot,wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),$
	;	poly(wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),coef),$
	;	col=140,thick=2
	;stop
	if (max(where(imaginary(roots) eq 0)) eq -1) then begin
		print,'% mideshift_xy: Complex roots! I quit!'
			print,'% mideshift_xy: measurement failure'
			xmin=-1000
			ymin=-1000
			sigmax=-1000
			sigmay=-1000
			wavelength=keepwavelength & flux=keepflux
			if not keyword_set(noplot) then begin
				plot,wavelength,flux,psym=2,title=data(0),charsize=1.7
			endif
		return
	endif
	roots=double(roots(where(imaginary(roots) eq 0)))
	xfit(i)=roots(where(abs(roots-wavelength(pixelmin-nhalf+i)+data(0)) eq $
	min(abs(roots-wavelength(pixelmin-nhalf+i)+data(0)))))
	;print,roots
	;print,'pick:',xfit(i),'   ref=',wavelength(pixelmin-nhalf+i)-data(0)
endfor

sigmax=stdev(xfit-wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0))
ydiff=poly(wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),coef)-$
	    flux(pixelmin-nhalf:pixelmin+nhalf)
xmin=xmin(0)
ymin=ymin(0)
sigmax=sigmax(0)/sqrt(npoints*1.)
sigmax2=poly(wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),derivativecoef)
sigmax2=total(1.d0/ydiff^2*sigmax2^2)
sigmax2=1./sqrt(sigmax2)

;print,sigmax,sigmax2,(sigmax+sigmax2)/2.0
sigmax=(sigmax+sigmax2)/2.0d0

endelse; 2pd if

if not keyword_set(noplot) then begin
	plot,wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),$
	flux(pixelmin-nhalf:pixelmin+nhalf),psy=8,$
	yr=[min(flux(pixelmin-nhalf:pixelmin+nhalf))*0.98,$
	max(flux(pixelmin-nhalf:pixelmin+nhalf))*1.02],$
	xr=[min(wavelength(pixelmin-nhalf:pixelmin+nhalf))-data(0)-0.01,$
	max(wavelength(pixelmin-nhalf:pixelmin+nhalf))-data(0)+0.01],$
	xstyle=1,ystyle=1,thick=2,charsize=2.,/nodata,$
	title=string(xmin,format='(f10.4)')
	
	plotsym,0,2,/fill
	oplot,wavelength(pixelmin-nhalf:pixelmin+nhalf)-data(0),$
	flux(pixelmin-nhalf:pixelmin+nhalf),psy=8
	ran=10*step
	xgrid=dindgen(ran/0.0001)*0.001+wavelength(pixelmin-nhalf)-data(0)
	if not keyword_set(tpb) then begin
		oplot,xgrid,poly(xgrid,coef),thick=2
		oplot,[xmin-data(0),xmin-data(0)],[ymin-sigmay,ymin+sigmay]
	endif	
	xx=[xmin-data(0),xmin-data(0)]
	yy=[0,1]
	oplot,xx,yy
	oplot,[xmin-data(0)-sigmax,xmin-data(0)+sigmax],[ymin,ymin],thick=2
endif

xx=[xmin,xmin]
yy=[0,1]

; get the input wave.-flux vectors back
wavelength=keepwavelength & flux=keepflux

endelse

end
