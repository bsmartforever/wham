;
; voi and voi2 are auxiliary functions used by getew_xy.pro
;
pro voi,x,b,f
common cte,center,height,continuum
f=b(0)*voigt(b(1),(x-center)*center^2/b(2)) + continuum
end
;
pro voi2,x,b,f
common cte,center,height,continuum
f=b(0)*voigt(b(1),(x-center)*center^2/b(2)) + b(3)
end
;
pro getew_xy,x,y,ew,a=a,b=b,noplot=noplot,cont=cont,restrict=restrict,$
	yerror=yerror,_extra=e
;+
;
;	Automatic determination of the EW of a spectral line by fitting 
;	Gaussian and Voigt profiles. The fits are forced to match the continuum
;	at 1.0 and, in the case of a Voigt profile, the center of the line at 
;	is fixed to the position determined by the Gaussian fit.
;
;	IN: x	- 	flt/dblarr	Vector with wavelengths 
;	    y	-	flt/dblarr	Vector with normalized flux
;
;	OUT: ew	- 	dblarr(2) Equivalent width determined from the Gaussian
;			and the Voigt fits (angstroms). A zero value is returned
;			when the measurement could not be performed.
;
;	KEYWORDS: a	float		Gaussian parameters
;					f=a(0)*exp(-((x-a(1))/a(2))^2./2) + a(3)
;		  b	flt array(2)	Voigt profile parameters
;					f=b(0)*voigt(b(2),(x-b(1))*b(1)^2/b(3))+b(4)
;
;		cont    		when switched on, the continuum location
;					becomes a fitting parameter (otherwise
;					fixed to 1.0)
;
;		restrict		when on, the initial search for the 
;					line center (mideshift_xy) is restricted
;					to 'restrict' from the mean of x 
;					(same units as in x)
;	
;		yerror		std. deviation between data and model in the area
;					used for least-squares fitting. As ew, this is a 2-element
;					array, with the first element for the Gaussian fit and the
;					second for the Voigt fit
;
;		Extra keywords are passed along to plot (e.g. yrange, charsize, etc.)
;
;
;	Note: the numerical integration of the fits goes on only out to the
;		limits of the input wavelength window. The only points taken
;		into account in the fit are those from the line center up to
;		the first place where the slope changes sign (to avoid blends).
;
;		The routine curvefit.pro, which is used by this procedure,
;		will sometimes crash when there are not enough points to fit.
;		A modified curvefit.pro (see callende library) that will exit
;		more graciously is recommended for batch applications.
;
; Carlos Allende Prieto, UT, Oct 2001
;		       , UT, April 2006, Voigt fitting enabled, 'height'
;				and 'continuum' are included as fitting
;				parameters, and so is 'center' for the 
;				Gaussian case. Keywords 'cont' and 
;				'restrict' added
;				, UT, February 2009, yerror keyword added
;				, IAC, July 2011, accepted extra keywords for 'plot' 
;
;-

itmax=100
common cte,center,height,continuum

npar = n_params()
if (npar eq 0) then begin
	print,'use -- getew_xy,x,y,ew[,a=a,b=b,cont=cont,restrict=restrict,yerror=yerror,_extra=e]'
	return
endif 
ew=dblarr(2)

if keyword_set(restrict) then w=where(abs(x-mean(x)) le restrict) else $
	w=indgen(n_elements(x))
	
mideshift_xy,x[w],y[w],xmin,ymin,xsig,/noplot,/int,/nois
if (xmin eq -1000) then begin
	print,'% GETEW_XY: no minimum could be measured by mideshift'
	return
endif

height=-(1.-ymin) & center=xmin & continuum=1.00

if not keyword_set(noplot) then begin
plotsym,0,0.6,/fill
plot,x,y,xr=[min(x),max(x)],yr=[min(y)*.9,1.03],ystyl=1,xstyle=1,psy=8,$
xtitle='!3Wavelength ('+string("305B)+')',ytitle='Normalized flux',charsiz=1.4,_extra=e

xyouts,0.7,0.03,systime(0),/norm,charsize=1.

endif

; cleaning - from the line center we go up only until the slopes change
; sign (leaving a margin from the center)
margin=4e-6
blue_limit=max(where(x lt xmin*(1.-margin) and deriv(x,y) ge 0.0))
red_limit=min(where(x gt xmin*(1.+margin) and deriv(x,y) le 0.0))
if (blue_limit(0) eq -1) then blue_limit=0 
if (red_limit(0) eq -1) then red_limit=n_elements(x)-1
sy=y(blue_limit:red_limit)
sx=x(blue_limit:red_limit)
;oplot,replicate(xmin*(1.-margin),2),[0,2]
;oplot,replicate(xmin*(1.+margin),2),[0,2]

if keyword_set(cont) then begin
	f=gaussfit(sx,sy,a,nter=4,yerror=yerror1)
	gaussianmia,x,a,f
	ew(0)=int_tabulated(x,a[3]-f)
endif else begin
	f=gaussfit(sx,1.-sy,a,nter=3,yerror=yerror1)
	gaussianmia,x,[a,0.0],f
	ew(0)=int_tabulated(x,f)
	f=1.-f
	a=[-a(0),a(1:2),continuum]
endelse

;update value of the center, which will be FIXED
;for the Voigt fitting
center=a(1)

if not keyword_set(noplot) then begin
	oplot,sx,sy,thick=7,col=30
	oplot,x,f,col=80,thick=3	
endif
 

weights=replicate(1.0d0,n_elements(sx))
if keyword_set(cont) then begin
	b=dblarr(4)
	b(0)=height & b(1)=0.034237377 & b(2)=2311136.7 & b(3)=1.00
	r=curvefit(sx,sy,weights,b,sigma,function_name='voi2',/noder,itmax=itmax,iter=it,yerror=yerror2)
	voi2,x,b,f
	ew(1)=int_tabulated(x,b(3)-f)
	b=[b(0),center,b(1:3)]
endif else begin
	b=dblarr(3)
	b(0)=height & b(1)=0.034237377 & b(2)=2311136.7 & continuum=1.00
	r=curvefit(sx,sy,weights,b,sigma,function_name='voi',/noder,itmax=itmax,iter=it,yerror=yerror2) 
	voi,x,b,f
	ew(1)=int_tabulated(x,1.-f)
	b=[b(0),center,b(1:2),continuum]
endelse
if (n_elements(it) lt 1) then begin
	ew(1)=0.0d0
	print,'% GETEW_XY: Voigt fit did not succeed'
	return
endif
if (it gt itmax) then begin
	ew(1)=0.0d0
	print,'% GETEW_XY: Voigt fit did not converge after ',itmax,' iterations'
	return
endif
if not keyword_set(noplot) then begin
	oplot,x,f,col=140,thick=3
endif

yerror=[yerror1,yerror2]

end





