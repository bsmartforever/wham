pro 	xc,y1,y2,s1,s2,delta,e_delta,nrange=nrange,npoints=npoints,$
		order=order,plot=plot,gauss=gauss,fourier=fourier,$
		ycutoff=ycutoff,_extra=e

;+
;	Determination of the maximum of the cross-correlation function
;	of two equal-length vectors by fitting a polynomial or a Gaussian
;
;	IN: y1 	- 	fltarr		1st vector
;	    y2  -	fltarr		2nd vector
;	    s1	-	fltarr		error 1st vector
;	    s2  -	fltarr		error 2nd vector
;
;	OUT: delta	float		shift to be applied to y2 to match y1 
;					(max. of cross-correlation function)
;					(units are pixels)
;
;	    e_delta	float		error in delta  (pixels)
;
;	KEYWORDS: nrange integer	half range of the shifts in the 
;					cross-correlation integral 
;
;	    npoints- number of pixels around the minimum to enter the fit.
;			(default: 7)
;
;	    order  - order of the polynomial: 2 or 3 (default: 2 = 2nd order)
;			A gaussian can also specified with order<0 (or by
;			using the keyword 'gauss'.
;
;	    gauss  - use a Gaussian instead of a polynomial to model the
;			peak of the cross-correlation function
;
;	    fourier - compute the cross-correlation in Fourier space
;			(nrange is n_elements(y1)/2-1 regardles of input nrange)
;
;	    ycutoff - use this keyword to set a lower limit to the fluxes
;					included in the cross-correlation (this is 
;					incompatible with the 'fourier' keyword)
;
;	    extras - extra plotting keywords can be used and will be passed
;			along to plot
;
;
;	NOTES: By default, the  cross-correlation runs through almost the full 
;	length of the vectors by using the shift function on the 2nd vector:
;	The integration of the cross-correlation is therefore going from
;	-nrange to nrange, where nrange=n_elements(y)/2-1, but nrange can be
;	reduced when the cross-correlation is performed in pixel space.
;	When working on pixel space, the part of the second vector shifted 
;	which exceeds the vector's length on one side is pasted on the other 
;	side by calling the 'shift' function (i.e. the vectors are assumed
;	periodic).
;
;	C. Allende Prieto, UT@Austin, Nov 2004
;			 	    , Oct 2006 -- improved error calculation
;			 	    , Dec 2006 -- added Gaussian model
;				    , July 2009 -- added ycutoff keyword
;-

;set failure values for delta/e_delta beforehand
delta=-1d6
e_delta=-1d6

;checks
if N_params() LT 2 then begin
      print,'% XC: - xc,y1,y2,s1,s2,delta,e_delta[,nrange=nrange,'
      print,'% XC:   	npoints=npoints,order=order,plot=plot,gauss=gauss]'
      return
endif

nel=n_elements(y1)
if (nel ne n_elements(y2)) then begin
      print,'% XC:  - error: the two vectors have different dimensions'
      return
endif
if n_elements(s1) ne nel or n_elements(s2) ne nel then begin
      print,'% XC:  - error: the dimension of the error arrays does not match'	
      return
endif

if not keyword_set(nrange) then begin
nrange=nel/2-1 ; this is the number of pixels we'll shift one way and the other
endif else begin
if nrange gt nel/2-1 then begin
      nrange=nel/2-1
      print,'% XC:  - warning: nrange was larger than the length of the arrays'	
      print,'% XC:  - and was reset to n_elements(y)/2-1'	      
endif
endelse

if not keyword_set(order) then order=2 else begin
if (order ne 2 and order ne 3) then begin
      if (order ge 0) then begin
        print,'% XC:  - error: the order for the polynomial fitting can be 2 or 3'
        return
      endif
endif
endelse
if keyword_set(gauss) then order=-1
if not keyword_set(npoints) then begin
	;take a guess!
	if order lt 0 then npoints=39 else npoints=7
endif

if (abs(npoints - fix(npoints))*1d10 gt 1.d0)  then begin
        print,'% XC:  - error: np must be an integer'
        return
endif

if (npoints gt nel)  then begin
        print,'% XC:  - error: np > number of elements of the input arrays!'
        return
endif


;block 1: compute the cross-correlation -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

if not keyword_set(fourier) then begin

t=fltarr(2*nrange+1) 
s=fltarr(2*nrange+1) 

if keyword_set(ycutoff) then begin
	ind=where(y1[0:nel-1] gt ycutoff)
endif else ind=indgen(nel)

for i=-nrange,nrange do begin		
	yy=shift(y2,i)
	ss=shift(s2,i)
	t[i+nrange]=total(y1[ind]*yy[ind])
	s[i+nrange]=total( $
	y1[ind]^2*ss[ind]^2 +$
	yy[ind]^2*s1[ind]^2)	
endfor
t=t[0:nrange*2]
s=s[0:nrange*2]
s=sqrt(s)
x=indgen(n_elements(t))-nrange

endif else begin
;compute the cross-correlation in Fourier space

if keyword_set(ycutoff) then begin
	print,'% XC: warning -- the KEYWORD ycutoff cannot be used in Fourier space'
	print,'% XC:         It will be ignored!'
endif

if nrange lt nel/2-1 then begin
      print,'% XC:  - warning: nrange was reset to its maximum value ',nel/2-1
      print,'% XC:  as the calculation is performed in Fourier space'
endif
nrange=nel/2-1

ty1=fft(y1,-1)
ty2=fft(y2,-1)
tys1=fft(y1^2,-1)
tys2=fft(y2^2,-1)
ts1=fft(s1^2,-1)
ts2=fft(s2^2,-1)
t=nel*shift(abs(fft(ty1*conj(ty2),1)),nel/2-1)
s=nel*(shift(abs(fft(tys1*conj(ts2),1)),nel/2-1) + $
	shift(abs(fft(ts1*conj(tys2),1)),nel/2-1))
t=t[0:nrange*2]
s=s[0:nrange*2]
s=sqrt(s)
x=indgen(n_elements(t))-nrange

endelse

;block 2: measuring the line shift      -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

x0=where(t eq max(t))
nhalf=(npoints-1)/2

;checking that the maximum does not involve too many pixels or none
nx0=n_elements(x0) 
if max(x0) le -1 or n_elements(x0) gt nhalf then begin
	print,'% XC:  - cannot find a peak in the cross-correlation function'
	delta=-1d6
	e_delta=-1d6
	return	
endif else begin
	if (nx0 gt 1) then $
	print,'% XC: warning the maximum of the ccf is not a single-pixel'
endelse
;and that we have enough points
if (min(x0)-nhalf lt 0 or max(x0)+nhalf gt n_elements(x)-1) then begin
	print,'% XC:  - not enough points to FIT'
	delta=-1d6
	e_delta=-1d6
	return
endif

;finding the central pixel from weighted average in a symmetric window
xw=total(t[x0[0]-nhalf:x0[0]+nhalf]*x[x0[0]-nhalf:x0[0]+nhalf])/$
	total(t[x0[0]-nhalf:x0[0]+nhalf])+nrange
x0=round(xw)

t0=t[x0]

nhalf1=nhalf	; points on the left side of the maximum
nhalf2=nhalf 	; points on the right side of the maximum
; dealing with even values of npoints
if (npoints/2 eq round(npoints/2.)) then begin
	if (xw gt x0) then nhalf2=nhalf2+1 else nhalf1=nhalf1+1	
endif

;fail safe
if nhalf1+nhalf2+1 ne npoints then stop,'% XC: something is wrong!'

;check that we have enough points
if (x0-nhalf1 lt 0 or x0+nhalf2 gt n_elements(x)-1) then begin
	print,'% XC:  - not enough points to FIT'
	delta=-1d6
	e_delta=-1d6
	return
endif

if order lt 0 then begin
	;Gaussian

	;F(x) = a0*EXP(-z^2/2) + a3	z=(x-a1)/a2	;gaussfit names
	;g(x) = a2*EXP(-z^2/2) + a1	z=(x-a3)/a4	;my names (paper)
	;my ai=aa(i-1)					;array aa here
	
	c=gaussfit(x[x0-nhalf1:x0+nhalf2],t[x0-nhalf1:x0+nhalf2],nterms=4,$
	;measure_errors=s[x0-nhalf1:x0+nhalf2],$ ;somewhat less stable
	estimates=[max(t[x0-nhalf1:x0+nhalf2])-median(t[x0-nhalf1:x0+nhalf2]),$ 
		   x[x0],nhalf1/20.,median(t[x0-nhalf1:x0+nhalf2])],a)

	aa=[a[3],a[0],a[1],a[2]]
				
endif else begin
	;Polynomial
	
	c=poly_fit(x[x0-nhalf1:x0+nhalf2],t[x0-nhalf1:x0+nhalf2],order,$
	measure_errors=s[x0-nhalf1:x0+nhalf2],covar=covar,/double)
endelse

;block 3: estimating error bars      -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

if order lt 0 then begin

	;covar (the covariance matrix) is computed internally by poly_fit
	;but not by gaussfit (as of idl_6.1)
	
	;F(x) = a0*EXP(-z^2/2) + a3	z=(x-a1)/a2	;gaussfit names
	;g(x) = a2*EXP(-z^2/2) + a1	z=(x-a3)/a4	;my names (paper)
	;my ai=aa(i-1)					;array aa here
	
	;GAUSS_FUNCT,x[x0-nhalf1:x0+nhalf2],a,tmp,pder
	
	covar=dblarr(4,4)      ;covar will hold the curvature matrix for now
	for i=0,3 do begin
		for j=0,3 do begin
			;using my order for the parameters (aa)
			covar[i,j]=0.d0
			for k=0,npoints-1 do begin
				z=(x[x0-nhalf1+k]-aa[2])/aa[3]
				if (i eq 0) then Pi=1.0d0 else begin
					Pi=z^(i-1)*exp(-z^2/2.d0)
				endelse
				if (i eq 2 or i eq 3) then Pi=Pi*aa[1]/aa[3]
				if (j eq 0) then Pj=1.0d0 else begin
					Pj=z^(j-1)*exp(-z^2/2.d0)
				endelse
				if (j eq 2 or j eq 3) then Pj=Pj*aa[1]/aa[3]
				
				covar[i,j]=covar[i,j]+ $
				1.d0/s[x0-nhalf1+k]^2*Pi*Pj				
			endfor
		endfor
	endfor
		
	;now derive the covariance matrix
	covar=invert(covar,status,/double)
	if status ne 0 then begin
		if status eq 1 then begin
			print,'% XC: ERROR- status from invert is 1, singular array!'
			return			
		endif else begin
			print,'% XC: WARNING- status from invert is not 0, but ',status
		endelse
	endif
	
	delta=aa[2]
	e_delta=covar[2,2] 
	e_delta=sqrt(e_delta)
	
endif
	
if order eq 2 then begin

	delta=-c[1]/2.d0/c[2]
	e_delta=1.d0/4.d0/c[2]^2*(covar[1,1] + c[1]^2/c[2]^2*covar[2,2]) - $
	 c[1]/2.d0/c[2]^3*covar[1,2]		
	e_delta=sqrt(e_delta)
	
endif	 

if order eq  3 then begin

	beta=c[2]^2-3.d0*c[1]*c[3]
	if beta lt 0.0d0 then begin
		print,'% XC: - negative discriminant in 2nd order equation'
		delta=-1d6
		e_delta=-1d6
		return		
	endif
	beta=sqrt(beta)
	
	;straightforward calculation
	;x1=(-c[2]+beta)/3.d0/c[3]
	;x2=(-c[2]-beta)/3.d0/c[3]
	;px1a2= - 1.d0/2.d0/beta
	;px1a3= 1.d0/3.d0/c[3]*(-1.d0 + c[2]/beta)
	;px1a4= -(c[1]/2.d0/c[3]/beta + (-c[2] + beta)/3.d0/c[3]^2)
	;px2a2=  1.d0/2.d0/beta
	;px2a3= 1.d0/3.d0/c[3]*(-1.d0 - c[2]/beta)
	;px2a4= -(-c[1]/2.d0/c[3]/beta + (-c[2] - beta)/3.d0/c[3]^2)
	;print,x1,x2
	;print,px1a2,px1a3,px1a4,px2a2,px2a3,px2a4
	
	;rearranged for numerical stability
	;print,'a_3=',c[2]
	gamma=1.d0 + beta/abs(c[2])
	x1=-c[2]*gamma/3.d0/c[3]
	x2=-c[1]/c[2]/gamma
	px1a2= c[2]/2.d0/beta/abs(c[2])
	px1a3= - c[2]^2*gamma/3.d0/c[3]/beta/abs(c[2])
	px1a4= c[1]*c[2]/2.d0/c[3]/beta/abs(c[2]) + c[2]*gamma/3.d0/c[3]^2
	px2a2= -1.d0/c[2]/gamma *(1.d0 + 3.d0*c[1]*c[3]/2.d0/beta/abs(c[2])/gamma)
	px2a3= c[1]/beta/abs(c[2])/gamma
	px2a4= - 3.d0*c[1]^2/2.d0/c[2]/beta/abs(c[2])/gamma^2
		
	;print,x1,x2
	;print,px1a2,px1a3,px1a4,px2a2,px2a3,px2a4	
		
	wroot=where(abs([x1,x2]-x0[0]+nrange) eq min(abs([x1,x2]-x0[0]+nrange)))
	if (wroot[0] eq 0) then begin
		delta=x1
	        e_delta=px1a2^2*covar[1,1] + px1a3^2*covar[2,2] + $
	        	px1a4^2*covar[3,3] + $
		2.d0*px1a2*px1a3*covar[1,2] + 2.d0*px1a2*px1a4*covar[1,3] + $
		2.d0*px1a3*px1a4*covar[2,3]
		e_delta=sqrt(e_delta)		
	endif else begin
		delta=x2
	        e_delta=px2a2^2*covar[1,1] + px2a3^2*covar[2,2] + $
	        	px2a4^2*covar[3,3] + $
		2.d0*px2a2*px2a3*covar[1,2] + 2.d0*px2a2*px2a4*covar[1,3] + $
		2.d0*px2a3*px2a4*covar[2,3]	
		e_delta=sqrt(e_delta)			
	endelse
	
	if (max(where(imaginary(delta) eq 0)) eq -1) then begin
		print,'imaginary roots!'
		delta=-1d6
		e_delta=-1d6
		return						
	endif

	
endif

;block 4: plotting, if requested     -.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

if keyword_set(plot) then begin
ploterror,x[x0-nhalf1:x0+nhalf2],t[x0-nhalf1:x0+nhalf2],$
	s[x0-nhalf1:x0+nhalf2],psy=4,xr=[x[x0-nhalf1-1],x[x0+nhalf2+1]],/xstyl,$
	yr=[min(t[x0-nhalf1:x0+nhalf2])*0.9992,max(t[x0-nhalf1:x0+nhalf2])*1.0008],$
	ytit='C(x)',xtit='x',charsi=1.8,yminor=2,thick=2,charthick=2,$
	xthick=2,ythick=2,_extra=e
xx=interpol(x,n_elements(x)*10.)
if order ge 0 then tt=poly(xx,c) else GAUSS_FUNCT,xx,a,tt
oplot,xx,tt,thick=2
oplot,[delta,delta],[-1e6,1e6],linestyle=2,thick=2
oplot,[delta-e_delta,delta+e_delta],[t0,t0],linestyle=2,thick=2

;oplot,x[x0-nhalf1:x0+nhalf2],c,thick=2,col=140

endif

end
