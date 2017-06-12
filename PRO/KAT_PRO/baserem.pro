;+
;*NAME:
;   	BASEREM     30 SEPTEMBER 1980
;  
;*PURPOSE:
;   	To fit a polynomial of order NDEG to a user-specified
;   	spectral region for removing a baseline from a spectral feature.
;  
;*CALLING SEQUENCE:
;   	BASEREM,x,y,ndeg,xl,xr,a,YBF,CHIS
;  
;*PARAMETERS:
;    	X	(REQ) (I) (1) (I L F D)
;		Array of independent variables.
;    	Y	(REQ) (I) (1) (I L F D)
;		Array of dependent variables.
;    	NDEG	(REQ) (I) (1) (I)
;		Degree of polynomial used to approximate baseline.
;    	A	(REQ) (I/O) (1) (F)
;		Floating point array with at least NDEG+1 elements .
;        	(polynomial coefficients are written into the last NDEG+1 
;		elements).
;  		Input vector A with added coefficients as described above
;		(output).
;	XL,XR   (REQ) (I/O) (0) (I L F D)
;		Scalar values which if non-zero are used as the left & right
;         	coordinates of the excluded region. If zero, the user is 
;		prompted for these values via terminal cursor positions.
; 
;    	YBF	(REQ) (O) (1) (I L F D)
;		Values of Y produced by polynomial.
;		
;    	CHIS	(REQ) (O) (0) (I L F D)
;		Variance of the fit (as calculated by WPOLYFIT without 
;		weighting).
;  
;*INTERACTIVE INPUT:
;
;    	User is prompted for cursor positions of spectral feature region not 
;    	to be included in baseline calculation (if XL & XR are initially 
;	zero).
;  
;*SUBROUTINES:
;    	WPOLYFIT
;    	PARCHECK
;  
;*NOTES:
;
;  -  	BASEREM will prompt user for cursor positions until the number of points
;    	between the limits is greater than the number of elements in vector A
;    	plus NDEG-2. 
;  -  	BASEREM is used by GAUSSFIT for removing a baseline of a Gaussian 
;       feature. In GAUSSFIT, the vector A has NDEG + 1 + 3*NCOMP elements
;    	where NCOMP is the number of Gaussian components in the input array.
;  -  	The points selected by the user to designate the edges of the feature
;    	can be specified in any order. These points are INCLUDED in the baseline
;    	fit.
;  -  	When BASEREM is used to fit the background of a region in which several
;    	Gaussians are to be fit, the user still specifies one set of endpoints.
;    	The endpoints should represent the leftmost and rightmost sides of the
;    	features. 
;  -  	BASEREM use to be restricted to fits of less than 3rd order when
;    	used on the PDP computer. Higher fits are now possible but depending on 
;    	the magnitude of the input X and Y arrays, problems may occur. If 
;    	errors occur the user may want to try rerunning BASEREM after scaling 
;    	the input X and/or Y arrays. For most IUE applications, rescaling the
;    	flux array is necessary for high order fits.
;  
;*PROCEDURE:
;
;    	BASEREM extracts the  region excluding that described by the cursor
;    	positions (or XL and RL), and uses WPOLYFIT to calculate a polynomial 
;    	fit, the YBF values, and the reduced chi square.If NDEG = 0, BASERERM
;    	returns 1 element in the vector A simply representing the average value
;    	of the baseline region. Since the weighting vector passed to WPOLYFIT
;    	is set to ones, the CHIS parameter is simply the variance of the fit.
;  
;*EXAMPLES:
;
;    	To fit a baseline with a 5th order polynomial:
;      	A = FLTARR(9)     ; 6 for baseline, 3 for a possible Gaussian feature
;	xl= 0
;	xr= 0
;      	BASEREM,W,F,5,xl,xr,A,YBF,CHISQ
;  
;*MODIFICATION HISTORY:
;
;     	PDP VERSION: I. DEAN AHMAD (modified VAX version: R. Thompson)
;     	7-16-84 RWT updated documentation & made user interaction optional
;     	8-8-84  RWT defined FXL & FXR for non-interactive mode 
;     	11-8-85 RWT RETALL used for 1st RETURN & NELEMENTS & # added for DIDL
;      	4-13-87 RWT VAX mods: add PARCHECK, replace TEKDATA with CURSOR, 
;		replace TKPLOT & XYOUT with PLOTS, XYOUTS, & SCTODC, use 
;		assignment statements.
;     	10-28-87 RWT remove restriction of NDEG being <3, allow sides of 
;		feature to be specified in any order, add procedure call 
;		listing, add PLOT,Y, add endpoints to baseline array, remove 
;		FXR, FXL, YL & YR calculations, and remove oplot and listing 
;		of CHIS.
;        8-22-89 RWT Unix mods: remove SCTODC & HARDCOPY, store coords.
;               in arrays, add get_kbrd
;        Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;************************************************************************
pro baserem,x,y,ndeg,xl,xr,a,ybf,chis
;
npar = n_params()
if npar eq 0 then begin
    print,' BASEREM,X,Y,NDEG,XL,XR,A,YBF,CHIS'
    retall & end
parcheck,npar,8,'baserem'
xl = float(xl)
xr = float(xr)
npts = n_elements(x)  
sa   = n_elements(a)
nterms = sa - ndeg - 1                     ; # of terms for gaussians
if ((xl+xr) eq 0.0) then begin  ; prompt user for coordinates
  xc = fltarr(2)
  yc = xc
  plot,y
   if (!d.name eq 'TEK') then  $
   st = 'place cursor at sides of feature; press any key ' else $
   st = 'place cursor at sides of feature; press any button on mouse '
  xyouts,!d.x_ch_size,!d.y_size-!d.y_ch_size,font=0,/device,st 
  repeat begin
     cursor,xl,yl,1,/data
     flush = get_kbrd(0)
     wait,0.1
     cursor,xr,yr,1,/data
     flush = get_kbrd(0)
     xc = [xl,xr]
     yc = [yl,yr]
     xc = xc(sort(xc))
     yc = yc(sort(yc))
     xc = fix(xc + 0.5)
     xc = xc > 1 < x(npts-2)
     if (xc(1) - xc(0)) le 3 then print,'error in gaussfit:',  $
        '  set endpoints further apart'
     end until (xc(1)-xc(0))+1 gt nterms  ;  end repeat
  xl = xc(0)
  xr = xc(1)
  end 
newn = npts-xr+xl
if (newn lt ndeg) then begin
  print,'not enough baseline points specified for desired fit'
  retall & end
;
; extract baseline segments from outside feature region
;
ybase=fltarr(newn+1)
xbase=ybase
ybase(0) = y(0:xl)
ybase(xl+1) = y(xr:*)
xbase(0) = x(0:xl)
xbase(xl+1) = x(xr:*)
;
; unless ndeg is negative, determine baseline
;
if ndeg ge 0 then begin
   wpolyfit,xbase,ybase,0*ybase+1.,ndeg,ab,ybf,chis   ;fit polynomial
   a(nterms) = ab(0:ndeg)      ; store baseline params at end of a
   end else chis=total(ybase*ybase)/(newn-1)
print,' '
return
end