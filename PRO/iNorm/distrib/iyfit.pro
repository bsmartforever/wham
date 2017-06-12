;+
;				iYFIT.PRO
;				Version 1.0
;Program Description:
;	This procedure fits a Legendre polynomial continuum to a spectrum
;	and calculates associated errors as outline in Sembach & Savage 
;	1992.
;
;Restrictions:
;       Requires cursor (3 button mouse) input
;
;Screen Output: 
;	Text  &  Graphics
;
;Use:
;	IMYFIT,x,y,xarray,yarray,store,ycon,a,sigma,ycon_sig,ftflag
;
;On Input:
;		x	:== x coordinate array
;		y	:== y coordinate array
;	 ey :== ey coordinate array
;		xarray	:== x defined continuum array
;		yarray  :== y defined continuum array
;		store   :== stored continuum region array (2xn)
;On Output:
;		ycon	:== y calculated continuum array
;		a	:== coefficient array
;		sigma   :== sigma of fit
;		ycon_sig:== error on continuum points 
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	04/10/13  NL	- Version 1.0
;
;External Routines Called:
;	LEGERR		- to calculate Legenedre polynomial errors
;	LEGFIT		- to calculate Legendre polynomial fit
;	LEGPOLY		- to construct Legendre polynomial fit
;----------------------------------------------------------------------------
PRO iYFIT,x,y,ey,xarray,yarray,store,ycon,a,sigma,ycon_sig,ftflag

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imyfit' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Overplot defined continuum regions.
;
	PRINT,'iYFIT::  Current fit order: ', $
	STRING(N_ELEMENTS(a)-1,'(I1)')
LOOP:
loadct,39,/silent
	liney1=[!cymax/30.0,-!cymax/30.0]+0.9*!cymax
	FOR k=0,N_ELEMENTS(store)/2-1 DO BEGIN
		OPLOT,[store(0,k),store(0,k)],liney1,color= 200
		OPLOT,[store(1,k),store(1,k)],liney1,color= 200
		OPLOT,[store(0,k),store(1,k)],[1.0,1.0]*0.9*!cymax,color= 200
	ENDFOR
;
;Print heading and instructions.
;
	READ,'iYFIT::  Minimum order of fit (-1=clear,0=quit): ',minord 
	minord = FIX(minord)  
	IF minord EQ 0 THEN RETURN
	IF minord LT 0 THEN BEGIN
		PLOT,x,y  &  GOTO,LOOP
	ENDIF
	READ,'iYFIT::  Maximum order of fit: ',maxord  
	maxord = FIX(maxord) > minord
	PRINT,'iYFIT::  Working...'
;
;Check to be sure nord is not too large for array.
;
	IF maxord GE N_ELEMENTS(xarray) THEN GOTO,ESCAPE
	maxord = maxord < 9
	minord = minord < 9
;
;Scale xarray and x into appropriate range so that Legendre polynomial can be
;fit over the range from -1 to + 1.
;
	maxx = MAX(ABS(xarray))  &  !c = 0
	xarray = xarray/maxx
	x = x / maxx
;
;Get Legendre polynomial fit and form continuum over all x.
;
	LEGFIT,xarray,yarray,minord,maxord,yfit,a,eps,chi2
	ycon = LEGPOLY(x,a)
;
;Get sigma from Legenrde polynomial fit.  It is just square root of chi2 here.
;
	sigma = SQRT(chi2)
;
;Calculate mean signal to noise (added 09/01/89).
;
	sn = 0.0
	IF sigma NE 0.0 THEN sn = AVG(yfit)/sigma
;
;Calculate signal to noise at 0 km/s.
;
	loc0 = SORT(ABS(x)) & loc0 = loc0(0)
	sn0 = ycon(loc0) / sigma
;
;Overplot the derived continuum as a dashed curve.
;
	!p.linestyle = 2  &  OPLOT,x*maxx,ycon,color= 190	&  !p.linestyle = 0
;
;Calculate the error bars on the continuum due to errors in the coefficients
;of the Legendre polynomial fit.
;
	LEGERR,x,y,a,eps,chi2,ycon_sig	;chi2 = variance (=sigma^2) here
;
;Convert x and xarray back into the correct units.
;
	x = x * maxx
	xarray = xarray * maxx
;
;Bound ycon so that plotting errors and arithmetic errors aren't important.
;
	maxy = MAX(y) & miny = MIN(y)
	ycon = ycon > (-ABS(miny)*2) < (maxy*2)
;
;Print the fitting statistics to upper corner of screen.
;
	PRINT,'iYFIT::  Information Dump Follows'
	PRINT,'----------------------------------------'
	PRINT,'Order used = ',maxord
	PRINT,'# points   = ',STRING(N_ELEMENTS(yarray),'(I8)')
	PRINT,'RMS sigma  = ',STRING(sigma,'(E9.3)')
	PRINT,'Mean S/N   = ',STRING(sn,'(F8.2)')
	PRINT,'Mean S/N(0)= ',STRING(sn0,'(F8.2)')
	PRINT,'----------------------------------------'
	PRINT,'iYFIT::  End of Information Dump'
        PRINT,'iYFIT::  Hit any key to continue'
        choice = GET_KBRD(1)
        ftflag=1
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'iYFIT::  '+!err_string
	RETURN  &  END
