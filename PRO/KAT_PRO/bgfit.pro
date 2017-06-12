;+
;				bgfit.PRO
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
;	bgfit,x,y,xarray,yarray,region,ycon,a,sigma,ycon_sig
;
;On Input:
;		x	:== x coordinate array
;		y	:== y coordinate array
;	 ey :== ey coordinate array
;		xarray	:== x defined continuum array
;		yarray  :== y defined continuum array
;		region   :== regiond continuum region array (2xn)
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
PRO bgfit,x,y,ycon,sigma,$
	ey=ey,ycon_sig=ycon_sig,$
	region=region,minord=minord,maxord=maxord,quiet=quiet

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'bgfit' & RETURN & ENDIF


;region to fit the continuum
;xarray,yarray

;region of the continuum array as [2,n] array
;region

if (NOT keyword_set(region)) then region=[min(x),max(x)]
if N_ELEMENTS(region[*,0]) ne 2 then begin 
	print,''
	print,'*** Region must be a [2,n] sized array ***'
	print,''
	GOTO,ESCAPE
endif


index=where((x ge region[0,0]) AND (x le region[1,0]),count)
if count eq 0 then begin
	print,'*** Invalid range: ',region[0,0],region[0,1]
	GOTO,ESCAPE
endif 
if N_ELEMENTS(region[0,*]) gt 1 then begin
	for i=0,N_ELEMENTS(region[0,*])-1 do begin
		index_tmp=where((x ge region[0,i]) AND (x le region[1,i]),count)	
		if count ne 0 then index=[index,index_tmp] $
		else begin
			print,'*** Invalid range: ',region[0,0],region[0,1]
			print,'*** Skipping region ***'
		endelse 

	endfor
endif

xarray=x[index] & yarray=y[index]
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Overplot defined continuum regions.
;
	;PRINT,'bgfit::  Current fit order: ', $
	;STRING(N_ELEMENTS(a)-1,'(I1)')
LOOP:
loadct,39,/silent
;
;Print heading and instructions.
;
	if (NOT keyword_set(minord)) then minord=1. else minord = FIX(minord)
	if (NOT keyword_set(maxord)) then maxord=11. else maxord = FIX(maxord)

	if minord le 0 then minord=1
	maxord = FIX(maxord) > minord
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

if (NOT keyword_set(quiet)) then begin
	;
	;Print the fitting statistics to upper corner of screen.
	;
		PRINT,'bgfit::  Information Dump Follows'
		PRINT,'----------------------------------------'
		PRINT,'Order used = ',maxord
		PRINT,'# points   = ',STRING(N_ELEMENTS(yarray),'(I8)')
		PRINT,'RMS sigma  = ',STRING(sigma,'(E9.3)')
		PRINT,'Mean S/N   = ',STRING(sn,'(F8.2)')
		PRINT,'Mean S/N(0)= ',STRING(sn0,'(F8.2)')
		PRINT,'----------------------------------------'
		PRINT,'bgfit::  End of Information Dump'
	        PRINT,'bgfit::  Hit any key to continue'
	        choice = GET_KBRD(1)
	        ftflag=1
		RETURN
	;------------------------------------------------------------------------------
endif

	RETURN 

ESCAPE:
	PRINT,'bgfit::  '+!err_string

	RETURN  &  END
