;+
;Kenneth Sembach
;				LEGFIT.PRO
;
;Created: 04/23/91
;Last Revised: 05/02/99
;
;Program Description:
;	This procedure calculates a Legendre polynomial fit to data.  Derived
;       from procedure similar to that given by Bevington (1969).
;
;Restrictions:
;	None
;
;Screen Output:
;	None
;
;Use:
;	LEGFIT,x,y,minord,maxord,yfit,a,eps,chi2
;	
;On Input:
;		x	:== abscissa array
;		y	:== ordinate array
;		minord	:== minimum order to start fit
;		maxord	:== maximum order to terminate fit
;
;On Output:
;		yfit	:== fitted array (over x)
;		a	:== coefficients of fit
;		eps	:== error matrix
;		chi2	:== chi-squared for fit (by definition = sigma^2 here)
;                           We use uniform weighting = 1 here, and let sigma
;                           be goodness of fit.
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/08/92  KRS	- Now runs under Version 2 IDL.
;	05/02/99  KRS	- Documentation updates
;
;External Routines Called:
;	FTEST		- to check for need for another term in polynomial
;------------------------------------------------------------------------------
PRO LEGFIT,x,y,minord,maxord,yfit,a,eps,chi2
	
        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'legfit' & RETURN & ENDIF
;
;Flags and counters.
;
	nflag = 0
	nord = maxord
;
;Array subscript length and vector. 
;
	nx = N_ELEMENTS(x)
	ix = INDGEN(nx)
;
;Form legendre polynomial.
;
	p = FLTARR(nx,maxord+1)
	p(ix) = 1.
	p(ix+nx) = x
        FOR j=2.,maxord DO $
         p(ix+j*nx) = ((2.*j-1.)*x*p(*,j-1)-(j-1)*p(*,j-2))/j
;
;Begin loop to do fit.
;
	FOR nord=minord,maxord DO BEGIN

	   LOOP:
		ncoeff = nord + 1
	   ;
	   ;Form alpha and beta matrices.
	   ;
		beta = FLTARR(nord+1)
		alpha = FLTARR(nord+1,nord+1)
		FOR k=0,nord DO beta(k) = TOTAL(y*p(*,k))
		FOR k=0,nord DO BEGIN
			FOR j=0,nord DO alpha(j,k)=TOTAL(p(*,j)*p(*,k))
		ENDFOR
 	   ;
	   ;Invert alpha matrix ==> error matrix eps.
	   ;
		eps = INVERT(alpha)
	   ;
   	   ;Calculate coefficients and fit.
	   ;
		a = beta # eps
		yfit = FLTARR(nx)
		FOR j=0,nord DO yfit = yfit + a(j) * p(*,j)
	   ;
	   ;Calculate chi squared of fit - uniform weighting=1.
	   ;
;		sigma = SQRT(TOTAL((y-yfit)^2)/(nx-ncoeff-1))
		chisq = TOTAL((y-yfit)^2) 
		chi2 = chisq / (nx-ncoeff-1)
	   ;
	   ;Check chi2 against previous chi2 to see if additional term should 
	   ;be kept.  Check probability for "95% confidence".
	   ;
		IF nflag EQ 1 THEN GOTO,OUT
		IF nord GT minord THEN BEGIN
			f = (chisq1-chisq)/chi2
			fcutoff = FTEST(nx-ncoeff-1,0.05) 
			IF f LT fcutoff THEN BEGIN
				nord = nord - 1
				nflag = 1
				GOTO,LOOP  ;back up to top to do it over again
			ENDIF
		ENDIF
		chisq1 = chisq
	ENDFOR
OUT:
	maxord = maxord < nord
;
;Calculate errors on coefficients - not used here since uniform weighting of 
;data, but could be used someday.
;
;	siga = FLTARR(maxord+1)
;	FOR j=0,maxord DO siga(j) = SQRT(chi2*eps(j,j))

	RETURN
	END
