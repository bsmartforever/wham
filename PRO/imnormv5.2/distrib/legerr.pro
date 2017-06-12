;+
;Kenneth Sembach
;                               LEGERR.PRO
;
;Created: Unknown
;Last Revised: 
;
;Program Description:
;	This procedure calculates errors for use with LEGFIT.
;
;Restrictions:
;	Errors treated with uniform weighting ala LEGFIT.
;
;Screen Output:
;       None
;
;Use:
;       LEGERR,x,y,a,eps,chi2,error
;
;On Input:
;		x       :== abcissa array
;		y       :== ordinate array
;		a	:== Legendre polynomial coefficient array (from LEGFIT)
;		eps	:== Error matrix (from LEGFIT)
;		chi2	:== Variance returned by LEGFIT (uniform weighting)
;
;On Ouptut:
;               error  :== error array for y
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;
;External Routines Called:
;       None
;------------------------------------------------------------------------------
PRO LEGERR,x,y,a,eps,chi2,error

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'legerr' & RETURN & ENDIF

	ncoeff = N_ELEMENTS(a)
	nx = N_ELEMENTS(x)
	ix = FINDGEN(nx)
	nord = N_ELEMENTS(a) - 1

	p = FLTARR(nx,ncoeff)
	p(ix) = 1.
	p(ix+nx) = x
	FOR j=2,nord DO BEGIN
		p(ix+j*nx) = ((2.*j-1.)*x*p(*,j-1)-(j-1)*p(*,j-2)) / j
	ENDFOR

	eps1 = chi2 * eps
	tot = FLTARR(nx)
	FOR i=0,nx-1 DO BEGIN
		tot(i) = 0
		FOR l=0,nord DO BEGIN
			FOR k=0,nord DO BEGIN
				tot(i) = tot(i) + eps1(l,k) * p(i,l)*p(i,k)
			ENDFOR		
		ENDFOR	
	ENDFOR
	error = SQRT(tot)

	RETURN  &  END


