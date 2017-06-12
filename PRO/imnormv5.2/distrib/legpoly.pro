;+
;Kenneth Sembach
;				LEGPOLY.PRO
;
;Created: 04/23/91
;Last Revised: 10/08/92
;
;Program Description:
;	This function calcuates the Legendre polynomial corresponding
;	to an abcissa vector and a coefficient vector.
;
;Restrictions:
;	None 
;
;Screen Output:
;	None
;
;Use:
;	result = LEGPOLY(x,a)
;	
;On Input:
;		x	:== abcissa array
;		a	:== coefficient array
;
;On Ouptut:
;		result	:== array containing Legendre polynomial construction
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/08/92  KRS	- Updated to run under Version 2 IDL.
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
FUNCTION LEGPOLY,x,coeff

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'legpoly' & RETURN,0 & ENDIF
;
;Array length variables.
;
	nx = N_ELEMENTS(x)
	ix = FINDGEN(nx)
	ncoeff = N_ELEMENTS(coeff)
	nord = ncoeff - 1
	nc = ncoeff > 2
;
;Form legendre polynomial pieces.
;
	p = FLTARR(nx,ncoeff)
	p(ix) = 1.
	p(ix+nx) = x
	FOR j=2,nord DO p(ix+j*nx) = ((2.*j-1.)*x*p(*,j-1)-(j-1)*p(*,j-2)) / j
;
;Add the pieces to form entire polynomial.
;
	y = FLTARR(nx)
	FOR k=0,nord DO y = y + coeff(k) * p(*,k)
;
;Return to caller.
;
	RETURN,y
	END
