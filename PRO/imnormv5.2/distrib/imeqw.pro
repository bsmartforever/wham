;+
;Kenneth Sembach
;				IMEQW.PRO
;				Version5.2
;
;Created: Unknown
;Last Revised: 05/02/99
;
;Program Description:
;	This procedure calculates equivalent widths and errors for
;	spectral lines given a fitted continuum with errors.  If no errors
;	are to be calculated, only the first five parameters need to be 
;	passed.  
;
;Restrictions:
;	Error points in the ordinate array are assumed to be uncorrelated.
;	Error points in the continuum array are assumed to be correlated.
;	
;Screen Output:
;	None
;
;Use:
;	IMEQW,x,y,ycon,y_sig,ycon_sig,ew,y_err,ycon_err,zero_err
;	
;On Input:
;		x	:== abcissa array (in Angstroms)
;		y	:== ordinate array 
;		ycon	:== fitted continuum array
;		y_sig	:== error array for ordinate
;		ycon_sig:== error array for fitted continuum
;
;On Ouptut:
;		ew	:== equivalent width
;		y_err	:== error in equivalent width due to y_sig
;		ycon_err:== error in equivalent width due to ycon_sig
;		zero_err:== error in equivalent width due to 2% background err
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/08/92  KRS	- Version 5.0, runs under Version 2 IDL.
;	02/27/95  KRS	- Version 5.0, minor change to update endpoint widths.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
PRO IMEQW,x,y,ycon,y_sig,ycon_sig,ew,y_err,ycon_err,zero_err,root

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imeqw' & RETURN & ENDIF
;
;Calculate dx.
;
	nx = N_ELEMENTS(x)
	dx = FLTARR(nx)
	FOR j=1,nx-2 DO dx(j) = (x(j+1)-x(j-1)) / 2.0 
	dx(0) = x(1)-x(0)		;Updated over dx(0)=dx(1)
	dx(nx-1) = x(nx-1)-x(nx-2)	;Updated over dx(nx-1) = dx(nx-2)
;
;Calculate equivalent width.
;
	ew = TOTAL((1.-y/ycon)*dx)
;
;Calculate error due to errors in continuum points.  Add the errors by
;straight addition since errors are correlated => cannot add in quadrature.
;
	ycon_err = TOTAL(SQRT(ycon_sig^2 * (y/ycon^2)^2 * dx^2))	
;
;Calculate (statistical) error due to errors in y (intensity) points.
;
	y_err = SQRT(TOTAL(y_sig^2 / ycon^2 * dx^2))
;
;Calculate the error due to a 2% zero level shift.
;
	z_eps = 0.02
	zero_err = z_eps * ew

	RETURN  &  END
	
