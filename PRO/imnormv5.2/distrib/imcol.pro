;+
;Kenneth Sembach
;				IMCOL.PRO
;				Version5.2
;
;Created: Unknown
;Last Revised: 05/02/99
;
;Program Description:
;       This procedure calculates column densities and errors for
;       spectral lines given a fitted continuum with errors.  Column 
;	densities and errors are calculated using the integrated apparent
;	column density technique.
;
;Restrictions:
;       Error points in the ordinate array are assumed to be uncorrelated.
;       Error points in the continuum array are assumed to be correlated.
;
;Screen Output:
;       None
;
;Use:
; 	IMCOL,x,y,ycon,y_sig,ycon_sig,wavc,fval,col,y_err,ycon_err,zero_err
;
;On Input:
;               x       :== abcissa array
;               y       :== ordinate array
;               ycon    :== fitted continuum array
;               y_sig   :== error array for ordinate
;               ycon_sig:== error array for fitted continuum
;		wavc	:== wavelength of line
;		fval	:== fvalue of line
;
;On Ouptut:
;               col     :== integrated column density
;               y_err   :== column density error due to y_sig
;               ycon_err:== column density error due to ycon_sig
;               zero_err:== column density error due to 2% background err
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       01/28/93  KRS   - Version 5.0, runs under Version 2 IDL.
;       02/27/95  KRS   - Version 5.0, minor change to update endpoint widths.
;	03/26/96  KRS	- Version 5.X, background error calculation updated to
;			  empirical determination.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;       None
;------------------------------------------------------------------------------
PRO IMCOL,x,y,ycon,y_sig,ycon_sig,wavc,fval,col,y_err,ycon_err,zero_err,root
    common FLAGSAT, flag_sat

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imcol' & RETURN & ENDIF
;
;Calculate dx.
;
	nx = N_ELEMENTS(x)
	dx = FLTARR(nx)
	FOR j=1,nx-2 DO dx(j) = (x(j+1)-x(j-1)) / 2.0 
        dx(0) = x(1)-x(0)               ;Updated over dx(0)=dx(1)
        dx(nx-1) = x(nx-1)-x(nx-2)      ;Updated over dx(nx-1) = dx(nx-2)
;
;Calculate integrated apparent column density.  Units are atoms/cm2/(km/s).
;
	tau = TOTAL(ALOG(ycon/y)*dx)
	col = tau / (wavc*fval*2.654e-15)
	 flag_sat = 0 
  test = where(y le 0,ct) 
  yy1 = y 
  if ct ne 0 then begin 
  ; artificially remove 0 and negative flux
  yy1[test] = 1e-15
  flag_sat = 1
  tau = TOTAL(ALOG(ycon/yy1)*dx)
  col = tau / (wavc*fval*2.654e-15)
  endif
;
;Calculate error on integrated apparent optical depth due to intensity errors.
;	
	y_err = SQRT(TOTAL(y_sig^2 * (-1./y)^2 * dx^2)) / (wavc*fval*2.654e-15)
;
;Calculate error on integrated apparent optical depth due to continuum errors.
;Add the errors by straight addition since errors are correlated => cannot add 
;in quadrature.
	ycon_err = TOTAL(ycon_sig * (1./ycon) * dx) / (wavc*fval*2.654e-15)
;
;Calculate error on integrated apparent optical depth due to 2% zero level 
;shift
;
	z_eps = 0.02
	zero_err = TOTAL(ALOG(1+z_eps*ALOG(ycon/y))*dx) / (wavc*fval*2.654e-15)
;
;Empirical estimate of error due to 2% zero level shift.
;
	z_eps = 0.02
	yc1 = ycon*(1-z_eps)
	y1  = y-ycon*z_eps
	tau1 = TOTAL(ALOG(yc1/y1)*dx)
	col1 = tau1 / (wavc*fval*2.654e-15)		
	zero_err = ABS(col1-col)

	RETURN  &  END
