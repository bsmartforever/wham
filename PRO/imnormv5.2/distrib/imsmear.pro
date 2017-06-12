;+
;Kenneth Sembach
;				IMSMEAR.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revision:	05/02/99
;
;Program Description:
;	This function convolves a spectrum with a gaussian having a FWHM
;	specified by the user.  Updated for use on Sparc.
;
;Screen Output: None
;
;Use:
;	Result = IMSMEAR(x,y,fwhm,[updates])
;
;On Input:
;		x	:== x coordinate array
;		y	:== y coordinate array
;		fwhm	:== fwhm of Gaussian
;		updates	:== update array
;On Output:
;		Result	:== Smoothed version of y
;
;Common Blocks:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Version 3.0
;	03/21/91  KRS   - Version 4.0, update comment parameter added.
;	05/11/93  KRS	- Version 5.0, Changed to function.  Use on Sparc.
;	05/02/99  KRS	- Version 5.2, documentation updated for distribution
;
;External Routines called:
;	None
;----------------------------------------------------------------------------
FUNCTION IMSMEAR,x,y,fwhm

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imsmear' & RETURN,0 & ENDIF
;
;Return if FWHM = 0
;
	IF FWHM EQ 0 THEN BEGIN
	PRINT,'IMSMEAR(v5.2):: FWHM = 0, returning original y vector'
	RETURN,y
	ENDIF
;
;Form constant for exponential.
;
	alpha = 4.0 * ALOG(2.0)
;
;Calculate an average spacing for the data to determine how many Gaussian
;elements need to be calculated.
;
	x1    = x(1:N_ELEMENTS(x)-1)
	x2    = x(0:N_ELEMENTS(x)-2)
	x3    = ABS(x1-x2)
	width = TOTAL(x3)/N_ELEMENTS(x3)
;
;Compute the number of points needed in Gaussian on each side.
;
	npts = FIX(2.0*fwhm/width)
;
;If npts = 0, then get out of routine (internal consistency check).
;
	IF npts EQ 0 THEN BEGIN
;		PRINT,'IMSMEAR(v5.2)::  'Unable to form Gaussian - Returning'
;		RETURN,y
	ENDIF
;
;Form generic array centered about 0 with a length of npts on each side of 0.
;
	array = [-REVERSE(FINDGEN(npts))-1,0,FINDGEN(npts)+1] * width
;
;Form gaussian kernal.
;
	kernal = EXP(-alpha*(array/fwhm)^2)
;
;If the length of the kernal is not less than the length of the data arrays,
;then escape (internal consistency check).
;
	IF N_ELEMENTS(kernal) GE N_ELEMENTS(x) THEN BEGIN
;		PRINT,'IMSMEAR(v5.2)::  'Unable to form Gaussian - Returning'
;		RETURN,y
	ENDIF		
;
;Convolve the spectrum with the kernal, using unitary weighting.
;
	RETURN,CONVOL(y,kernal,TOTAL(kernal))
	END




