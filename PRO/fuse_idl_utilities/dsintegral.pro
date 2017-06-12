function dsintegral, vector, percent
;+
; NAME:
;	DSINTEGRAL
;
; PURPOSE:
;	Find the element of an array for which the integral is equal to
;	an input value.
;
; CATEGORY:
;	Arrays.
;
; CALLING SEQUENCE:
;	Result = DSINTEGRAL(Vector,Percent)
;
; INPUTS:
;	Vector:	The vector to integrate.
;	Percent:The percent included.
;
; OUTPUTS:
;	This function integrates the input vector and returns the
;	element at which the integral equals Percent percent of the
;	total. 
;
; PROCEDURE:
;	Straightforward.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 16 June 1997.
;	20 November 1998 Renamed from INTEGRAL to DSINTEGRAL.
;-

	intval = float(intvector(vector))	;integral of input vector
	intval = intval / intval(n_elements(intval)-1)
						;normalize

	temp = where(intval ge (float(percent)/100.))
	result = temp(0)						
					;find where integral >= percent

	return, result

end
