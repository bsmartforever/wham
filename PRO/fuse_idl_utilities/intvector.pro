function intvector,input,skipzero=skipzero
;+
; NAME:
;	INTVECTOR
;
; PURPOSE:
;	This function integrates a one dimensional vector.
;
; CATEGORY:
;	Math.
;
; CALLING SEQUENCE:
;	Result = INTVECTOR(Input)
;
; INPUTS:
;	Input:	The one dimensional vector to be integrated.
;
; KEYWORD PARAMETERS:
;	SKIPZERO:
;		If set, start the integration with element 1 instead
;		of element 0 of the array.
;
; OUTPUTS:
;	This function returns the integrated vector.
;
; PROCEDURE:
;	Straightforward.
;
; EXAMPLE:
;		A = [0,1,3,4,2,4,2]
;		B = INTVECTOR(A)
;		PRINT,B
;
;		0   1   4   8  10  14  16
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 3 April 1997.
;	2 June 1998 Changed istart to a long.
;	29 August 2003 Leon Aksman - removed TOTAL from loop in order to
;	 speed things up.
;-

	n = n_elements(input)
	outvector = input
	outvector[0] = 0

	if(keyword_set(skipzero)) then begin
		print,'Skip zero'
		istart = 1L
	endif else begin
		istart = 0L
	endelse

	sum = 0
        FOR i=istart, n-1 do begin
        	 sum = sum + outvector[i]
        	 outvector[i] = sum
        ENDFOR

	return,outvector

end
