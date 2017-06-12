;+
;Kenneth Sembach
;				iAXIS.PRO
;				Version 5.0
;
;Program Description:
;	This function converts between wavelength and velocity space.
;	If an error is encountered, no message is printed but in_flag is
;	set to zero.
;
;Restrictions:
;	None	
;
;Screen Output: 
;	None
;
;Use:
;	result = iAXIS(x,wavc,in_flag)
;
;On Input:
;		x	:== x coordinate array
;		wavc	:== laboratory wavelength of line
;		in_flag	:== (-1=lambda->velocity, +1=velocity->lambda) 
;On Output:
;		result	:== converted x coordinate array
;		in_flag :== 0 if wavc = 0
;
;Common Blocks / Structures:
;	None
;
;External Routines called:
;	None
;----------------------------------------------------------------------------
FUNCTION iAXIS,x,wavc,in_flag
;
;Speed of light in km/sec.
;
	c = 2.997925e5	
;
;Return if no wavelength is defined .
;
	IF ((wavc EQ 0) OR (ABS(in_flag) NE 1)) THEN BEGIN
		in_flag=0  &  RETURN,x
	ENDIF
;
;Covert between lambda and velocity.
;
	IF in_flag EQ -1 THEN RETURN,c*(x-wavc)/wavc	;Lambda to velocity.
	IF in_flag EQ +1 THEN RETURN,wavc*(x/c)+wavc 	;Velocity to lambda.

	RETURN,x  &  END