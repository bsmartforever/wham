;+
;Kenneth Sembach
;				IMAXIS.PRO
;				Version 5.0
;Created: 09/01/89
;Last Revision:	10/06/92
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
;	result = IMAXIS(x,wavc,in_flag)
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
;Latest Update Comments:
;	10/06/92  KRS	- Version 5.0, runs under Version 2 IDL.  
;			  Variable sys_flag removed.  Procedure converted
;			  to function.
;
;External Routines called:
;	None
;----------------------------------------------------------------------------
FUNCTION IMAXIS,x,wavc,in_flag
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