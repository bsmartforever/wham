;+
;				iCURS.PRO
;
;Program Description:
;	This procedure reads the cursor position from the current X window.
;	Coordinates are read in data coordinates. 
; 
;Restrictions:
;	Only works with 3 button mouse.  Use right mouse button to exit.
;
;Screen Output: 
;	Graphics  &  Text
;
;Use:
;	iCURS
;
;On Input:
;	None
;
;On Output:
;	None

;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	04/11/13  NL	- Version 1.0
;	
;External Routines called:
;	None
;------------------------------------------------------------------------------
PRO iCURS
;
;Error control
;
	ON_IOERROR,ESCAPE
;
;Print heading.
;
	PRINT,'iCURS::  Mark (C1,C2)   Quit (C3)'
	PRINT,'iCURS::  Information Dump Follows'
	PRINT,'----------------------------------------'
   	PRINT,'          X             Y             '	

LOOP: 
;
;Get cursor position in data coordinates.  Use the /DOWN qualifier.
;
	IF N_PARAMS() EQ 0 THEN CURSOR,xpos,ypos,/DOWN
	IF N_PARAMS() NE 0 THEN CURSOR,xpos,ypos,/DOWN,/DEVICE
;
;If the right mouse button is pressed, then exit.  Otherwise print position
;and plot a crosshair on the plot.
;
	IF !err EQ 4 THEN BEGIN
		PRINT,'----------------------------------------'
		PRINT,'iCURS::  End of Information Dump'
		RETURN
	END
	IF ABS(xpos) LE 1.e4 THEN BEGIN
		format = '$(4x,f10.4,e)'
		PRINT,format,xpos,ypos
	ENDIF ELSE PRINT,xpos,ypos

	IF N_PARAMS() EQ 0 THEN BEGIN	
		!p.linestyle = 2
		nsum = !nsum & !nsum = 1
		OPLOT,!x.crange,[ypos,ypos]
		OPLOT,[xpos,xpos],!y.crange
		!p.linestyle = 0
		!nsum = nsum
	ENDIF

	GOTO,LOOP
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'iCURS:: '+!err_string
	RETURN  &  END
