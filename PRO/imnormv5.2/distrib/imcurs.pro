;+
;Kenneth Sembach
;				IMCURS.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revised: 05/02/92
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
;	IMCURS
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
;	10/07/92  KRS	- Version 5.0, runs under Version 2 IDL.
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines called:
;	None
;------------------------------------------------------------------------------
PRO IMCURS
;
;Error control
;
	ON_IOERROR,ESCAPE
;
;Print heading.
;
	PRINT,'IMCURS(v5.2)::  Mark (C1,C2)   Quit (C3)'
	PRINT,'IMCURS(v5.2)::  Information Dump Follows'
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
		PRINT,'IMCURS(v5.2)::  End of Information Dump'
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
	PRINT,'IMCURS(v5.2):: '+!err_string
	RETURN  &  END
