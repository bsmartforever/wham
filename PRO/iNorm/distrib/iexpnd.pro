;+
;				IEXPND.PRO
;				Version 1.0
;
;Program Description:
;	This procedure allows interactive expansion of a plot in the current
;	X window. Limited use for analyzing the spectrum. 
;
;Screen Output: 
;	Text  &  Graphics
;
;Use:
;	IMEXPND,x,y
;
;On Input:
;		x	:== x coordinate array
;		y 	:== y coordinate array
;On Output:
;	None
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	04/13/13  - NL: version 1.0
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
PRO IEXPND,x,y

       IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imexpnd' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and commands.
;
LOOP:
	PRINT,'IExpnd::  (c)ursor  (k)eyboard   (u)nexpand   (q)uit'
	PRINT,'IExpnd::  [1-0] fast-x  [!-)] fast-y'
	choice = GET_KBRD(1)			
;
;Fast scaling of x-axis.
;
	IF choice EQ '1' THEN BEGIN
		xpos1 = -100  &  xpos2 = 100  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '2' THEN BEGIN
		xpos1 = -200  &  xpos2 = 200  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '3' THEN BEGIN
		xpos1 = -300  &  xpos2 = 300  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '4' THEN BEGIN
		xpos1 = -400  &  xpos2 = 400  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '5' THEN BEGIN
		xpos1 = -500  &  xpos2 = 500  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '6' THEN BEGIN
		xpos1 = -600  &  xpos2 = 600  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '7' THEN BEGIN
		xpos1 = -700  &  xpos2 = 700  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '8' THEN BEGIN
		xpos1 = -800  &  xpos2 = 800  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '9' THEN BEGIN
		xpos1 = -900  &  xpos2 = 900  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '0' THEN BEGIN
		xpos1 = -1000  &  xpos2 = 1000  &  ypos1 = 0  &  ypos2 = 0
		GOTO,FASTOUT
	ENDIF
;
;Fast scaling of y-axis.
;
	IF choice EQ '!' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -100  &  ypos2 = 100
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '@' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -200  &  ypos2 = 200
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '#' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -300  &  ypos2 = 300
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '$' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -400  &  ypos2 = 400
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '%' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -500  &  ypos2 = 500
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '^' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -600  &  ypos2 = 600
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '&' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -700  &  ypos2 = 700
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '*' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -800  &  ypos2 = 800
		GOTO,FASTOUT
	ENDIF
	IF choice EQ '(' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -900  &  ypos2 = 900
		GOTO,FASTOUT
	ENDIF
	IF choice EQ ')' THEN BEGIN
		xpos1 = 0   &  xpos2 = 0  &  ypos1 = -1000  &  ypos2 = 1000
		GOTO,FASTOUT
	ENDIF
;
;Quit if asked to do so.
;
	IF choice EQ 'q' THEN RETURN			
;
;Unexpand spectrum if asked to do so.
;
	IF choice EQ 'u' THEN BEGIN
		PRINT,'IExpnd::  Axes will be unexpanded'
		!x.style = 0  &  !y.style = 0
		!x.range = 0  &  !y.range = 0		
		RETURN
	ENDIF	
;
;Use cursor or keyboard to mark limits.
;
	IF choice EQ 'c' THEN BEGIN		
	    PRINT,'IExpnd::  Axes will be expanded by cursor'
	    PRINT,'IExpnd::  Mark lower left limit'
	    CURSOR,xpos1,ypos1,/DOWN
	    PRINT,'IExpnd::  Marked:    (',xpos1,ypos1,'    )'
	    !p.psym = 1 & OPLOT,[xpos1,xpos1],[ypos1,ypos1] & !p.psym = 0
	    PRINT,'IExpnd::  Mark upper right limit'
	    CURSOR,xpos2,ypos2,/DOWN
	    PRINT,'IExpnd::  Marked:    (',xpos2,ypos2,'    )'
	ENDIF ELSE IF choice EQ 'k' THEN BEGIN
	    PRINT,'IExpnd::  Axes will be expanded by keyboard'
	    READ,'IExpnd::  Enter xmin,ymin: ',xpos1,ypos1
	    !p.psym = 1 & OPLOT,[xpos1,xpos1],[ypos1,ypos1] & !p.psym = 0
	    READ,'IExpnd::  Enter xmax,ymax: ',xpos2,ypos2
	ENDIF ELSE GOTO,LOOP
;
;Check to be sure expansion is ok.
;
	loc = WHERE((x GE xpos1) AND(x LE xpos2))
  !psym=10

	IF ((loc(0) EQ -1) AND (xpos1 NE xpos2)) THEN BEGIN
	    	PRINT,'IExpnd::  Unable to x-expand plot'
		!x.range = [0,0]
		  !psym=10
		GOTO,LOOP
	ENDIF
	loc = WHERE((y GE ypos1) AND(y LE ypos2))
	IF ((loc(0) EQ -1) AND (ypos1 NE ypos2)) THEN BEGIN
	    	PRINT,'IExpnd::  Unable to y-expand plot'
		!y.range = [0,0]
		  !psym=10
		GOTO,LOOP
	ENDIF
FASTOUT:
;
;Do the expansion according to limits defined above and return to caller.
;
	minx = MIN(x)
	maxx = MAX(x)

	IF (((xpos1 LT minx) AND (xpos2 LT minx)) OR $
		((xpos1 GT maxx) AND (xpos2 GT maxx))) THEN BEGIN
		PRINT,'IExpnd:: X-axis expansion impossible'
		RETURN
	ENDIF ELSE BEGIN
		!x.style = 1  &  !y.style = 1
		!x.range = [xpos1,xpos2]
		!y.range = [ypos1,ypos2]
		  !psym=10
	ENDELSE
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IExpnd::  '+!err_string
	RETURN  &  END