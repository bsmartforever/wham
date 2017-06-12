;+
;Kenneth Sembach
;                               iBLEM.PRO
;                               Version 5.2
;Created: 09/01/89
;Last Revision: 05/02/99
;
;Program Description:
;       This program linearly interpolates across spectral regions identified
;	by the user with the mouse.
;
;Restrictions:
;       3-button mouse assumed
;
;Screen Output:
;       Text & Graphics
;
;Use:
;       IMBLEM,x,y,yorig
;
;On Input:
;               x       :== x coordinate array
;               y       :== y coordinate array
;               ey       :== ey coordinate array
;On Output:
;               y       :== blemish corrected y coordinate array
;               yorig   :== original y coordinate array
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       04/11/13  NL   - Version 1.0
;
;External Routines called:
;	XLIMIT
;----------------------------------------------------------------------------
PRO iBLEM,x,y,ey,yorig

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imblem' & RETURN & ENDIF
;
;Error control.
;
        ON_IOERROR,ESCAPE
	yorig = y
LOOP:
;
;Get input from user.  If right mouse button is pushed, the return.
;If middle button is pushed, then clear. 
;		
        PRINT,'IMBLEM(v5.2)::  Mark (C1)  Reset (C2)  Quit (C3)'
	CURSOR,xpos1,ypos1,/DOWN
        xpos1 = xpos1 > MIN(x)  &  !c = 0
	IF !err EQ 4 THEN BEGIN
		IF N_PARAMS() EQ 4 THEN BEGIN
		loc = WHERE(y NE yorig)
		   IF loc(0) NE -1 THEN BEGIN
			npts = N_ELEMENTS(loc)
			comment = 'IMBLEM(v5.2)::  '+STRTRIM(npts,2) $
				   + ' blemished points removed  '+ !stime
		   ENDIF
		ENDIF
		RETURN
	ENDIF

        IF !err EQ 2 THEN BEGIN
		y = yorig
		PLOT,x,y
		GOTO,LOOP
	ENDIF
	PRINT,'IMBLEM(v5.2)::  Left limit:  ',xpos1
        OPLOT,[1,1]*xpos1,[1,1]*ypos1,psym=1
;
;Get and mark right of region.
;
        CURSOR,xpos2,ypos2,/DOWN
        xpos2 = xpos2 < MAX(x)  &  !c = 0
        PRINT,'IMBLEM(v5.2)::  Right limit: ',xpos2
        OPLOT,[1,1]*xpos2,[1,1]*ypos2,psym=1
;
;Check to be sure range is not reversed.  If reversed, get again.
;
        IF xpos2 LE xpos1 THEN $
		PRINT,'IMBLEM(v5.2)::  Intended action unclear - try again.'
;
;Determine which elements to interpolate and do interpolation.
;
        XLIMIT,x,xpos1,xpos2,x1,x2
	xint = x(x1:x2)
  yint = INTERPOL([y(x1),y(x2)],[x(x1),x(x2)],xint)
  eyint = INTERPOL([ey(x1),ey(x2)],[x(x1),x(x2)],xint)
	y(x1:x2) = yint
	ey(x1:x2) = eyint
	OPLOT,xint,yint
	GOTO,LOOP		
;----------------------------------------------------------------------------
ESCAPE:
        PRINT,'IMBLEM(v5.2)::  '+!err_string
        RETURN  &  END
