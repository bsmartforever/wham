;+
;				ITRIM.PRO
;				Version 1.0
;				
;Program Description:
;	This program trims a spectrum and associated continuum and error bars.
;
;Screen Output:
;	Text & Graphics
;
;Use:
;	IMTRIM,x,y,ycon,lbar,ubar,coflag,ebflag,updates
;
;On Input:
;		x	:== x coordinate array
;   y :== y coordinate array
;   ey :== ey coordinate array
;   ycon  :== continuum coordinate array
;   ycon_sig  :== continuum coordinate array
;		coflag	:== continuum fitting flag (0=no,1=yes)
;On Output:
;		x	:== trimmed x coordinate array
;		y	:== trimmed y coordinate array
;   ey :== ey coordinate array
;		ycon 	:== trimmed continuum coordinate array
;   ycon_sig  :== continuum coordinate array
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	04/12/13  NL	- Version 1.0
;	
;External Routines called:
;	XLIMIT		- to determine trimming elements
;----------------------------------------------------------------------------
PRO iTRIM,x,y,ey,ycon,ycon_sig,coflag


        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imtrim' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and get left trim limit.
;
	PRINT,'iTrim::  (c)ursor trim   (k)eyboard trim   (q)uit'
LOOP:
	choice = GET_KBRD(1)
;
;Quit if user wants to do so.
;
	IF choice EQ 'q' THEN RETURN
;
;Do the trim in the x-coordinate.
;
loadct,39,/silent
LOOP1:
	IF choice EQ 'c' THEN BEGIN
		PRINT,'iTrim::  Mark (C1,C2,C3)'
		CURSOR,xpos1,ypos1,/DOWN
                PRINT,'iTrim::  Marked left limit:  ',xpos1
                !p.psym = 1 & OPLOT,[xpos1,xpos1],[ypos1,ypos1],color = 230,thick=3,symsize=2 & !p.psym = 10
                CURSOR,xpos2,ypos2,/DOWN
                PRINT,'iTrim::  Marked right limit: ',xpos2
	ENDIF ELSE IF choice EQ 'k' THEN BEGIN
		READ,'iTrim::  Enter left limit:  ',xpos1
		READ,'iTrim::  Enter right limit: ',xpos2
	ENDIF ELSE BEGIN
		PRINT,'iTrim::  (c)ursor trim  (k)eyboard trim  (q)uit'
		GOTO,LOOP
	ENDELSE


	xpos1 = xpos1 > MIN(x) < MAX(x)
	xpos2 = xpos2 < MAX(x) > MIN(x)

	IF xpos1 GT xpos2 THEN BEGIN
                PRINT,'iTrim::  '$
                        +'Limits will be reversed for trimming'
                xtmp = xpos1 & xpos1 = xpos2 & xpos2 = xtmp
        ENDIF
;
;Find the trim limits.
;
	XLIMIT,x,xpos1,xpos2,x1,x2

        IF ABS(x1-x2) LE 1 THEN BEGIN
                PRINT,'iTrim::  '$
                        +'Insufficient spectral range remaining.'
                PRINT,'iTrim::  Please re-enter limits.'
                GOTO,LOOP1
        ENDIF
;
;Ask if limits are acceptable.  If not, return.
;
	PLOT,x(x1:x2),y(x1:x2)
	READ,'iTrim::  Is the trimmed spectrum acceptable? ',choice
	IF STRMID(choice,0,1) NE 'y' THEN BEGIN
		PRINT,'iTrim::  Spectrum untrimmed'
		RETURN
	ENDIF
;
;Trim the spectrum.  
;
	x = x(x1:x2)
  y = y(x1:x2)
	ey = ey(x1:x2)
;
;Trim the continuum and error bar arrays if they exist.  Return if only two 
;parameters are passed (ie., called outside IMNORM).
;
	IF N_PARAMS() EQ 4 THEN RETURN
	IF coflag EQ 1 THEN begin
	 ycon = ycon(x1:x2)
	 ycon_sig = ycon_sig(x1:x2)
	 endif 
;
;Update message to be put into file header and return.
;
	npts = N_ELEMENTS(x)
	comment = 'iTrim::  Spectrum trimmed to '+STRING(npts,'(I5)') $
			+' points  '+!stime
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'iTrim::  '+!err_string
	RETURN  &  END
