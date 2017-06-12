;+
;Kenneth Sembach, Nicolas Lehner
;				IMTRIM.PRO
;				Version 6.0
;Created: 09/01/89
;Last Revision:	05/02/99
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
;		y	:== y coordinate array
;		ycon 	:== continuum coordinate array
;		lbar	:== lower continuum error bar array
;		ubar	:== upper continuum error bar array
;		coflag	:== continuum fitting flag (0=no,1=yes)
;		ebflag 	:== error bar flag (0=no,1=RMS shift,2=Legendre)
;		updates	:== update array
;On Output:
;		x	:== trimmed x coordinate array
;		y	:== trimmed y coordinate array
;		ycon 	:== trimmed continuum coordinate array
;		lbar	:== trimmed lower error bar array
;		ubar    :== trimmed upper error bar array
;		updates	:== updated updates
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Version 3.0
;	03/21/91  KRS   - Version 4.0, no significant revisions.
;	10/09/92  KRS	- Version 5.0, runs under Version 2 IDL.
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines called:
;	XLIMIT		- to determine trimming elements
;----------------------------------------------------------------------------
PRO IMTRIM,x,y,ycon,lbar,ubar,coflag,ebflag,updates

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imtrim' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and get left trim limit.
;
	PRINT,'IMTRIM(v6.0)::  (c)ursor trim   (k)eyboard trim   (q)uit'
LOOP:
	choice = GET_KBRD(1)
;
;Quit if user wants to do so.
;
	IF choice EQ 'q' THEN RETURN
;
;Do the trim in the x-coordinate.
;
LOOP1:
	IF choice EQ 'c' THEN BEGIN
		PRINT,'IMTRIM(v6.0)::  Mark (C1,C2,C3)'
		CURSOR,xpos1,ypos1,/DOWN
                PRINT,'IMTRIM(v6.0)::  Marked left limit:  ',xpos1
                !p.psym = 10 & OPLOT,[xpos1,xpos1],[ypos1,ypos1] & !p.psym = 10
                CURSOR,xpos2,ypos2,/DOWN
                PRINT,'IMTRIM(v6.0)::  Marked right limit: ',xpos2
	ENDIF ELSE IF choice EQ 'k' THEN BEGIN
		READ,'IMTRIM(v6.0)::  Enter left limit:  ',xpos1
		READ,'IMTRIM(v6.0)::  Enter right limit: ',xpos2
	ENDIF ELSE BEGIN
		PRINT,'IMTRIM(v6.0)::  (c)ursor trim  (k)eyboard trim  (q)uit'
		GOTO,LOOP
	ENDELSE


	xpos1 = xpos1 > MIN(x) < MAX(x)
	xpos2 = xpos2 < MAX(x) > MIN(x)

	IF xpos1 GT xpos2 THEN BEGIN
                PRINT,'IMTRIM(v6.0)::  '$
                        +'Limits will be reversed for trimming'
                xtmp = xpos1 & xpos1 = xpos2 & xpos2 = xtmp
        ENDIF
;
;Find the trim limits.
;
	XLIMIT,x,xpos1,xpos2,x1,x2

        IF ABS(x1-x2) LE 1 THEN BEGIN
                PRINT,'IMTRIM(v6.0)::  '$
                        +'Insufficient spectral range remaining.'
                PRINT,'IMTRIM(v6.0)::  Please re-enter limits.'
                GOTO,LOOP1
        ENDIF
;
;Ask if limits are acceptable.  If not, return.
;
	PLOT,x(x1:x2),y(x1:x2)
	READ,'IMTRIM(v6.0)::  Is the trimmed spectrum acceptable? ',choice
	IF STRMID(choice,0,1) NE 'y' THEN BEGIN
		PRINT,'IMTRIM(v6.0)::  Spectrum untrimmed'
		RETURN
	ENDIF
;
;Trim the spectrum.  
;
	x = x(x1:x2)  &  y = y(x1:x2)
;
;Trim the continuum and error bar arrays if they exist.  Return if only two 
;parameters are passed (ie., called outside IMNORM).
;
	IF N_PARAMS() EQ 2 THEN RETURN
	IF coflag EQ 1 THEN ycon = ycon(x1:x2)
	IF ebflag GE 1 THEN BEGIN
		lbar = lbar(x1:x2)
		ubar = ubar(x1:x2)
	ENDIF
;
;Update message to be put into file header and return.
;
	npts = N_ELEMENTS(x)
	comment = 'IMTRIM(v6.0)::  Spectrum trimmed to '+STRING(npts,'(I5)') $
			+' points  '+!stime
	IMUPDATE,updates,';'+comment
	PRINT,comment
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMTRIM(v6.0)::  '+!err_string
	RETURN  &  END
