;+
;Kenneth Sembach
;				IMGAUS.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revision:	05/02/99
;
;Program Description:
;	This program fits a Gaussian + 2nd order polynomial to a region of the
;	spectrum defined by the user.  An option to plot the fit is
;	available.
;
;Restrictions:
;	None
;
;Screen Output: 
;	Graphics text
;
;Use:
;	IMGAUS,x,y
;
;On Input:
;		x	:== x coordinate array
;		y 	:== y coordinate array
;On Output:
;	No output variables
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Test version.
;	03/20/91  KRS   - Version 4.0, first working version.
;	11/17/92  KRS	- version 5.0, runs under IDL version 2.0.
;
;External Routines Called:
;	GAUSSFIT	- to do gaussian fitting (user library)
;	POLY 		- to form polynomial (user library)
;	XLIMIT		- to get limits of fit
;----------------------------------------------------------------------------
PRO IMGAUS,x,y

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imgaus' & RETURN & ENDIF
;
;Error control
;
	ON_IOERROR,ESCAPE
;
;Print heading and get limits.
;
LOOP:
	PRINT,'IMGAUS(v5.2)::  ## GAUSSIAN FIT MODE##'
	PRINT,'IMGAUS(v5.2)::  Mark (C1)   Clear (C2)   Quit (C3)'
	CURSOR,xpos1,ypos1,/DOWN  
	OPLOT,[xpos1,xpos1],[ypos1,ypos1],PSYM=1,SYMSIZE=2.0
	IF !err EQ 4 THEN RETURN  
	IF !err EQ 2 THEN BEGIN
		PLOT,x,y  &  GOTO,LOOP
	ENDIF
	CURSOR,xpos2,ypos2,/DOWN
	OPLOT,[xpos2,xpos2],[ypos2,ypos2],PSYM=1,SYMSIZE=2.0
	IF !err EQ 4 THEN RETURN  
	IF !err EQ 2 THEN BEGIN
		PLOT,x,y  &  GOTO,LOOP
	ENDIF

	IF xpos2 LE xpos1 THEN BEGIN
		PRINT,'IMGAUS(v5.2)::  Please mark region again with x1 < x2.'
		PLOT,x,y & GOTO,LOOP	
	ENDIF
;
;Find the limits and store points to be used in Gaussian fit..
;
	XLIMIT,x,xpos1,xpos2,x1,x2
	xx = x(x1:x2)  &  yy = y(x1:x2)
;
;Find Gaussian fit and continuum and overplot them.
;
	gfit = GAUSSFIT(xx,yy,a)  &  OPLOT,xx,gfit
	contin = POLY(xx,a(3:5))  &  OPLOT,xx,contin,LINESTYLE=2
;
;Calculate the normalized depth.
;
	a1 = a(1)
	XLIMIT,xx,a1,a1,x1,x2
	depth = ABS(a(0)/contin((x1-1)>0))
;
;Calculate 5 and 10% depth velocities.
;
	v5 = SQRT((-ALOG(0.05)*2*a(2)^2))
	v10 = SQRT((-ALOG(0.10)*2*a(2)^2))
;
;Stats.
;
	fwhm = a(2)*1.4142*1.665
	vplus = a(1) + fwhm/2
	vminus = a(1) - fwhm/2	
;
;Print the results to the upper left corner of the screen.
;
	PRINT,'IMGAUS(v5.2)::  Information Dump Follows'
	PRINT,'----------------------------------------'
	PRINT,"$('Range = ',f10.3,'  to ',f10.3)",xpos1,xpos2
	PRINT,"$('Depth = ',f10.3)",depth
	PRINT,"$('v-    = ',f10.3)",vminus
	PRINT,"$('v+    = ',f10.3)",vplus
	PRINT,"$('<v>   = ',f10.3)",a(1)
	PRINT,"$('FWHM  = ',f10.3)",fwhm
	PRINT,"$('05% depths @ ',2f10.3)",[-v5,v5]+a(1)
	PRINT,"$('10% depths @ ',2f10.3)",[-v10,v10]+a(1)
	PRINT,'---------------------------------------'
	PRINT,'IMGAUS(v5.2)::  End Information Dump'
;
;Wait for key to be hit.
;
	PRINT,'IMGAUS(v5.2)::  Hit any key to continue.'
	dummy = GET_KBRD(1)
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMGAUS(v5.2)::  '+!err_string
	RETURN  &  END
