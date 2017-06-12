;+
;				IMCONT.PRO
;				Version 6.0
;Created: 09/01/89
;Last : 05/02/99
;
;Program Description:
;	This procedure obtains and stores regions of the continuum for
;	later use in fitting a polynomial to the continuum.
;
;Restrictions:
;       Requires cursor (3 button mouse) input
;
;Screen Output:
;	Graphics  &  Text
;
;Use:
;	IMCONT,x,y,xarray,yarray,store,coflag
;
;On Input:
;		x	:== x coordinate array
;		y 	:== y coordinate array
;		xarray  :== defined x coordinate array
;		yarray  :== defined y coordinate array
;		store 	:== continuum region storage array (2xn)
;		coflag 	:== continuum definition flag (0=no,1=yes)
;
;On Output:
;		xarray  :== redefined x coordinate array
;		yarray  :== redefined y coordinate array
;		store 	:== new continuum region storage array (2xn)
;		coflag 	:== continuum definition flag (0=no,1=yes)
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
; 04/07/13  NL  - Version 6.0, updated in colors.   
; 10/07/92  KRS - Version 5.0, runs under Version 2 IDL.    
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;	XLIMIT		- To determine the elements  of marked regions
;----------------------------------------------------------------------------
PRO IMCONT,x,y,xarray,yarray,store,coflag

  	IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imcont' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
	IF N_PARAMS() NE 6 THEN coflag = 0
;
;Print instructions.
;
	PRINT,'IMCONT(v5.2)::  Mark (C1)   Clear (C2)   Quit (C3)'

LOOP:
;
;Determine how many regions have already been fit ans show them.
;
loadct,39
	liney1 = [1,-1]*!y.crange(1)/30. + 0.9*!y.crange(1)
	IF coflag EQ 0 THEN n_store = 0
	IF coflag NE 0 THEN BEGIN
	   n_store =  N_ELEMENTS(store)/2
	   FOR k=0,n_store-1 DO BEGIN
		OPLOT,[store(0,k),store(0,k)],liney1,color=200
		OPLOT,[store(1,k),store(1,k)],liney1,color=200
		OPLOT,[store(0,k),store(1,k)],[1.0,1.0]*0.9*!y.crange(1),color=200
	   ENDFOR
	ENDIF
;
;Get input from user.  If right mouse button is pushed, the return.
;If middle button is pushed, then clear. 
;
	CURSOR,xpos1,ypos1,/DOWN
	xpos1 = xpos1 > MIN(x)  &  !c = 0
	IF !err EQ 4 THEN RETURN
	IF !err EQ 2 THEN BEGIN
		nstore = 0  &  store = 0  &  coflag = 0
		xarray = 0.  &  yarray = 0.0
		PLOT,x,y  &  GOTO,LOOP
	ENDIF
;
;Mark left of region.
;
	PRINT,'IMCONT(v5.2)::  Left limit:  ',xpos1
	OPLOT,[xpos1,xpos1],liney1,color=200
;
;Get and mark right of region.
;
	CURSOR,xpos2,ypos2,/DOWN	
	xpos2 = xpos2 < MAX(x)  &  !c = 0
  	PRINT,'IMCONT(v5.2)::  Right limit: ',xpos2
	OPLOT,[xpos2,xpos2],liney1,color=200
;
;Check to be sure range is not reversed.  If reversed, get again.
;
	IF xpos2 GT xpos1 THEN BEGIN
		OPLOT,[xpos1,xpos2],[1,1]*0.9*!y.crange(1)	,color=200
	ENDIF ELSE GOTO,LOOP
;
;Detrmine which regions to store.
;
	XLIMIT,x,xpos1,xpos2,x1,x2
;
;Keep track of the defined regions and get next region.
;
	coflag = 1
	n_store = n_store + 1
	IF n_store EQ 1 THEN BEGIN
		store = FLTARR(2,1) + [xpos1,xpos2]
		xarray = x(x1:x2)
		yarray = y(x1:x2)
	ENDIF ELSE BEGIN
		store = [[store],[xpos1,xpos2]]
		xarray = [xarray,x(x1:x2)]
		yarray = [yarray,y(x1:x2)]
	ENDELSE

	GOTO,LOOP		
;----------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMCONT(v5.2)::  '+!err_string
	RETURN  &  END

