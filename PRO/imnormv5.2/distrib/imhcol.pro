;+
;Kenneth Sembach
;				IMHCOL.PRO
;				Version5.2
;
;Program Description:
;       This procedure calculates column densities of H I 21 cm emission. 
;
;
;Screen Output:
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments/Creation:
;	05/02/08  NL	
;
;External Routines Called:
;       None
;------------------------------------------------------------------------------
PRO IMHCOL,x,y,wavc;,ycon,sigma,ycon_sig,wavc

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imcol' & RETURN & ENDIF

LOOP:
;	OPLOT,x,ycon,LINESTYLE=2

plots, [min(x),max(x)],[0,0], thick = 2 

	PRINT,'IMHCOL(v5.2)::  (c)ursor   (k)eyboard'
	choice = GET_KBRD(1)
;
;Get limits of integration regardless of which mode is used.
;
	IF choice EQ 'c' THEN BEGIN
	  PRINT,'IMHCOL_PREP(v5.2)::  Mark (C1)   Clear (C2)   Quit (C3)'
	  CURSOR,xpos1,ypos1,/DOWN  &  IF !err EQ 4 THEN RETURN  
	  IF !err EQ 2 THEN BEGIN
		PLOT,x,y  &  GOTO,LOOP
	  ENDIF
	  xpos1 = xpos1 > MIN(x)
	  PRINT,'IMHCOL(v5.2)::  Left limit:  ',xpos1,IMAXIS(xpos1,wavc,-1)
	  CURSOR,xpos2,ypos2,/DOWN  &  IF !err EQ 4 THEN RETURN
	  IF !err EQ 2 THEN BEGIN
		PLOT,x,y  &  GOTO,LOOP
	  ENDIF
	  xpos2 = xpos2 < MAX(x)
	  PRINT,'IMHCOL(v5.2)::  Right limit: ',xpos2,IMAXIS(xpos2,wavc,-1)
	ENDIF ELSE IF choice EQ 'k' THEN BEGIN
	  PRINT,'IMHCOL(v5.2)::  Space:  (v)elocity   (w)avelength'
	  choice = GET_KBRD(1)
 
	  IF choice EQ 'w' THEN BEGIN 
		READ,'IMHCOL(v5.2)::  Enter left limit (w):  ',xpos1
		READ,'IMHCOL(v5.2)::  Enter right limit (w): ',xpos2
	  ENDIF ELSE BEGIN
		READ,'IMHCOL(v5.2)::  Enter left limit (v):  ',xpos1
		READ,'IMHCOL(v5.2)::  Enter right limit (v): ',xpos2
		xpos1 = IMAXIS(xpos1,wavc,1)  &  xpos2 = IMAXIS(xpos2,wavc,1)
	  ENDELSE
	  xpos1 = xpos1 > MIN(x) < MAX(x)
	  xpos2 = xpos2 < MAX(x) > MIN(x)
	ENDIF ELSE GOTO,LOOP

	IF xpos1 GT xpos2 THEN BEGIN
		PRINT,'IMHCOL(v5.2)::  '$
			+'Limits will be reversed for integration'
		xtmp = xpos1 & xpos1 = xpos2 & xpos2 = xtmp
	ENDIF



index = where(x ge xpos1 and x le xpos2)
out = int_tabulated(x[index],y[index]) 
nh1 = alog10(1.823e18 * out) 

index = where(x ge xpos1-10 and x le xpos2+10)
out = int_tabulated(x[index],y[index]) 
enh1 = alog10(1.823e18 * out) 
print, 'err (+/-10)', nh1 - enh1


plots, [xpos1,xpos1],[-0.1,0.2], thick = 2 
plots, [xpos2,xpos2],[-0.1,0.2], thick = 2 

	PRINT,'IMHCOL(v5.2)::  Information Dump Follows'
	PRINT,'--------------------------------------------'
	PRINT,'N(HI)        = ', nh1
	PRINT,'err (+/-10)  = ',nh1 - enh1
	PRINT,'--------------------------------------------'

	PRINT,'IMHCOL(v5.2)::  End Information Dump'

;------------------------------------------------------------------------------
	RETURN  &  END
