;+
;Kenneth Sembach
;				XLIMIT.PRO
;				Version 6.0
;Created: 09/01/89
;Last Revision:	02/27/95
;
;Program Description:
;	This procedure finds the end pixels (x1 & x2) of array x between the
;	values xpos1 & xpos2.
;
;Screen Output: None
;
;Use:
;	XLIMIT,x,xpos1,xpos2,x1,x2
;
;On Input:
;		x	:== xarray
;		xpos1	:== left data limit
;		xpos2	:== right data limit
;
;On Output:
;		x1	:== left x pixel
;		x2	:== right x pixel
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/15/92  KRS	- Version 5.0, IMLIMIT renamed to XLIMIT
;	02/27/95  KRS	- Stupid conditional removed.  No more changing of
;			   xpos1 and xpos2.
;
;External Routines called:
;	None
;----------------------------------------------------------------------------
PRO XLIMIT,x,xpos1,xpos2,x1,x2

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'xlimit' & RETURN & ENDIF

	x1 = WHERE(x GE xpos1)  &  x1 = x1(0)
	x2 = WHERE(x GT xpos2)  &  x2 = x2(0) - 1

	IF x1 LE -1 THEN x1=0
	IF x2 LE -1 THEN x2=N_ELEMENTS(x)-1

	RETURN  &  END
