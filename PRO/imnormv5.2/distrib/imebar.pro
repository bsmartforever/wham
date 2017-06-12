;+
;Kenneth Sembach
;				IMEBAR.PRO
;				Version 5.2
;Created: 09/01/89
;Last Revision:	10/08/92
;
;Program Description:
;	This program allows manipulation of error bars given a known sigma,
;	the x and y arrays, and the fit array. 
;
;Restrictions:
;	Only error bars obeying RMS shift can be produced.
;
;Screen Output: 
;	Text  &  Graphics
;
;Use:
;	IMEBAR,x,y,ycon,sigma,b_sigma,lbar,ubar,ebflag
;
;On Input:
;	x	  :== x coordinate array
;	y	  :== y coordinate array
;	ycon	  :== y fitted coordinate array
;	sigma	  :== sigma of fit 
;	b_sigma   :== sigma for ebars
;	lbar	  :== lower error bar array if defined
;	ubar      :== upper error bar array if defined
;	ebflag	  :== error bar flag (0=undefined, 1=RMS)
;
;On output:
;	b_sigma   :== sigma for ebars
;	lbar	  :== revised y lower error bar array
;	ubar   	  :== revised y upper error bar array
;	ebflag	  :== error bar flag (0=undefined,1=defined)
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;       10/08/92  KRS   - Early updates
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines called:
;	None
;----------------------------------------------------------------------------
PRO IMEBAR,x,y,ycon,sigma,b_sigma,lbar,ubar,ebflag

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imebar' & RETURN & ENDIF
;
;Error control
;
	ON_IOERROR,ESCAPE
;
;Overplot continuum.
;
	!p.linestyle = 2  &  OPLOT,x,ycon  &  !p.linestyle = 0
;
;Commands.
;
LOOP:
	PRINT,'IMEBAR(v5.2)::  (c)lear   (d)efine   (q)uit'
;
;Overplot error bars if they are defined
;
	IF ebflag NE 0 THEN BEGIN
		!p.linestyle = 2  &  OPLOT,x,ubar 
		OPLOT,x,lbar  &  !p.linestyle = 0
	ENDIF
	choice = GET_KBRD(1)
;
;Zap the current error bars.
;
	IF choice EQ 'c' THEN BEGIN
		b_sigma = 0  &  ebflag = 0
		PLOT,x,y 
	ENDIF
;
;Define error bars.
;
	IF choice EQ 'd' THEN BEGIN
		READ,'IMEBAR(v5.2)::  Fraction of RMS sigma to be applied: ',$
			constant
		b_sigma = FLOAT(constant) * sigma
		lbar = ycon - b_sigma
		ubar = ycon + b_sigma
		ebflag = 1
	ENDIF
;
;If user wants to quit, then do so.  Otherwise, next choice.
;
	IF choice EQ 'q' THEN RETURN
	GOTO,LOOP
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMEBAR(v5.2):: ' + !err_string
	RETURN  &  END
