;+
;Kenneth Sembach
;                               IMREBIN.PRO
;                               Version 5.2
;Created: Unknown
;Last Revision: 05/02/99
;
;Program Description:
;       This procedure rebins data and associated error bars for use in 
;	IMNORM.
;
;Restrictions:
;       Rebins only by whole integer amounts.
;
;Screen Output: 
;       Text
;
;Use:
;       IMOPLOT,root
;
;On Input:
;		root    :== root of file name to be read (.dat assumed)
;		x       :== x coordinate array
;		y       :== y coordinate array
;		ycon	:== y continuum array
;		lbar	:== lower error bar array if defined
;		ubar	:== upper error bar array if defined
;		coflag  :== continuum definition flag (0=undefined,1=defined)
;		ebflag	:== error bar flag (0=undefined, 1=defined)

;On Output:
;		x       :== rebinned x coordinate array
;		y       :== rebinned y coordinate array
;		ycon	:== rebinned y continuum array
;		lbar	:== rebinned lower error bar array if defined
;		ubar	:== rebinned upper error bar array if defined
;		updates :== string array containing update comments
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;       IMUPDATE	- to revise the update comments
;------------------------------------------------------------------------------
PRO IMREBIN,x,y,ycon,lbar,ubar,coflag,ebflag,updates

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imrebin' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and get rebinning factor.
;
LOOP:
	npx = N_ELEMENTS(x) & npy = N_ELEMENTS(y)
	IF npx NE npy THEN BEGIN
		PRINT,'IMREBIN(v5.2)::  x,y vectors are not same length
		PRINT,'IMREBIN(v5.2)::  Cannot rebinn: ',npx,npy
		RETURN
	ENDIF
	PRINT,'IMREBIN(v5.2)::  Current x,y vectors are '+STRTRIM(npx,2)$
		+' points long'
	fact = 0.0
	READ,'IMREBIN(v5.2)::  Enter linear rebinning factor (0=quit): ',fact
;
;Check to be sure binning factor is okay (fact < 1 increases spectrum size)
;
	IF fact EQ 0 THEN RETURN
	IF fact GT 1 THEN fact = FIX(fact)
	newpt = LONG(npx/fact)
	IF newpt LT 2 THEN BEGIN
		PRINT,'IMREBIN(v5.2)::  Invalid rebinning factor'
		GOTO,LOOP
	ENDIF
	IF (((npx MOD fact) NE 0) AND ((newpt MOD npx) NE 0)) THEN BEGIN
		PRINT,'IMREBIN(v5.2)::  Spectrum length must be evenly divisible/expandable by factor''
		GOTO,LOOP
	ENDIF

	PRINT,'IMREBIN(v5.2)::  Rebinning vectors to '+STRTRIM(newpt,2) $
		+' points'
;
;Plot result and ask if okay.  If not, return.
;
	PLOT,REBIN(x,newpt),REBIN(y,newpt)
	choice = 'N'
	READ,'IMREBIN(v5.2)::  Is the binned spectrum acceptable? ',choice
	IF STRMID(choice,0,1) NE 'y' THEN BEGIN
		PRINT,'IMREBIN(v5.2)::  Spectrum remains unbinned at '$
			+STRTRIM(npx,2)+' points'
		RETURN
	ENDIF
;
;Rebin.  Return if only two parameters are passed (ie., called outside IMNORM).
;
	x = REBIN(x,newpt)
	y = REBIN(y,newpt)
	IF N_PARAMS() EQ 2 THEN RETURN
	IF coflag EQ 1 THEN ycon = REBIN(ycon,newpt)
	IF ebflag GE 1 THEN BEGIN
		lbar = REBIN(lbar,newpt)
		ubar = REBIN(ubar,newpt)
	ENDIF
;
;Update message to be put into file header and return.
;
	comment = 'IMREBIN(v5.2)::  Spectrum rebinned from '$
		+STRTRIM(npx,2) +' to '+STRTRIM(newpt,2) $
		+' points  '+!stime
	IMUPDATE,updates,';'+comment
	PRINT,comment
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'IMREBIN(v5.2)::  '+!err_string
	RETURN  &  END
