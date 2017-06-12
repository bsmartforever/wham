;+
;                               iREBIN.PRO
;                               Version 1.0
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
;On Input:
;		root    :== root of file name to be read (.dat assumed)
;		x       :== x coordinate array
;   y       :== y coordinate array
;   y       :== ey coordinate array
;   ycon  :== y continuum array
;   ycon_sig  :== y continuum array
;		lbar	:== lower error bar array if defined
;		ubar	:== upper error bar array if defined
;		coflag  :== continuum definition flag (0=undefined,1=defined)

;On Output:
;		x       :== rebinned x coordinate array
;   y       :== rebinned y coordinate array
;   ey      :== rebinned ey coordinate array
;   ycon  :== rebinned y continuum array
;   ycon_sig  :== rebinned y error continuum array
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       04/12/13  NL   - Version 1.0
;
;External Routines Called:
;
;------------------------------------------------------------------------------
PRO iREBIN,x,y,ey,ycon,ycon_sig,coflag

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'iRebin' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Print heading and get rebinning factor.
;
LOOP:
           npx=n_elements(x)
	PRINT,'iRebin::  Current x,y vectors are '+STRTRIM(npx,2)$
		+' points long'
	fact = 0.0
	READ,'iRebin::  Enter linear rebinning factor (0=quit): ',bin
	
;
;
	IF bin EQ 0 THEN RETURN
	IF bin GT 1 THEN begin
	bin = FIX(bin)
         nrebin=npx / bin                
         nlast=npx / bin * bin
          x1=boxave(reform(x[0:nlast-1]),bin)
          y1=boxave(reform(y[0:nlast-1]),bin)
         ey1=sqrt(boxave(reform((ey[0:nlast-1])^2),bin) / bin )
	PRINT,'iRebin::  Rebinning vectors to '+STRTRIM(nrebin,2) $
		+' points'
endif 
;
;Plot result and ask if okay.  If not, return.
;
!psym=10
	PLOT,x1,y1
	 print,'iRebin::  Is the binned spectrum acceptable? '
	       test  = GET_KBRD(1) 
         IF test NE 'y' THEN BEGIN
		PRINT,'iRebin::  Spectrum remains unbinned at '$
			+STRTRIM(npx,2)+' points'
		RETURN
	ENDIF
;
; Return if only two parameters are passed.
;
	x = x1
	y = y1
	ey = ey1
	IF N_PARAMS() EQ 2 THEN RETURN
	IF coflag EQ 1 THEN begin 
  ycon = boxave(reform(ycon[0:nlast-1]),bin)
  ycon_sig = boxave(reform(ycon_sig[0:nlast-1]),bin)
	endif

;Update message and return.
;
	comment = 'iRebin::  Spectrum rebinned from '$
		+STRTRIM(npx,2) +' to '+STRTRIM(nrebin,2) $
		+' points  '+!stime
	PRINT,comment
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'iRebin::  '+!err_string
	RETURN  &  END
