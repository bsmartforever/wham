;+
;Kenneth Sembach
;                               IMSTAT_PREP.PRO
;                                 Version 6.0
;Created: 09/01/89
;Last Revision: 05/02/99
;
;Program Description:
;       This procedure is a driver for the routine IMSTAT2.PRO.  It determines
;       the integration range and type of noise to propagate through a line 
;       during a line statistics measurement and then displays results of 
;       the calculation.
;
;Restrictions:
;       None  
;
;Screen Output:
;       Text  &  Graphics
;
;Use:
;       IMSTAT_PREP,x,y,ycon,sigma,ycon_sig
;
;On Input:
;               x       :== wavelength array
;               y       :== intensity array
;               ycon    :== continuum array
;               sigma   :== RMS sigma of continuum fit
;               ycon_sig:== continuum fit error array
;
;On Ouptut:
;               None
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       04/08/13  NL   - Version 6.0, colored version
;       10/14/92  KRS  - Version 5.0, runs under Version 2 IDL.
;                         Simplified commands.
;       02/25/95   KRS  - End points of integration added to avoid truncation
;                         (ie., extend from xpos1 to xpos2)
;       05/02/99   KRS  - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;       IMSTAT2         - to calculate line statistics
;       XLIMIT          - to determine elements of integration
;------------------------------------------------------------------------------
PRO IMSTAT_PREP,x,y,ycon,sigma,ycon_sig
	; common SHARE, ncol,ncole1,ncole2,ncolez, w,w_es,w_ec,w_et,w_ez,va,vaerr,ba,baerr,m3,m3err,v1,v2,col3sig

loadct,39
        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imstat_prep' & RETURN & ENDIF
;
;Error control
;
	ON_IOERROR,ESCAPE
LOOP:
;
;Overplot the defined continuum.
;
	OPLOT,x,ycon,linestyle=2, color = 190,thick=2
;
;Type of input.
;
        PRINT,'IMSTAT_PREP(v6.0)::  (c)ursor   (k)eyboard'
        choice = GET_KBRD(1)
;
;Get limits of integration regardless of which mode is used.
;
        IF choice EQ 'c' THEN BEGIN
          PRINT,'IMSTAT_PREP(v6.0)::  Mark (C1)   Clear (C2)   Quit (C3)'
          CURSOR,xpos1,ypos1,/DOWN  &  IF !err EQ 4 THEN RETURN  
          IF !err EQ 2 THEN BEGIN
                PLOT,x,y  &  GOTO,LOOP
          ENDIF
          xpos1 = xpos1 > MIN(x)  &  !c = 0
          PRINT,'IMSTAT_PREP(v6.0)::  Left limit:  ',xpos1
          CURSOR,xpos2,ypos2,/DOWN  &  IF !err EQ 4 THEN RETURN
          IF !err EQ 2 THEN BEGIN
                PLOT,x,y  &  GOTO,LOOP
          ENDIF
          xpos2 = xpos2 < MAX(x)  &  !c = 0
          PRINT,'IMSTAT_PREP(v6.0)::  Right limit: ',xpos2
        ENDIF ELSE IF choice EQ 'k' THEN BEGIN
          READ,'IMSTAT_PREP(v6.0)::  Enter left limit (v):  ',xpos1
          READ,'IMSTAT_PREP(v6.0)::  Enter right limit (v): ',xpos2
          xpos1 = xpos1 > MIN(x)  &  !c = 0
          xpos2 = xpos2 < MAX(x)  &  !c = 0
        ENDIF ELSE GOTO,LOOP
;
;Compute which range of elements should be included in integration.
;
        XLIMIT,x,xpos1,xpos2,x1,x2
        xwork    = x(x1:x2)
        ywork    = y(x1:x2)
        yconwork = ycon(x1:x2)
        ycon_sig_work = ycon_sig(x1:x2)
  ;      POLYFILL,[xwork,REVERSE(xwork)],[ywork,REVERSE(yconwork)]
        plotcolorfill, xwork, yconwork, /noerase, col=234, bottom=ywork
        
		v1 = xpos1 
		v2 = xpos2 

;
;Ask about what kind of statistics should be used (poisson or fixed pattern)
;to describe the noise characteristics of the data.
;
        PRINT,'IMSTAT_PREP(v6.0)::  Noise:  (p)oisson   (f)ixed pattern' 
        kind = GET_KBRD(1) 
        IF kind EQ 'f' THEN BEGIN 
                PRINT,'IMSTAT_PREP(v6.0)::  Fixed pattern noise assumed'
                y_sig_work = sigma + FLTARR(N_ELEMENTS(ywork)) 
        ENDIF ELSE BEGIN
                PRINT,'IMSTAT_PREP(v6.0)::  Poisson noise assumed'
                y_sig_work = sigma * SQRT(abs(ywork)/yconwork)
        ENDELSE
;
;Calculate line statistics.
;
	IMSTAT2,xwork,ywork,yconwork,y_sig_work,ycon_sig_work,m1,m1err,$
		m2,m2err,m3,m3err,m4,m4err
;
;Print information dump.
;

;	va = m1
;	vaerr = m1err
;	ba = m2 * sqrt(2)
;	baerr = m2err * sqrt(2)

        PRINT,'IMSTAT_PREP(v6.0)::  Information Dump Follows'
        PRINT,'----------------------------------------------'
	PRINT,"$('<v>       = ',f8.3,'  +/- ',f7.3)",m1,m1err
	PRINT,"$('<b>       = ',f8.3,'  +/- ',f7.3)",m2* sqrt(2),m2err* sqrt(2)
	PRINT,"$('Skew      = ',f8.3,'  +/- ',f7.3)",m3,m3err
	PRINT,"$('Delta(ext)= ',f8.3,'  +/- ',f7.3)",m4,m4err
       	PRINT,'----------------------------------------------'
        PRINT,'IMSTAT_PREP(v6.0)::  End Information Dump'
	RETURN
;------------------------------------------------------------------------------
ESCAPE:
        PRINT,'IMSTAT_PREP(v6.0):: '+!err_string
        RETURN  &  END
