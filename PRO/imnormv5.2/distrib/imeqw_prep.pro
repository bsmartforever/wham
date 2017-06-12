;+
;Kenneth Sembach
;				IMEQW_PREP.PRO
;				Version 6.0
;
;Created: Unknown
;Last Revised: 05/02/99
;
;Program Description:
;	This procedure is a driver for the routine IMEQW.PRO.  It determines
;	the integration range and type of noise to propagate through a line 
;	during an equivalent width measurement and then displays results of 
;	the calculation.
;
;Restrictions:
;	None  
;
;Screen Output:
;	Text  &  Graphics
;
;Use:
;	IMEQW_PREP,x,y,ycon,sigma,ycon_sig,wavc
;	
;On Input:
;		x	:== wavelength array
;		y	:== intensity array
;		ycon	:== continuum array
;		sigma	:== RMS sigma of continuum fit
;		ycon_sig:== continuum fit error array
;		wavc	:== laboratory wavelength of line
;
;On Ouptut:
;		None
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/014/92  KRS	- Version 5.0, runs under Version 2 IDL.
;			  Simplified commands.
;	02/25/95   KRS	- End points of integration added to avoid truncation
;			  (ie., extend from xpos1 to xpos2)
;	05/02/99   KRS  - Version 5.2, documentation updated for distribution
;	               	- Limit checking added
;
;External Routines Called:
;	IMEQW		- to calculate equivalent width and errors
;	XLIMIT		- to determine elements of integration
;------------------------------------------------------------------------------
PRO IMEQW_PREP,x,y,ycon,sigma,ycon_sig,wavc
;	common SHARE, ncol,ncole1,ncole2,ncolez, w,w_es,w_ec,w_et,w_ez,va,vaerr,ba,baerr,m3,m3err,v1,v2,col3sig


        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imeqw_prep' & RETURN & ENDIF
;
;Error control
;
	ON_IOERROR,ESCAPE
LOOP:
;
;Overplot the defined continuum.
;
	OPLOT,x,ycon,LINESTYLE=2, color = 190,thick=2
;
;Type of input.
;
	PRINT,'IMEQW_PREP(v6.0)::  (c)ursor   (k)eyboard'
	choice = GET_KBRD(1)
;
;Get limits of integration regardless of which mode is used.
;
	IF choice EQ 'c' THEN BEGIN
           PRINT,'IMEQW_PREP(v6.0)::  Mark (C1)   Clear (C2)   Quit (C3)'
           CURSOR,xpos1,ypos1,/DOWN  &  IF !err EQ 4 THEN RETURN  
           IF !err EQ 2 THEN BEGIN
              PLOT,x,y  &  GOTO,LOOP
           ENDIF
           xpos1 = xpos1 > MIN(x)
           PRINT,'IMEQW_PREP(v6.0)::  Left limit:  ',$
            xpos1,IMAXIS(xpos1,wavc,-1)
           CURSOR,xpos2,ypos2,/DOWN  &  IF !err EQ 4 THEN RETURN
           IF !err EQ 2 THEN BEGIN
              PLOT,x,y  &  GOTO,LOOP
           ENDIF
           xpos2 = xpos2 < MAX(x)
           PRINT,'IMEQW_PREP(v6.0)::  Right limit: ',$
            xpos2,IMAXIS(xpos2,wavc,-1)
          
        ENDIF ELSE IF choice EQ 'k' THEN BEGIN
           
           PRINT,'IMEQW_PREP(v6.0)::  Space:  (v)elocity   (w)avelength'
           choice = GET_KBRD(1)
 
           IF choice EQ 'w' THEN BEGIN 
              READ,'IMEQW_PREP(v6.0)::  Enter left limit (w):  ',xpos1
              READ,'IMEQW_PREP(v6.0)::  Enter right limit (w): ',xpos2
           ENDIF ELSE BEGIN
              READ,'IMEQW_PREP(v6.0)::  Enter left limit (v):  ',xpos1
              READ,'IMEQW_PREP(v6.0)::  Enter right limit (v): ',xpos2
              xpos1 = IMAXIS(xpos1,wavc,1)  &  xpos2 = IMAXIS(xpos2,wavc,1)
           ENDELSE
           xpos1 = xpos1 > MIN(x) < MAX(x)
           xpos2 = xpos2 < MAX(x) > MIN(x)
        ENDIF ELSE GOTO,LOOP
        
        IF xpos1 GT xpos2 THEN BEGIN
           PRINT,'IMEQW_PREP(v6.0)::  '$
            +'Limits will be reversed for integration'
           xtmp = xpos1 & xpos1 = xpos2 & xpos2 = xtmp
        ENDIF
;
;Compute which range of elements should be included in integration and fill
;in line.  XLIMIT will find points in between limits, but be sure to set
;endpoints of integration equal to the limits.
;
print, xpos2,xpos1
        XLIMIT,x,xpos1,xpos2,x1,x2
        
        IF ABS(x1-x2) LE 1 THEN BEGIN
           PRINT,'IMEQW_PREP(v6.0)::  '$
            +'Insufficient spectral range specified.'
           PRINT,'IMEQW_PREP(v6.0)::  Please re-enter limits.'
           GOTO,LOOP
        ENDIF
        
        xwork = [xpos1,x(x1:x2),xpos2]
        ywork = [INTERPOL(y,x,xpos1),y(x1:x2),INTERPOL(y,x,xpos2)]
        yconwork = [INTERPOL(ycon,x,xpos1),ycon(x1:x2),INTERPOL(ycon,x,xpos2)]
        ycon_sig_work = [INTERPOL(ycon_sig,x,xpos1),ycon_sig(x1:x2)]
        ycon_sig_work = [ycon_sig_work,INTERPOL(ycon_sig,x,xpos2)]
;        POLYFILL,[xwork,REVERSE(xwork)],[ywork,REVERSE(yconwork)]
       plotcolorfill, xwork, yconwork, /noerase, col=234, bottom=ywork

;
;Ask about what kind of statistics should be used (poisson or fixed pattern)
;to describe the noise characteristics of the data.
;
        PRINT,'IMEWQ_PREP(v6.0)::  Noise:  (p)oisson   (f)ixed pattern' 
        kind = GET_KBRD(1) 
        IF kind EQ 'f' THEN BEGIN 
           PRINT,'IMEQW_PREP(v6.0)::  Fixed pattern noise assumed'
           y_sig_work = sigma + FLTARR(N_ELEMENTS(ywork)) 
        ENDIF ELSE BEGIN
           PRINT,'IMEQW_PREP(v6.0)::  Poisson noise assumed'
           y_sig_work = sigma * SQRT((ywork>0.0001)/yconwork)
        ENDELSE
;
;Calculate equivalent width and associated errors (cont ,stat, tot).
;
        IMEQW,xwork,ywork,yconwork,y_sig_work,ycon_sig_work,ew,y_err,ycon_err,zero_err
        toterr = SQRT(y_err^2 + ycon_err^2)
;
;Calculate linear column density and error.
;
        fval = FIND_FVAL(wavc)
        col = (ew/wavc)/8.85e-13/(wavc*1.e-8)/fval
        colerr = col - ((ew-toterr)/wavc)/8.85e-13/(wavc*1.e-8)/fval
        col3sig = alog10(3* (toterr/wavc)/8.85e-13/(wavc*1.e-8)/fval)

;
;Print information dump.  Equivalent widths in mA.
;	
; 	w    = ew * 1000.0
;	w_es =  y_err * 1000.0
;	w_ec =  ycon_err * 1000.0
;	w_et =  toterr * 1000.0
;	w_ez = zero_err* 1000.0

        PRINT,'--------------------------------------------'
        PRINT,'EW           = ', STRING(ew * 1000.0,'(F9.2)')
        PRINT,'Stat Error   = ',STRING(y_err * 1000.0 ,'(F9.2)')
        PRINT,'Cont Error   = ',STRING(ycon_err * 1000.0,'(F9.2)')
        PRINT,'Tot Error    = ',STRING(toterr * 1000.0 ,'(F9.2)')
 ;       PRINT,'2% Zero Err  = ',STRING(zero_err * 1000.0,'(F9.2)')
        PRINT,'Linear COG N = ',STRING(ALOG10(col),'(F9.4)')
        PRINT,'3sigma EW    < ',STRING(toterr * 3000.0,'(F9.2)')
        PRINT,'3sigma N     < ',STRING(col3sig,'(F9.2)')
        PRINT,'--------------------------------------------'
        RETURN
;------------------------------------------------------------------------------
ESCAPE:
        PRINT,'IMEQW_PREP(v6.0):: '+!err_string
     RETURN  &  END
