;+
;                               IMCOL_PREP.PRO
;                                 Version 6.0
;
;Program Description:
;       This procedure calculates the apparent column density, equivalent width,
;       and line statistics. 
;
;Restrictions:
;       None
;
;Screen Output:
;       Text  &  Graphics
;
;Use:
;       IMCOL_PREP,x,y,ycon,sigma,ycon_sig,ebflag,wavc
;
;On Input:
;               x       :== velocity array
;               y       :== intensity array
;               ycon    :== continuum array
;               sigma   :== RMS sigma of continuum fit
;               ycon_sig:== continuum fit error array
;		ebflag 	:== error bar flag
;               wavc    :== laboratory wavelength of line
;
;On Ouptut:
;               storing inputs and outputs with possibility to save them at 
;               the end the session
;
;Common Blocks / Structures:
;       SHARE: inputs and outputs. 
;
;Latest Update Comments:
; 04/06/13  NL    - Version 6.0
;                         Calculate all the parameters at once. 
;                         Save variables for a save file. 
;	01/28/93  KRS  	- Version 5.0, runs under Version 2 IDL.
;                    	  Simplified commands.
;	02/25/95  KRS  	- End points of integration added to avoid truncation
;                      	  (ie., extend from xpos1 to xpos2)
;	05/02/99  KRS  	- Version 5.2, documentation updated for distribution
;                      	- Limit checking added
;
;External Routines Called:
;       IMCOL           - to calculate column density and errors
;       XLIMIT          - to determine elements of integration
;------------------------------------------------------------------------------
PRO IMCOL_PREP,x,y,ycon,sigma,ycon_sig,ebflag,wavc
	common SHARE, ncol,ncole1,ncole2,ncolez, w,w_es,w_ec,w_et,w_ez,va,vaerr,ba,baerr,m3,m3err,v1,v2,col3sig
  common NOISE, set_noise
  common FLAGSAT, flag_sat
  
loadct,39
        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imcol_prep' & RETURN & ENDIF
;
;Error control
;
	ON_IOERROR,ESCAPE
;
;Find the appropriate fvalue from a standard line list.
;
	fval = FIND_FVAL(wavc)
LOOP:
;
;Plot the column density versus velocity.
;
	ycol = FLTARR(N_ELEMENTS(y))
	FOR i=0,N_ELEMENTS(y)-1 DO BEGIN
		IF (y(i) GT 0) AND (ycon(i) GT 0) THEN BEGIN
			ycol(i) = ALOG(ycon(i)/y(i)) / (wavc*fval*2.654e-15)
		ENDIF
	ENDFOR
	PLOT,x,ycol
;
;Overplot zero line.
;
	OPLOT,!x.crange,[0,0],LINESTYLE=2,color = 190, thick=2
;
;Print wavelength, f-value, and error type to screen for a user check.
;
	PRINT,'IMCOL_PREP(v6.0)::  Wavelength = ',STRING(wavc,'(f8.3)')
	PRINT,'IMCOL_PREP(v6.0)::  f-value = ',STRING(fval,'(f7.5)')
	IF ebflag LE 1 THEN BEGIN
		PRINT,'IMCOL_PREP(v6.0)::  RMS shifting errors'
	ENDIF ELSE IF ebflag EQ 2 THEN BEGIN
		PRINT,'IMCOL_PREP(v6.0)::  Legendre fit errors'
	ENDIF ELSE BEGIN
		PRINT,'IMCOL_PREP(v6.0)::  Unknown error type'
	ENDELSE
;
;Type of input.
;
	PRINT,'IMCOL_PREP(v6.0)::  (c)ursor   (k)eyboard
	choice1 = GET_KBRD(1)
;
;Get limits of integration regardless of which mode is used.
;
	IF choice1 EQ 'c' THEN BEGIN
		PRINT,'IMCOL_PREP(v6.0)::  Mark (C1)   Clear (C2)   Quit(c3)

          CURSOR,xpos1,ypos1,/DOWN  &  IF !err EQ 4 THEN RETURN
          IF !err EQ 2 THEN BEGIN
                PLOT,x,y  &  GOTO,LOOP
          ENDIF
          xpos1 = xpos1 > MIN(x)  &  !c = 0
          PRINT,'IMCOL_PREP(v6.0)::  Left limit:  ',xpos1,IMAXIS(xpos1,wavc,+1)
          CURSOR,xpos2,ypos2,/DOWN  &  IF !err EQ 4 THEN RETURN
          IF !err EQ 2 THEN BEGIN
                PLOT,x,y  &  GOTO,LOOP
          ENDIF
          xpos2 = xpos2 < MAX(x)  &  !c = 0
          PRINT,'IMCOL_PREP(v6.0)::  Right limit: ',xpos2,IMAXIS(xpos2,wavc,+1)
	ENDIF ELSE BEGIN
                READ,'IMCOL_PREP(v6.0)::  Enter left limit (v):  ',xpos1
                READ,'IMCOL_PREP(v6.0)::  Enter right limit (v): ',xpos2
	ENDELSE

        IF xpos1 GT xpos2 THEN BEGIN
                PRINT,'IMCOL_PREP(v6.0)::  '$
                        +'Limits will be reversed for integration'
                xtmp = xpos1 & xpos1 = xpos2 & xpos2 = xtmp
        ENDIF
;
;Compute which range of elements should be included in integration and fill
;in line.  XLIMIT will find points in between limits, but be sure to set
;endpoints of integration equal to the limits.
;
    v1 = xpos1 
    v2 = xpos2  

        XLIMIT,x,xpos1,xpos2,x1,x2
        IF ABS(x1-x2) LE 1 THEN BEGIN
                PRINT,'IMCOL_PREP(v6.0)::  '$
                        +'Insufficient spectral range specified.'
                PRINT,'IMCOL_PREP(v6.0)::  Please re-enter limits.'
                GOTO,LOOP
        ENDIF
        xwork = [xpos1,x(x1:x2),xpos2]
        ywork = [INTERPOL(y,x,xpos1),y(x1:x2),INTERPOL(y,x,xpos2)]
        yconwork = [INTERPOL(ycon,x,xpos1),ycon(x1:x2),INTERPOL(ycon,x,xpos2)]
        ycolwork = [INTERPOL(ycol,x,xpos1),ycol(x1:x2),INTERPOL(ycol,x,xpos2)]
        ycon_sig_work = [INTERPOL(ycon_sig,x,xpos1),ycon_sig(x1:x2)]
        ycon_sig_work = [ycon_sig_work,INTERPOL(ycon_sig,x,xpos2)]
	
;
;Ask about what kind of statistics should be used (poisson or fixed pattern)
;to describe the noise characteristics of the data.
;
        PRINT,'IMCOL_PREP(v6.0)::  Noise:  (p)oisson   (f)ixed pattern'
        kind = GET_KBRD(1)
        IF kind EQ 'f' THEN BEGIN
                PRINT,'IMCOL_PREP(v6.0)::  Fixed pattern noise assumed'
                y_sig_work = sigma + FLTARR(N_ELEMENTS(ywork))
        ENDIF ELSE BEGIN
                PRINT,'IMCOL_PREP(v6.0)::  Poisson noise assumed'
                y_sig_work = sigma * SQRT(abs(ywork)/yconwork)
        ENDELSE
; save noise variable 
         set_noise = kind          

;
;Calculate the column density and error by calling IMCOL.
;
	IMCOL,xwork,ywork,yconwork,y_sig_work,ycon_sig_work,wavc,fval,col,$
		y_err,ycon_err,zero_err
	tot_err = SQRT(y_err^2 + ycon_err^2)
	IF col LE 0.0 THEN col=1.0
;
;FIll in the integrated area for the viewer to see.
;
;        POLYFILL,[xwork,REVERSE(xwork)],[ycolwork,REVERSE(yconwork)],color=240
          plotcolorfill, xwork, ycolwork, /noerase, col=234, bottom=0 
        
;
;Print information dump.
;
if flag_sat eq 0 then begin
        PRINT,'--------------------------------------------'
	PRINT,'log N (best) =   ',STRING(ALOG10(col),'(F9.3)')
  PRINT,'(+1 sig)     =   ',STRING(ALOG10(col+tot_err)-ALOG10(col),'(F9.3)')
  PRINT,'(-1 sig)     =   ',STRING(-ALOG10(col-tot_err)+ALOG10(col),'(F9.3)')
;	PRINT,'2% zero err  =   ',STRING(ALOG10(col+zero_err)-ALOG10(col),'(F9.3)')
        PRINT,'--------------------------------------------'
	ncol   = alog10(col)
	ncole1 = ALOG10(col+tot_err) - ALOG10(col)
	ncole2 = -ALOG10(col-tot_err) + ALOG10(col)
	ncolez = ALOG10(col+zero_err)-ALOG10(col)
endif 
if flag_sat eq 1 then begin
        PRINT,'--------------------------------------------'
    PRINT,'log N   > ',STRING(ALOG10(col),'(F9.3)')
        PRINT,'--------------------------------------------'
  ncol   = alog10(col)
  ncole1 = 1
  ncole2 = 1
  ncolez = 1
endif	
;------------------------------------------------------------------------------
;
xpos1 = v1
xpos2 = v2 
        XLIMIT,x,xpos1,xpos2,x1,x2
        xwork    = x(x1:x2)
        ywork    = y(x1:x2)
        yconwork = ycon(x1:x2)
        ycon_sig_work = ycon_sig(x1:x2)
 
;
;Ask about what kind of statistics should be used (poisson or fixed pattern)
;to describe the noise characteristics of the data.
;
        IF kind EQ 'f' THEN BEGIN
                y_sig_work = sigma + FLTARR(N_ELEMENTS(ywork))
        ENDIF ELSE BEGIN
                y_sig_work = sigma * SQRT(abs(ywork)/yconwork)
        ENDELSE


;Calculate line statistics.
;
  IMSTAT2,xwork,ywork,yconwork,y_sig_work,ycon_sig_work,m1,m1err,$
    m2,m2err,m3,m3err,m4,m4err
;
;Print information dump.
;

  va = m1
  vaerr = m1err
  ba = m2 * sqrt(2)
  baerr = m2err * sqrt(2)

        PRINT,'----------------------------------------------'
  PRINT,"$('<v>       = ',f8.3,'  +/- ',f7.3)",m1,m1err
  PRINT,"$('<b>       = ',f8.3,'  +/- ',f7.3)",m2* sqrt(2),m2err* sqrt(2)
  PRINT,"$('Skew      = ',f8.3,'  +/- ',f7.3)",m3,m3err
  PRINT,'----------------------------------------------'
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;
;Calculate equivalent width and associated errors (cont ,stat, tot).
; change first velocity to wavelength 
;
xpos1 = v1
xpos2 = v2 
               xpos1 = IMAXIS(xpos1,wavc,1)  &  xpos2 = IMAXIS(xpos2,wavc,1)
               x  = IMAXIS(x,wavc,1) 
        XLIMIT,x,xpos1,xpos2,x1,x2
                
        xwork = [xpos1,x(x1:x2),xpos2]
        ywork = [INTERPOL(y,x,xpos1),y(x1:x2),INTERPOL(y,x,xpos2)]
        yconwork = [INTERPOL(ycon,x,xpos1),ycon(x1:x2),INTERPOL(ycon,x,xpos2)]
        ycon_sig_work = [INTERPOL(ycon_sig,x,xpos1),ycon_sig(x1:x2)]
        ycon_sig_work = [ycon_sig_work,INTERPOL(ycon_sig,x,xpos2)]
 
        IF kind EQ 'f' THEN BEGIN 
           y_sig_work = sigma + FLTARR(N_ELEMENTS(ywork)) 
        ENDIF ELSE BEGIN
           y_sig_work = sigma * SQRT(abs(ywork)/yconwork)
        ENDELSE     
        
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
  w    = ew * 1000.0
  w_es =  y_err * 1000.0
  w_ec =  ycon_err * 1000.0
  w_et =  toterr * 1000.0
  w_ez = zero_err* 1000.0
        PRINT,'--------------------------------------------'
        PRINT,'EW           = ', STRING(ew * 1000.0,'(F9.2)')
        PRINT,'Stat Error   = ',STRING(y_err * 1000.0 ,'(F9.2)')
        PRINT,'Cont Error   = ',STRING(ycon_err * 1000.0,'(F9.2)')
        PRINT,'Tot Error    = ',STRING(toterr * 1000.0 ,'(F9.2)')
   ;     PRINT,'2% Zero Err  = ',STRING(zero_err * 1000.0,'(F9.2)')
        PRINT,'Linear COG N = ',STRING(ALOG10(col),'(F9.4)')
        PRINT,'3sigma EW    < ',STRING(toterr * 3000.0,'(F9.2)')
        PRINT,'3sigma N     < ',STRING(col3sig,'(F9.2)')
        PRINT,'--------------------------------------------'
 ;return to velocity                    
  x  = IMAXIS(x,wavc,-1) 
  pause
        RETURN
ESCAPE:
        PRINT,'IMCOL_PREP(v6.0):: '+!err_string
        RETURN  &  END

