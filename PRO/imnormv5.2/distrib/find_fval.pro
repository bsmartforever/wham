;+
;Kenneth Sembach
;                               FIND_FVAL.PRO
;                               Version 5.2
;Created: Unknown
;Last Revision: 07/19/94
;
;Program Description:
;	This function finds the f-value of the line with wavelength wave
;	in the file !krs_linelist.
;
;Restrictions:
;       None
;
;Screen Output:
;       Error text
;
;Use:
;       result = FIND_FVAL(wave,[tol],[wave_out])
;
;On Input:
;		wave	:== wavelength to search for
;		tol	:== wavelength tolerance (optional)
;             wave_out  :== wavelength of adopted f-value (optional) 
;On Output:
;               result  :== f-value of line with wavelength wave
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       10/06/92  KRS   - Version 5.0, runs under Version 2 IDL.
;	07/19/94  KRS	- Version 5.1, updated to include comments at
;			  end of linelist.
;	05/02/99  KRS	- Version 5.2, updated comments for distribution
;       03/08/02  JCH   - Version 5.2.2, added reporting of adopted wave.
;
;External Routines called:
;       None
;----------------------------------------------------------------------------
FUNCTION FIND_FVAL,wave,tol,wave_out
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Find the fvalue of line within a delta wavelength defined by variable tol.
;
	IF N_PARAMS(0) LT 2 THEN tol = 0.0
       	OPENR,unit,!krs_linelist,/GET_LUN
	FOR ii=1,5000 DO BEGIN
      	ion = '       '  &  lam  = 0.0  &  fval = 0.0
        READF,unit,'$(1X,F8.3,5X,A7,3x,E9.3)',lam,ion,fval
        wave_out = lam
      	IF ABS(lam-wave) LE tol THEN BEGIN
		CLOSE,unit  &  FREE_LUN,unit
		RETURN,fval
	ENDIF
	IF lam EQ 0.0 THEN BEGIN
		PRINT,'FIND_FVAL(v5.2)::  Unable to locate line ',STRING(wave,'(f8.3)')
		CLOSE,unit  &  FREE_LUN,unit
		RETURN,fval
	ENDIF
	ENDFOR
;------------------------------------------------------------------------------
ESCAPE:
	PRINT,'FIND_FVAL(v5.2):: '+!err_string
	CLOSE,unit  &  FREE_LUN,unit
	RETURN,0.0  &  END



