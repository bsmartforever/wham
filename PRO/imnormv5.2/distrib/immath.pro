;+
;Kenneth Sembach
;				IMMATH.PRO
;				Version 5.2
;Created: 09/01/89
;Last Modified: /05/02/99
;
;Description:
;	This routine performs various mathematical operations on a 
;	spectrum.  
;
;Screen Output: Graphics text
;
;Use:
;	IMMATH,x,y
;
;On Input:
;		x	:== x coordinate array
;		y	:== y coordinate array
;
;On Output:
;		y	:== y coordinate array after mathematical operation
;
;Common Blocks:
;	None
;
;Latest Update Comments:
;	09/01/89  KRS	- Version 3.0
;	03/20/91  KRS   - Version 4.0, help option added, replace value
;			  option added.
;	05/18/94  KRS	- Version 5.1, runs under Version 2 IDL.  Update list
;			  included.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines called:
;	IMUPDATE
;------------------------------------------------------------------------------
PRO IMMATH,x,y,updates

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'immath' & RETURN & ENDIF
;
;Error control.
;
	ON_IOERROR,ESCAPE
;
;Initialize
;
	com = STRARR(80,1) & j=0
;
;Print heading.
;
   LOOP:
        PRINT,'IMMATH(v5.2)::  Information Dump Follows'
	PRINT,'IMMATH(v5.2)::  ----------------------------------------'
	PRINT,'IMMATH(v5.2)::  x = ('+STRTRIM(MIN(x),2)+' , ' $
		+STRTRIM(MAX(x),2)+')'
	PRINT,'IMMATH(v5.2)::  y = ('+STRTRIM(MIN(y),2)+' , ' $
		+STRTRIM(MAX(y),2)+')'
	PRINT,'IMMATH(v5.2)::  AVG = '+STRTRIM(AVG(y),2)
	PRINT,'IMMATH(v5.2);;  MED = '+STRTRIM(MEDIAN(y),2)
	PRINT,'IMMATH(v5.2)::  ----------------------------------------'
        PRINT,'IMMATH(v5.2)::  End of Information Dump'
	PRINT,'IMMATH(v5.2)::  (a)bs  (e)xp  (i)nverse  (l)og  (n)atural log'
	PRINT,'IMMATH(v5.2)::  (s)quare root (r)eplace value  (R)eset'
	PRINT,'IMMATH(v5.2)::  (+)Add constant  (*)Multiply constant  (q)uit' 
;
;Get operator.
;
   LOOP1:
	operator = GET_KBRD(1)		
	ysave = y
	loc = WHERE(y NE 0)
	CASE operator OF
   ;
   ;Absolute value of y.
   ;
	'a': BEGIN
	     com(j) = 'IMMATH(v5.2)::  Absolute value of spectrum taken  ' $
		+!stime
	     y = ABS(y)			
	     END
   ;
   ;Base e exponential of y.
   ;
	'e': BEGIN
	     com(j) = 'IMMATH(v5.2)::  Exponential of spectrum taken  '+!stime
	     y = EXP(y)			
	     END
   ;
   ;Reciprocal of y.
   ;
	'i': BEGIN	
	     com(j) = 'IMMATH(v5.2)::  Reciprical of spectrum taken  '+!stime
	     y(loc) = 1.0/y(loc) 	
	     END 
   ;
   ;Base 10 log of y > 0.
   ;
	'l': BEGIN	
	  com(j) = 'IMMATH(v5.2)::  Base 10 log of spectrum taken '+!stime
	  y = y > 0
          y(loc) = ALOG10(y(loc)) 	
	  END
   ;
   ;Base e log of y > 0
   ;
	'n': BEGIN	
	     com(j) = 'IMMATH(v5.2)::  Base e log of spectrum taken '+!stime
	     y = y > 0		
	     y(loc) = ALOG(y(loc))	
	     END
   ;
   ;Replace values.
   ;
	'r': BEGIN	
	     cutoff = 0.0			
	     READ,'IMMATH(v5.2)::  Enter lower cutoff: ',cutoff
	     y = y > cutoff			
	     com(j) = 'IMMATH(v5.2)::  Y-Cutoff of '+STRTRIM(cutoff,2) $
		+' imposed  '+!stime
	     END
   ;
   ;Square root of y.
   ;
	's': BEGIN
	     com(j) = 'IMMATH(v5.2)::  Square root of spectrum taken '+!stime
	     y = y > 0
   	     y = SQRT(y)		
	     END
	
	'*': BEGIN
	     constant = 0.0		
	     READ,'IMMATH(v5.2)::  Enter multiplication constant: ',constant
	     y = y * constant		;Multiply y by constant.
	     com(j) = 'IMMATH(v5.2)::  Constant '+STRTRIM(constant,2) $
		+' mulitplied '+!stime
	     END

	'+': BEGIN
	     constant = 0.0		
	     READ,'IMMATH(v5.2)::  Enter additive constant: ',constant
	     y = y + constant		;Add constant to y.
	     com(j) = 'IMMATH(v5.2)::  Constant '+STRTRIM(constant,2) $
			+' added '+!stime
	     END
	
	'q': BEGIN
             IF j GT 0 THEN FOR k=0,j-1 DO IMUPDATE,updates,';'+com(k)
	     RETURN
	     END

	'R': BEGIN
	     y = ysave
	     PRINT,'IMMATH(v5.2)::  Spectrum restored'
	     com = STRARR(80,1)
 	     j = -1
	     END

   	ELSE: BEGIN
	      PRINT,'IMMATH(v5.2)::  Invalid command: '+operator
	      GOTO,LOOP1
	      END
	ENDCASE
	PRINT,' '
	PLOT,x,y
	j = j+1
	GOTO,LOOP
;----------------------------------------------------------------------------
ESCAPE:
        PRINT,'IMMATH(v5.2)::  '+!err_string
        RETURN  &  END

