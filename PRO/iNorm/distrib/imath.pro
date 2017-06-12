;+
;				IMATH.PRO
;				Version1.0
;
;Description:
;	This routine performs various mathematical operations on a 
;	spectrum. Limited use.  
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
;	04/13/13  NL	- Version 1.0
;
;External Routines called:
;
;------------------------------------------------------------------------------
PRO IMATH,x,y

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imath' & RETURN & ENDIF
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
        PRINT,'iMATH::  Information Dump Follows'
	PRINT,'iMATH::  ----------------------------------------'
	PRINT,'iMATH::  x = ('+STRTRIM(MIN(x),2)+' , ' $
		+STRTRIM(MAX(x),2)+')'
	PRINT,'iMATH::  y = ('+STRTRIM(MIN(y),2)+' , ' $
		+STRTRIM(MAX(y),2)+')'
	PRINT,'iMATH::  AVG = '+STRTRIM(AVG(y),2)
	PRINT,'iMATH;;  MED = '+STRTRIM(MEDIAN(y),2)
	PRINT,'iMATH::  ----------------------------------------'
        PRINT,'iMATH::  End of Information Dump'
	PRINT,'iMATH::  (a)bs  (e)xp  (i)nverse  (l)og  (n)atural log'
	PRINT,'iMATH::  (s)quare root (r)eplace value  (R)eset'
	PRINT,'iMATH::  (+)Add constant  (*)Multiply constant  (q)uit' 
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
	     com(j) = 'iMATH::  Absolute value of spectrum taken  ' $
		+!stime
	     y = ABS(y)			
	     END
   ;
   ;Base e exponential of y.
   ;
	'e': BEGIN
	     com(j) = 'iMATH::  Exponential of spectrum taken  '+!stime
	     y = EXP(y)			
	     END
   ;
   ;Reciprocal of y.
   ;
	'i': BEGIN	
	     com(j) = 'iMATH::  Reciprical of spectrum taken  '+!stime
	     y(loc) = 1.0/y(loc) 	
	     END 
   ;
   ;Base 10 log of y > 0.
   ;
	'l': BEGIN	
	  com(j) = 'iMATH::  Base 10 log of spectrum taken '+!stime
	  y = y > 0
          y(loc) = ALOG10(y(loc)) 	
	  END
   ;
   ;Base e log of y > 0
   ;
	'n': BEGIN	
	     com(j) = 'iMATH::  Base e log of spectrum taken '+!stime
	     y = y > 0		
	     y(loc) = ALOG(y(loc))	
	     END
   ;
   ;Replace values.
   ;
	'r': BEGIN	
	     cutoff = 0.0			
	     READ,'iMATH::  Enter lower cutoff: ',cutoff
	     y = y > cutoff			
	     com(j) = 'iMATH::  Y-Cutoff of '+STRTRIM(cutoff,2) $
		+' imposed  '+!stime
	     END
   ;
   ;Square root of y.
   ;
	's': BEGIN
	     com(j) = 'iMATH::  Square root of spectrum taken '+!stime
	     y = y > 0
   	     y = SQRT(y)		
	     END
	
	'*': BEGIN
	     constant = 0.0		
	     READ,'iMATH::  Enter multiplication constant: ',constant
	     y = y * constant		;Multiply y by constant.
	     com(j) = 'iMATH::  Constant '+STRTRIM(constant,2) $
		+' mulitplied '+!stime
	     END

	'+': BEGIN
	     constant = 0.0		
	     READ,'iMATH::  Enter additive constant: ',constant
	     y = y + constant		;Add constant to y.
	     com(j) = 'iMATH::  Constant '+STRTRIM(constant,2) $
			+' added '+!stime
	     END
	
	'q': BEGIN
	     RETURN
	     END

	'R': BEGIN
	     y = ysave
	     PRINT,'iMATH::  Spectrum restored'
	     com = STRARR(80,1)
 	     j = -1
	     END

   	ELSE: BEGIN
	      PRINT,'iMATH::  Invalid command: '+operator
	      GOTO,LOOP1
	      END
	ENDCASE
	PRINT,' '
	PLOT,x,y
	j = j+1
	GOTO,LOOP
;----------------------------------------------------------------------------
ESCAPE:
        PRINT,'iMATH::  '+!err_string
        RETURN  &  END

