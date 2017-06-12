;************************************************************************
;+
;*NAME:
;    YFIT    (General IDL Library 01)
;  
;*CLASS:
;    curve fitting
;
;*CATEGORY:
;  
;*PURPOSE:
;    PROCEDURE TO EVALUATE Y FOR A SET OF FUNCTION PARAMETERS A
;    Y IS ASSUMED TO BE A GAUSSIAN ON TOP OF A POLYNOMIAL BASELINE
;  
;*CALLING SEQUENCE:
;    YFIT,X,M,A,YFIT
;  
;*PARAMETERS:
;    X       (REQ) (I) (1) (F)
;            required input vector containing the independent variable data
;    
;    M       (REQ) (I) (?) (?)
;            fossil parameter which is not used in the code.
;
;    A       (REQ) (I) (1) (F)
;            Required input vector which contains the fit parameters for
;            the multiple gaussian profile.
;            A(3*I) is the center of the Ith component gaussian
;            A(3*I+1) is the gaussian sigma width of the Ith gaussian component
;            A(3*I+2) is the height of the gaussian peak above the baseline
;                    of the Ith component.    
;
;    YFIT    (REQ) (O) (1) (F)
;            Output vector describing multiple gaussian profile. 
;  
;*EXAMPLES:
;    see WFIT.PRO
;
;*SYSTEM VARIABLES USED:
;    None
;
;*INTERACTIVE INPUT:
;    None
;
;*SUBROUTINES CALLED:
;    GAUSS
;    PARCHECK
;
;*FILES USED:
;    None
;  
;*SIDE EFFECTS:
;    None
;  
;*RESTRICTIONS:
;    None
;  
;*NOTES:
;    None
;  
;*PROCEDURE:
;    The composite gaussian profile is built up by the sum of the
;    individual gaussian component distributions. The independent variable
;    grid is specified by the input variable X.
;  
;*MODIFICATION HISTORY:
;    Aug. 18, 1979  I.D. Ahmad  initial program
;    Mar  21, 1988  CAG    GSFC add VAX RDAF-style prolog, calling sequence
;                               printout and PARCHECK
;    Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;************************************************************************
PRO YFIT,X,M,A,YFIT
IF N_PARAMS() EQ 0 THEN BEGIN
   PRINT,'YFIT,X,M,A,YFIT'
   RETALL & ENDIF
PARCHECK,N_PARAMS(),4,'YFIT'
;
YFIT = X * 0.0 
FOR I=0,N_ELEMENTS(A)/3-1 DO BEGIN
  N = 3*I
  GAUSS,X,A(N),A(N+1),A(N+2),Y
  YFIT=YFIT+Y
  END ; I LOOP
RETURN
END  ; YFIT