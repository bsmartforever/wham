;***********************************************************************
;+
;*NAME:
;    LINFIT   (General IDL Library 01)  June, 1984
; 
;*CLASS:
;    Curve Fitting
;
;*CATEGORY:
;   
;*PURPOSE:
;    To make a least squares fit to data with a straight line:   
;    Y = A + B*X  (IDL version of Bevington's LINFIT program)
; 
;*CALLING SEQUENCE: 
;    LINFIT,X,Y,WT,A,B,SI
; 
;*PARAMETERS:
;    X    (REQ) (I) (1) (F D)
;         Required input vector containing the independent variable data
;    Y    (REQ) (I) (1) (F D)
;         Required input vector containing the dependent variable data
;    WT   (REQ) (I) (1) (F D)
;         Required input vector containing the weighting function data
;         as described by Bevington (p.104). If uniform weighting is 
;         desired, set WT=1.0+0*X. For instrumental uncertainties, use
;         WT=1/sigmay^2.
;    A    (REQ) (O) (0) (F)
;         Required output scalar containing the y-intercept for the least
;         squares fit.
;    B    (REQ) (O) (0) (F)
;         Required output scalar containing the slope of the least squares
;         line.
;    SI   (OPT) (O) (1) (F)
;         Optional output vector containing statistical information on the
;         fit. 
;         SI(0)= correlation coefficient (R) where 1. is a perfect fit
;                & 0. represents no correlation (see STATISTICAL TREATMENT
;                OF EXPERIMENTAL DATA by Young p.130)
;         SI(1)= 1 sigma error of coefficient A (SIGMAA)
;         SI(2)= 1 sigma error of coefficient B (SIGMAB) 
;         SI(3)= mean value of Y array
;         if no weighting is specified the following are included:
;         SI(4)= scatter about mean (SQRT( TOTAL((Y-YAVG)^2)/N-1 ))
;         SI(5)= scatter about fit (SQRT( TOTAL((Y-(A+BX))^2)/N-2 ))
;
;*EXAMPLES:
;     To fit measurements of equivalent width (EQW) to a linear function
;     of time with uniform weighting of the individual points:
;     WT=0.*TIME+1.0
;
;     LINFIT,TIME,EQW,WT,A,B,SI
;     YFIT=B*TIME+A
;     !PSYM=4
;     PLOT,TIME,EQW
;     !PSYM=0
;     OPLOT,TIME,YFIT
;
;*SYSTEM VARIABLES USED:
;     None
;
;*INTERACTIVE INPUT:
;     None
;
;*SUBROUTINES CALLED:
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
;    Typing LINFIT without any parameters will display the 
;    procedure call statement.
; 
;*PROCEDURE:
;   Least squares (for further infomation see DATA REDUCTION
;   AND ERROR ANALYSIS FOR THE PHYSICAL SCIENCES by Bevington
;   pp. 92-118).
;   Modifications were made (9-25-84 RWT) which makes the procedure
;   differ slightly from Bevington's. Instead of fitting the
;   expression
;          Y = A + B*X,
;   LINFIT calculates
;         Y = a + b*(x-<X>)
;   where <X> = TOTAL(X*WT)/TOTAL(WT). LINFIT converts back to the
;   original expression using the equations:
;         A = a - b*<X>
;         B = b
;         SIGMAA = SQRT(SIGMAa^2 + SIGMAb^2*<X>^2)
;         SIGMAB = SIGMAb
;
;*MODIFICATION HISTORY: 
;    Jun    1984  RWT   GSFC  initial program based on Bevington's LINFIT
;    Jun    1984  NRE   GSFC  add weighting and documentation
;    Jul  5 1984  RWT   GSFC  more flexible weighting allowed
;    Jul 30 1984  RWT   GSFC  fix error in creating WT vector and add
;                             test of array sizes and zero denominator
;    Aug  7 1984  RWT   GSFC  combined statistical output into 1 array
;                             and avoid underflow/overflow errors
;    Sep  7 1984  RWT   GSFC  X and Y input arrays must be 1-dimensional
;                             arrays, containing at least 2 points. Output
;                             parameters are all set to zero if initial tests
;                             fail. PCHECK added.
;    Sep 25 1984  RWT   GSFC  found that subtracting mean from X array can
;                             improve accuracy of fit. (Apparently due to a 
;                             limitation in precision in IDL).
;    Oct 20 1985  RWT   GSFC  Modified for DIDL (uses # for @, NELEMENTS,
;                             and double precision.)
;    Apr 14 1987  RWT   GSFC  VAX mods: use PARCHECK
;    Aug 19 1987  RWT   GSFC  Make SI optional, and add listing of procedure
;                             call statement.
;    Mar 14 1988  CAG   GSFC  add VAX RDAF-style prolog.
;    Mar 11 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;***********************************************************************
PRO LINFIT,X,Y,WT,A,B,SI
NPAR = N_PARAMS(0)
IF NPAR EQ 0 THEN BEGIN
   PRINT,' LINFIT,X,Y,WT,A,B,SI'
   RETALL & END
PARCHECK,NPAR,[5,6],'LINFIT'
SI = FLTARR(6)
A  = 0.0 & B = 0.0
;
; CHECK TYPE & SIZES OF INPUT ARRAYS
;
PCHECK,X,1,010,0111
PCHECK,Y,2,010,0111
PCHECK,WT,3,110,0111
NX = N_ELEMENTS(X)
NY = N_ELEMENTS(Y)
IF NX NE NY THEN BEGIN
  PRINT,'ERROR IN LINFIT: SIZE OF INPUT ARRAYS DISAGREE'
  RETURN & END
IF (NX LT 2) OR (NY LT 2) THEN BEGIN
  PRINT,'LINFIT RETURNING: INPUT ARRAY(S) TOO SMALL'
  RETURN & END
;
; IF WT IS SCALAR (I.E. NO WEIGHTING) CHANGE TO VECTOR
;
S=SIZE(WT)
IF S(0) LT 1 THEN WT = FLTARR(NY) + WT  
;
; CALCULATE MEAN OF X & SUBTRACT FROM X ARRAY
;
WXAV = TOTAL(WT*X) / TOTAL(WT)
XS   = X - WXAV
;
; CALCULATE COEFFICIENTS A & B
;
SUM = TOTAL(DOUBLE(WT))
SX  = TOTAL(DOUBLE(XS*WT)) & SX2 = TOTAL(DOUBLE(XS*XS*WT))
SY  = TOTAL(DOUBLE(Y*WT))  & SY2 = TOTAL(DOUBLE(Y*Y*WT)) 
SXY = TOTAL(DOUBLE(XS*Y*WT))
DEL = DOUBLE(SUM*SX2 - SX*SX)
IF DEL EQ 0.0D0 THEN BEGIN
  PRINT,'ERROR IN LINFIT: ZERO DENOMINATOR FOUND'
  RETURN & END
AS = (SX2*SY - SX*SXY) / DEL
BS = (SUM*SXY - SX*SY) / DEL
A  = AS - BS * WXAV
B  = BS
;
; CALCULATE STANDARD DEVIATIONS & CORRELATION COEFICIENTS 
;
SI(0) =(SUM*SXY-SX*SY)/SQRT(DEL*(SUM*SY2-SY*SY))     ; correl. coeff.
SI(3) = TOTAL(Y)/NY                                  ; mean of Y array
IF (S(0) GT 0) THEN VAR = 1. ELSE BEGIN
  VAR = (SY2 +AS*AS*SUM +BS*BS*SX2 -2.0*(AS*SY+BS*SXY-AS*BS*SX)) / (NY-2) 
  SI(4) = SQRT(TOTAL((Y-SI(3))*(Y-SI(3)))/(NY-1))    ; scatter about mean
  IF (VAR GE 0.0) THEN SI(5) = SQRT(VAR)             ; est. parent sigma
  END
IF VAR GE 0.0 THEN BEGIN
  SI(1)=SQRT(VAR*SX2/DEL)                             ; sigmaa 
  SI(2)=SQRT(VAR*SUM/DEL)                             ; sigmab 
  SI(1)=SQRT(SI(1)*SI(1) + WXAV*WXAV*SI(2)*SI(2))     ; sigmaA 
  END ELSE PRINT,'VARIANCE LT 0.0, LINFIT CONTINUING'
RETURN
END