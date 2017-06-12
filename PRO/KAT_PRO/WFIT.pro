;*********************************************************************
;+
;*NAME:
;     WFIT        (General IDL Library 01) JUNE, 1984
;
;*CLASS:
;     Curve Fitting
;
;*CATEGORY:
;      
;*PURPOSE:  
;     To fit a nonlinear function to weighted data points using
;     linearization of the fitting function (i.e. an IDL version of
;     Bevingtion's program CURFIT).
; 
;*CALLING SEQUENCE: 
;     WFIT,X,Y,WEIGHT,CONST,IFIT,DELTAA,A,YFIT,SIGMA
; 
;*PARAMETERS:
;     X       (REQ) (I) (1) (F)
;             Required input vector containing the independent variable data
;     Y       (REQ) (I) (1) (F)
;             Required input vector containing the dependent variable data
;     WEIGHT  (REQ) (I) (1) (F)
;             Required input vector containing the data point weights:
;                 for instrumental errors use WEIGHT=1/sigmay^2,
;                 for statistical errors use WEIGHT=1/Y^2,
;                 (see Bevington for further information on weighting).
;     CONST   (REQ) (I) (1) (F)
;             Required input vector of fixed parameters
;     IFIT    (REQ) (I) (1) (F)
;             Required input vector denoting fit parameters which are allowed
;             to vary. 
;             IFIT has the same number of elements as vector A with
;             0's for elements which are to be fixed and 1's for
;             elements allowed to vary.
;     DELTAA  (REQ) (I) (1) (F)
;             Required input vector of increments for parameter A, none
;             of which may be zero.
;     A       (REQ) (I/O) (1) (F)
;             Required input vector of starting fit parameters.
;             This parameter is used to return the final fit parameters.
;     YFIT    (REQ) (O) (1) (F)
;             Vector of fitted dependent data calculated with A.
;     SIGMA   (REQ) (O) (1) (F)
;             Vector of 1 sigma errors corresponding to A
;
;*EXAMPLES:
;    see GAUSSFITS
;
;*SYSTEM VARIABLES USED:
;    None
;
;*INTERACTIVE INPUT
;    None
;
;*SUBROUTINES CALLED:
;    PCHECK
;    PARCHECK
;    GAUSS - (used by subroutine YFIT)
;    YFIT - evaluates Y for a set of parameters A and the array of
;               independent variables X. Y is assumed to be a Gaussian on
;               top of a polynomial baseline.
; 
;*FILES USED:
;    None
;
;*SIDE EFFECTS:
;    None
;
;*RESTRICTIONS: modified for unix/sun idl version 1.1
;    None.
;
;*NOTES:
;     - The function INVERT is used for matrix inversion & provides a 
;          warning if there is danger of a loss of accuracy.
;     - The function used in WFIT is specified in the routine YFIT.
;          This routine currently uses a Gaussian function.
;     - Users interested in applying other functions should create a separate
;          routine called YFIT and compile it after compiling WFIT.
;     - Typing WFIT without any parameters will display the procedure call
;          statement.
; 
;*PROCEDURE: 
;     The procedure uses the same technique as Bevington's CURFIT, to which
;     the user is referred for several chapters of discussion.
;     For a function Y of parameters A(I) the routine uses a linear
;     expansion of Y as a function of A(I).  The procedure uses
;     least squares to find the parameter increments which minimize the
;     chi-squared function.  The program automatically iterates through
;     parameter step sizes which are controlled by the parameter lambda.
;     One sigma errors are approximated by the inverse of the curvature
;     matrix, assuming the error in Y(I)=1. If WEIGHT is derived from the
;     uncertainty in a point Y(I), then absolute errors are produced.
;
;*MODIFICATION HISTORY:
;    Oct  3, 1980  I. Dean Ahmad    initial program based on BEVINGTON
;    Jul     1981  TBA  GSFC to include weighting
;    Jun     1984  NRE  GSFC to use IDL INVERT, change to 
;                            1/(SIGMA^2) weighting, and document
;    Jul 13, 1984  RWT  GSFC updated documentation, modified use
;                            of WEIGHT passed to CHISQ (i.e. doesn't
;                            need to be 1/(simay^2), and made YFIT
;                            an internal subroutine.
;    Sep 20, 1984  RWT  GSFC added IFIT parameter (and related mods.),
;                            and limited number of allowed iterations
;                            to <11.
;    Oct 20, 1985  RWT  GSFC modified for DIDL (i.e. use NELEMENTS,
;                            move $'s and make ARRAY double precision).
;    Apr 15, 1987 RWT  GSFC VAX mods: add PARCHECK and remove INSERTS
;    Aug 19, 1987 RWT  GSFC add procedure call listing
;    Mar  1, 1987 RWT  GSFC make YFIT a separate procedure.
;    Mar 21, 1987 CAG  GSFC add VAX RDAF-style prolog.
;    Jun 10, 1988 RWT  GSFC replace SIGMA(J) with SIGMA(INDEX(J))
;                           to move error to proper element when IFIT
;                           is used to fix variables 
;    Jun 14, 1988 RWT  GSFC replace most DO loops with array operators, use
;                           double precision, remove CHISQ, and optimize code
;    Aug 22, 1989 RWT  GSFC mods. for UNIX IDL
;    dec 22  1989 jtb@gsfc  prevent divide by zero error in alpha matrix
;                           manipulation
;    Mar 11 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;-
;******************************************************************************
pro wfit,x,y,weight,const,ifit,deltaa,a,yfit,sigma
; 
npar = n_params()
if npar eq 0 then begin
   print,' wfit,x,y,weight,const,ifit,deltaa,a,yfit,sigma'
   retall & end
parcheck,npar,9,'wfit'
pcheck,x,1,010,0011
pcheck,y,2,010,0011
pcheck,weight,3,010,0011
pcheck,const,4,110,0011
pcheck,ifit,5,010,0111
pcheck,deltaa,6,010,0011
pcheck,a,7,010,0011
nfit = fix(total(ifit))       ;  number of parameters allowed to vary
npts = n_elements(y)
nterms = n_elements(a)
nmax = nfit - 1
sigma=fltarr(nterms)
nfree=npts-nfit
diag = indgen(nfit) * (nfit + 1)
beta=fltarr(nfit)
wtarr = weight # ( fltarr(nfit) + 1.00d+00 )
d=fltarr(nfit,npts)
;
; compute book-keeping array 'index' from ifit and nterms
;
index = lonarr(nterms)            ;  index same size as a
index(0) = where(ifit ne 0)       ;  insert indices of non-zero terms
if(nfit lt nterms) then index(nfit) = where(ifit eq 0)  ; fill in rest
;
; if deltaa(i) = 0, or nfree < 0, then return to calling procedure
;
for i=0,nterms-1 do if deltaa(i) eq 0 then begin
    print,'error in wfit: deltaa(',i,') equals zero!'
    return & end
if nfree le 0 then begin
    print,'warning:  wfit not executed, nfree < 1'
    return & end   ; nfree le 0
;
; evaluate alpha and beta matrices
;
lambda = 1.00d-03
condit=-1.    ; condit is a measure of convergence from previous iteration
c2prev = -1.
n=0
repeat begin
  n=n+1
  if n gt 10 then begin
    print,'warning: >10 iterations used in wfit, check inputs; returning'
    a(*) = 0.0
    return & end
  yfit,x,const,a,yfit
  ydif=(y-yfit)*weight           ; factor used in calculation of beta
  for j=0,nmax do begin
    b = a
    jj = index(j)
    b(jj) = a(jj) + deltaa(jj)
    yfit,x,const,b,yf
    b(jj) = a(jj) - deltaa(jj)
    yfit,x,const,b,ym
    deriv=(yf-ym)/(2.*deltaa(jj))
    beta(j)=total(ydif*deriv)
    d(j,0) = transpose(deriv)
    end ; j loop
;
; evaluate alpha 
;
    alpha = d # (wtarr * transpose(d))
    if nfit eq 1 then alpha = dblarr(1,1) + alpha
;
; evaluate chi squared at starting point
;
    chisq1 = total( weight*(y-yfit)*(y-yfit) ) / nfree
;
; invert modified curvature matrix to find new parameter
;
    c = sqrt(alpha(diag) # alpha(diag))
    array = alpha
    ind0 = where( c eq 0,count0)
    ind1 = where( c ne 0,count1)
    res = check_math(1,1)
;
    repeat begin
;
;  perform ( array = alpha / c ) with checks for divide by zero
       if count0 gt 0 then array(ind0)=0
       if count1 gt 0 then array(ind1)=alpha(ind1)/c(ind1)
;
       array(diag) = 1.0d+00 + lambda
       array = invert(array)
;
;  perform ( bt =  array/c # beta ) with checks for divide by zero
       if count0 gt 0 then array(ind0)=0
       if count1 gt 0 then array(ind1)=array(ind1)/c(ind1)
       bt = array # beta
;
       b = a
       for j=0,nmax do b(index(j)) = b(index(j)) + bt(j)    ; new parameters
;
;      if chi squared increases, increase lambda and try again
;
       yfit,x,const,b,yfit
       chisqr = total( weight*(y-yfit)*(y-yfit) ) / nfree
       if chisq1 lt chisqr then lambda=10.*lambda
       end until chisq1 ge chisqr
;
; if chi squared decreases, decrease lambda and try again
;
    lambda=lambda/10.0d+00
;
; evaluate parameters and uncertainties
;  in 1st pass condit=1; else = fractional change in chisqr
;
    a=b
    condit = (c2prev/chisqr-1.) * (condit ge 0) + (condit lt 0)
    c2prev=chisqr
    end until ((condit lt .01) or (lambda le 1.e-6)) or (chisqr le 1.e-10)
for j=0,nmax do sigma(index(j)) = sqrt(array(j,j)/alpha(j,j))
if check_math(0,0) ne 0 then begin
   print,'warning: math error(s) occurred in wfit'
   print,'program continuing'
   end
return
end  ; wfit