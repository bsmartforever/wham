;**********************************************************************
;+
;*NAME:
;    WPOLYFIT     (General IDL Library 01) 7-25-84
; 
;*CLASS:
;    curve fitting
;
;*CATEGORY:
;
;*PURPOSE:
;    TO FIT A WEIGHTED POLYNOMIAL TO DATA POINTS USING THE
;    METHOD OF LEAST-SQUARES.
; 
;*CALLING SEQUENCE:
;    WPOLYFIT,X,Y,WEIGHT,NDEG,A,YFIT,CHISQR
; 
;*PARAMETERS: 
;    X        (REQ) (I) (1) (I L F D)
;             Required input vector containing the independent variable
;    Y        (REQ) (I) (1) (F)
;             Required input vector containing the dependent variable.
;    WEIGHT   (REQ) (I) (0 1) (F)
;             Required input scalar (which is converted to vector) or
;             vector giving the weights for each data point.
;    NDEG     (REQ) (I) (0) (I)
;             Required input scalar denoting the degree of the polynomial 
;             to be fit.
;    A        (REQ) (O) (1) (F)
;             Required output vector containing the coefficients of the
;             polynomial to be fit.
;             YFIT=A(0)+A(1)*X+A(2)*X^2+...A(NDEG)*X^NDEG
;    YFIT     (REQ) (O) (1) (F)
;             Required output vector containing the calculated values, based
;             on the preceding equation, at each value of X.
;    CHISQ    (REQ) (O) (0) (F)
;             Required output scalar denoting the reduced chi square statistic
;             for the fit.
; 
;*EXAMPLES:
;    To fit a quadratic function to spectral continuum data with uniform
;    weighting:
;    WPOLYFIT,WCON,FCON,WCON*0.+1.,2,A,YFIT,CHISQ
;
;*SYSTEM VARIABLES USED:
;    None
;
;*INTERACTIVE INPUT:
;    None
;
;*SUBROUTINES CALLED:
;    determ
;    parcheck
;    pcheck
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
;
;*PROCEDURE:
;    WPOLYFIT is an IDL version of Bevingtons program POLFIT (p. 140)
;    As explained in Bevington, the method of least-squares is used to
;    calculate the coefficients. The routine DETERM is used to calculate
;    the determinant of the constructed matrices.
; 
;*MODIFICATION HISTORY:
;     ????         I.D. Ahmad       initial program
;     Jul 25, 1984  RWT  GSFC corrected CHISQ calculation for NDEG=0
;                             and updated documentation
;     Jan 30, 1985  RWT  GSFC added compilation of PCHECK, allow
;                             scalar WEIGHT and floating point NDEG.
;     Jul 10, 1985  RWT  GSFC correct chisq calculation for NDEG>0,
;                             remove scaling, and add double precision.
;     Sep 23, 1985  RWT  GSFC modified for DIDL (use double 
;                             precision variables, use N_ELEMENTS,
;                             remove scaling, and correct CHISQ calculation
;                             for NDEG>0.
;     Apr 15, 1987  RWT  GSFC add PARCHECK
;     Dec  3, 1987  RWT  GSFC add procedure call listing and correct
;                             error when WEIGHT(0)=0.
;     Mar 21, 1988  CAG  GSFC add VAX RDAF-style prolog.
;     Apr 21, 1988  RWT  GSFC use new DETERM_PDP      
;     Aug 29, 1989  RWT  modify for SUN IDL
;     Mar 4 1991      JKF/ACC    - moved to GHRS DAF (IDL Version 2)
;- 
;**********************************************************************
pro wpolyfit,xin,yin,weight,ndeg,a,yfit,chisqr
;
npar = n_params()
if npar eq 0 then begin
  print,' WPOLYFIT,XIN,YIN,WEIGHT,NDEG,A,YFIT,CHISQR'
  retall & end
parcheck,npar,7,'wpolyfit'
pcheck,xin,1,010,0011
pcheck,yin,2,010,0011
pcheck,weight,3,110,0011
ndeg   = fix(ndeg)
x      = double(xin) 
y      = double(yin)
s      = fix(n_elements(x))
chisq  = 0 & flag=0    ; indicates whether x has been adjusted to avoid overflow
nterms = ndeg+1                   ; number of terms in polynomial
array  = dblarr(nterms,nterms)     ; matrix for solving simultaneous eqns.
a      = dblarr(nterms)
w      = size(weight)
if w(0) lt 1 then weight=fltarr(s) + 1.0
;
; if ndeg=0, simply average y
;
  if ndeg eq 0 then begin
    tw  = total(weight)
    a(0)= total(y*weight)/tw
    a0  = a(0)
    yfit= 0.*x+a0
    chisqr = (yfit-y)
    chisqr = total(weight*chisqr*chisqr)/(s-1)
  end else begin
;
; else proceed with polynomial fit
;
;
; reduce x by an order of magnitude to avoid overflow
;
    while 100000000.^(1./ndeg) le max(x) do begin
      x = x/10.
      flag = flag + 1
    end
;
; accumulate sums
;
    nmax=2*nterms-1
    sumx=dblarr(nmax)
    sumy=a
    indx=indgen(nmax)
    indy=indgen(n_elements(sumy))
;
; accumulate x^n and y*x^n
;
    for i=0,s-1 do begin
      addx=0.*sumx
      addy=0.*sumy
      if x(i) ne 0.0 then begin
        addx=weight(i)*abs(x(i))^indx
        addy=weight(i)*y(i)*abs(x(i))^indy
        if x(i) lt 0. then begin
          addx=addx-2.*(addx)*(indx mod 2)
          addy=addy-2.*(addy)*(indy mod 2)
        end ; x(i) lt 0
      end ; x(i) ne 0
      sumx=sumx+addx
      sumy=sumy+addy
    end ; i
    chisq=chisq+total(weight*y*y)
;
; construct matrices & calculate coefficients
;
    for k=0,ndeg do  $
       for j=0,ndeg do array(j,k)=sumx(j+k)
 ;   for j=0,ndeg do $
 ;     for k=0,ndeg do array(j,k)=sumx(j+k)
    IUE_determ,array,delta
;
; if matrix is singular, end program
;
    if delta eq 0 then begin
      chisqr=0
      a=a*0
    end else begin
      for l=0,ndeg do begin
        arr = array
	arr(0,l)=sumy(0:ndeg)
       ; for j=0,ndeg do arr(j,l)=sumy(j)
        IUE_determ,arr,det
        a(l)=det/delta
      end ; l
;
; calculate reduced chi square
;
      for j=0,ndeg do begin
        chisq=chisq-2.*a(j)*sumy(j)
        for k=0,ndeg do chisq=chisq+a(j)*a(k)*sumx(j+k)
      end ; j
      chisqr = float(chisq)/(s-nterms)
;
; calculate yfit
;
      yfit=fltarr(s)
      xn=yfit+1.
      yfit=yfit+a(0)
      for i=1,ndeg do begin
        xn=xn*x
        yfit=yfit+a(i)*xn
      end ; i loop
;
      if flag ne 0 then begin
        x = x * 10.0^flag
        a = a / (10.0^flag)^indgen(n_elements(a))
      end;  flag ne 0
    end ;  delta ne 0
  end ; ndeg ne 0
return
end   ; wpolyfit