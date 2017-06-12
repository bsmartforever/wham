;***********************************************************************
;+
;*NAME:
;    INTEG     OCTOBER 3, 1980
;
;*CLASS:
;    integration
;
;*CATEGORY:
;
;*PURPOSE:  
;    To numerically integrate tabulated data using the trapezoid rule.
;
;*CALLING SEQUENCE:
;    INTEG,X,Y,IMIN,IMAX,SUM
;
;*PARAMETERS:
;    X    (REQ) (I) (1) (I L F D)  
;         Required input vector of argument values
;
;    Y    (REQ) (I) (1) (I L F D)
;         Required input vector of function values
;
;    IMIN (REQ) (I) (0) (F)
;         Required input scalar containing the effective index of the
;         lower limit of the integration.
;
;    IMAX (REQ) (I) (0) (F)
;         Required input scalar containing the effective index of the
;         upper limit of the integration.
;
;    SUM  (REQ) (O) (0) (F)
;         Required output scalar parameter containing the definite
;         integral of Y between X(IMIN) and X(IMAX).
;
;*EXAMPLES:
;    Integrate the flux of the CIV feature in a spectrum
;    (WAVE vs FLUX) with integration limits of 1545 and
;    1555 Angstroms.
; 
;           TABINV,WAVE,1545.0,IMIN
;           TABINV,WAVE,1555.0,IMAX
;           INTEG,WAVE,FLUX,IMIN,IMAX,FLUXC4
;
;*SYSTEM VARIABLES USED:
;    None
;
;*INTERACTIVE INPUT:
;    None
;
;*SUBROUTINES CALLED:
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
;        IMIN and IMAX may have non-integer values
;
;*PROCEDURE: 
;        The trapezoid rule is used.
;
;
;*MODIFICATION HISTORY:
;    Jan 14 1981  I. Ahmad  GSFC  initial program
;    Jun  5 1987  RWT       GSFC  add PARCHECK and remove EXTRACTs
;    Mar 14 1988  CAG       GSFC  add VAX RDAF-style prolog
;-
;********************************************************************
pro integ,x,y,imin,imax,sum
;
   npar = n_params(0)
   if npar eq 0 then begin
      print,' integ,X,Y,IMIN,IMAX,SUM'
      retall & end
  ilo  = fix(imin)
  ihi  = fix(imax)
  n    = imin + ihi-ilo	               ; ihi-ilo = number of trapezoids
  z    = y(imin+1:n) + y(imin:n-1)
  sum  = total((z/2.) * (x(imin+1:n) - x(imin:n-1)))
  hi   = imax-ihi
  lo   = imin-ilo
  if (ihi lt imax) then sum = sum + (x(ihi+1) - x(ihi)) * hi *  $
     (y(ihi) + hi/2 * (y(ihi+1) - y(ihi)))
  sum  = sum - (x(ilo+1) - x(ilo)) * lo * (y(ilo) + lo/2 * (y(ilo+1) - y(ilo)))
return
end
