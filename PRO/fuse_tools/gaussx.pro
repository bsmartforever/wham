;***********************************************************************
;+
;*NAME:  
;
;    GAUSSX   (General IDL Library 01)  Aug 19, 1979 
;
;*CLASS:
;
;    numerical function
;
;*CATEGORY:
;
;*PURPOSE:  
;
;    TO CALCULATE A GAUSSIAN FUNCTION
; 
;*CALLING SEQUENCE: 
;
;    GAUSS,X,X0,DX,YMAX,Y
; 
;*PARAMETERS:
;
;    X     (REQ) (I) (0 1) (I L F D)
;          required scalar or vector containing the independent variable(s)
;
;    X0    (REQ) (I) (0)   (F D)
;          required scalar giving the center of the Gaussian function
;          This parameter must have the same units as X.
;
;    DX    (REQ) (I) (0)   (F D)
;          required scalar giving the one sigma width of the distribution
;          This parameter must have the same units as X.
;
;    YMAX  (REQ) (I) (0)   (F D)
;          the Gaussian value at the peak of the distribution
; 
;    Y     (REQ) (O) (0 1) (F D)
;          required output scalar or vector giving the calculated value
;          of the gaussian from the expression:
;          Y = YMAX * EXP (-0.5 * ((X-X0)/DX)^2)
; 
;*EXAMPLES:
;
;    To calculate a gaussian with center at 1545 A, sigma of 2 A, using the
;    wavelength scale derived from an IUE spectrum, with amplitude 1.0,
; 
;     GAUSSX,WAVE,1545.,2.,1.0,Y
;
;*SYSTEM VARIABLES USED:
;
;*INTERACTIVE INPUT:
;
;*SUBROUTINES CALLED:
;
;     PARCHECK
;     PCHECK
;
;*FILES USED:
;
;*SIDE EFFECTS:
;
;*RESTRICTIONS:
;
;*NOTES:
;
;    Values for which (X-X0)/DX > 9 are set to zero.
;    If DX = 0, the delta function is returned.
;
;	tested with IDL Version 2.1.0 (sunos sparc)  	25 Jun 91
;	tested with IDL Version 2.1.0 (ultrix mispel)	N/A
;	tested with IDL Version 2.1.0 (vms vax)      	25 Jun 91
; 
;*PROCEDURE: 
;
;    GAUSSX is similiar to Bevingtons program PGAUSS (p.45)
;
;*MODIFICATION HISTORY:
;
;    Aug 19 1979  I. Ahmad  initial program
;    Jul  7 1984  RWT GSFC  updated documentation
;    Sep 25 1984  RWT GSFC  changed limit from 12 sigma to 9 sigma due to
;                           problems in WFIT. Also compiles PCHECK.
;    Apr 13 1987  RWT GSFC  add PARCHECK
;    Aug 19 1987  RWT GSFC  add procedure call listing
;    Mar  9 1988  CAG GSFC  add VAX RDAF-style prolog
;    Jun 25 1991  PJL GSFC  cleaned up; lowercase; tested on SUN and VAX;
;			    updated prolog
;    Jan 23 1995  JKF Adapted IUE RDAF updated version(req'd for GAUSSFITS).
;
;-
;***********************************************************************
 pro gaussx,x,x0,dx,ymax,y
;
 npar = n_params(0)
 if npar eq 0 then begin
    print,' GAUSSX,X,X0,DX,YMAX,Y'
    retall
 endif  ; npar
 if dx ne 0 then begin
    arg=(abs((x-x0)/dx)<9.)   ; set values 9 sigma to 0 to avoid trap errors
    y=exp(-arg*arg/2)*(arg lt 9.0)
 endif else y=(0.*x)*(x ne x0)+(x eq x0) ; if dx eq 0 return delta function
 y=y*ymax
 return
 end  ; gauss
