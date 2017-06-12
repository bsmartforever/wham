PRO EULERPLUS,AI,BI,AO,BO,SELECT, FK4 = FK4, SELECT = select1, RADIAN=radian
;+
; NAME:
;     EULERPLUS
; PURPOSE:
;     Transform between Galactic, celestial, ecliptic coordinates, and Magellanic.
; EXPLANATION:
;     Use the procedure ASTRO to use this routine interactively
;
; CALLING SEQUENCE:
;      EULER, AI, BI, AO, BO, [ SELECT, /FK4, /RADIAN, SELECT = ] 
;
; INPUTS:
;       AI - Input Longitude, scalar or vector.  In DEGREES unless /RADIAN
;            is set.  If only two parameters are supplied, then  AI and BI 
;             will be modified to contain the output longitude and latitude.
;       BI - Input Latitude in DEGREES
;
; OPTIONAL INPUT:
;       SELECT - Integer (1-6) specifying type of coordinate transformation.  
;
;      SELECT   From          To        |   SELECT      From            To
;       1     RA-Dec (2000)  Galactic   |     4       Ecliptic      RA-Dec    
;       2     Galactic       RA-DEC     |     5       Ecliptic      Galactic  
;       3     RA-Dec         Ecliptic   |     6       Galactic      Ecliptic  
;	      7     Galactic       Magellanic |     8       Magellanic    Galactic 
;
;      If not supplied as a parameter or keyword, then EULER will prompt for 
;      the value of SELECT
;      Celestial coordinates (RA, Dec) should be given in equinox J2000 
;      unless the /FK4 keyword is set.
; OUTPUTS:
;       AO - Output Longitude in DEGREES, always double precision
;       BO - Output Latitude in DEGREES, always double precision
;
; OPTIONAL INPUT KEYWORD:
;       /FK4 - If this keyword is set and non-zero, then input and output 
;             celestial and ecliptic coordinates should be given in equinox 
;             B1950.
;       /RADIAN - if set, then all input and output angles are in radians rather
;             than degrees.
;       SELECT  - The coordinate conversion integer (1-6) may alternatively be 
;              specified as a keyword
; EXAMPLE:
;       Find the Galactic coordinates of Cyg X-1 (ra=299.590315, dec=35.201604)
;       IDL> ra = 299.590315d
;       IDL> dec = 35.201604d
;       IDL> euler,ra,dec,glong,glat,1 & print,glong,glat 
;            71.334990, 3.0668335
; REVISION HISTORY:
;       Written W. Landsman,  February 1987
;       Adapted from Fortran by Daryl Yentis NRL
;       Made J2000 the default, added /FK4 keyword  W. Landsman December 1998
;       Add option to specify SELECT as a keyword W. Landsman March 2003
;       Use less virtual memory for large input arrays W. Landsman June 2008
;       Added /RADIAN input keyword  W. Landsman   Sep 2008
;-
 On_error,2
 compile_opt idl2

 npar = N_params()
 if npar LT 2 then begin
    print,'Syntax - EULER, AI, BI, A0, B0, [ SELECT, /FK4, /RADIAN, SELECT= ]'
    print,'    AI,BI - Input longitude,latitude in degrees'
    print,'    AO,BO - Output longitude, latitude in degrees'
    print,'    SELECT - Scalar (1-8) specifying transformation type'
    return
 endif

  twopi   =   2.0d*!DPI
  fourpi  =   4.0d*!DPI
  rad_to_deg = 180.0d/!DPI

;   J2000 coordinate conversions are based on the following constants
;   (see the Hipparcos explanatory supplement).
;  eps = 23.4392911111d              Obliquity of the ecliptic
;  alphaG = 192.85948d               Right Ascension of Galactic North Pole
;  deltaG = 27.12825d                Declination of Galactic North Pole
;  lomega = 32.93192d                Galactic longitude of celestial equator  
;  alphaE = 180.02322d              Ecliptic longitude of Galactic North Pole
;  deltaE = 29.811438523d            Ecliptic latitude of Galactic North Pole
;  Eomega  = 6.3839743d              Galactic longitude of ecliptic equator              

;; Galactic - Magellanic based on Nidever et al. 2008 using Westmeier
;;   Euler angle formulation at:
;;      http://www.atnf.csiro.au/people/Tobias.Westmeier/tools_coords.php
;;
;; lambda_0 = psi tweaked from Westmeier's value to make LMC center (l = 280.47, b = -32.75)
;;   as specified in van der Marel et al. 2002 (by Nidever et al 2008) exactly L_MS = 0.
;;   This is the value in parentheses for psi[6].

  if keyword_set(FK4) then begin 

  equinox = '(B1950)' 
  psi   = [ 0.57595865315D, 4.9261918136D,  $
            0.00000000000D, 0.0000000000D,  $  
            0.11129056012D, 4.7005372834D,  $
            0.57353264105506241D - (0.13678513598655057D * !dtor), 4.8607419291511178D ]
  stheta =[ 0.88781538514D,-0.88781538514D, $
            0.39788119938D,-0.39788119938D, $
            0.86766174755D,-0.86766174755D, $
            0.99144486309440161D, -0.99144486309440161D]    
  ctheta =[ 0.46019978478D, 0.46019978478D, $
            0.91743694670D, 0.91743694670D, $
            0.49715499774D, 0.49715499774D, $
            -0.13052617915086359D, -0.13052617915086359D]    
   phi  = [ 4.9261918136D,  0.57595865315D, $
            0.0000000000D, 0.00000000000D, $
            4.7005372834d, 0.11129056012d, $
            psi[7], psi[6]]


 endif else begin 

  equinox = '(J2000)'
  psi   = [ 0.57477043300D, 4.9368292465D,  $
            0.00000000000D, 0.0000000000D,  $  
            0.11142137093D, 4.7127941937D,  $
            0.57353264105506241D - (0.13678513598655057D * !dtor), 4.8607419291511178D ]     

  stheta =[ 0.88998808748D,-0.88998808748D, $
            0.39777715593D,-0.39777715593D, $
            0.86766622025D,-0.86766622025D, $
            0.99144486309440161D, -0.99144486309440161D ]  
  
  ctheta =[ 0.45598377618D, 0.45598377618D, $
            0.91748206207D, 0.91748206207D, $
            0.49714719172D, 0.49714719172D, $
            -0.13052617915086359D, -0.13052617915086359D ]  
  
   phi  = [ 4.9368292465D,  0.57477043300D, $
            0.0000000000D,  0.00000000000D, $
            4.71279419371d, 0.11142137093d, $
            psi[7], psi[6]]

 endelse
;
 if N_elements(select) EQ 0 then $
          if N_elements(select1) EQ 1 then select=select1
 if N_elements(select) EQ 0 then begin
        print,' '
        print,' 1 RA-DEC ' + equinox + ' to Galactic'
        print,' 2 Galactic       to RA-DEC' + equinox
        print,' 3 RA-DEC ' + equinox + ' to Ecliptic'
        print,' 4 Ecliptic       to RA-DEC' + equinox
        print,' 5 Ecliptic       to Galactic'
        print,' 6 Galactic       to Ecliptic'
        print,' 7 Galactic       to Magellanic'
        print,' 8 Magellanic     to Galactic'
;
        select = 0
        read,'Enter selection: ',select
 endif

 I  = select - 1                         ; IDL offset
 if npar EQ 2 then begin

      if keyword_set(radian) then begin 
         ao = temporary(ai) - phi[i]
         bo = temporary(bi)
      endif else begin  
         ao = temporary(ai)/rad_to_deg - phi[i]
         bo = temporary(bi)/rad_to_deg
      endelse 
      
      endif else begin 
      if keyword_set(radian) then begin 
           ao = ai - phi[i]
	   bo = bi
      endif else begin      
          ao  = ai/rad_to_deg - phi[i]
          bo = bi/rad_to_deg
       endelse	  
 endelse
 sb = sin(bo) &	cb = cos(bo)
 cbsa = cb * sin(ao)
 bo  = -stheta[i] * cbsa + ctheta[i] * sb
 bo    = asin(bo<1.0d)
 if ~keyword_set(radian) then bo = bo*rad_to_deg
;
 ao =  atan( ctheta[i] * cbsa + stheta[i] * sb, cb * cos(ao) )
 ao = ( (ao+psi[i]+fourpi) mod twopi) 
 if ~keyword_set(radian) then ao = ao*rad_to_deg


 if ( npar EQ 2 ) then begin
	ai = temporary(ao) & bi=temporary(bo)
 endif
 return
 end
