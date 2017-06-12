PRO pmgal,ra,dec,pma0,pmd0,mul,mub

;+
; This program converts proper motions in celestial
; coordinates to proper motions in galactic coordinates
;
; INPUT:
;  ra    Right ascension (J2000.0) in degrees. Can be an array.
;  dec   Declination (J2000.0) in degrees.  Can be an array.
;  pma   Proper motion in RA, mu_RA * cos(dec) (in mas/year).
;        NOTE: This is the TRUE angular proper motion, and must
;        be corrected for the cos(dec) to get the proper motion
;        in the coordinate
;  pmd   Proper motion in DEC.
;  
; OUTPUT:
;  mul   Proper motion in Galactic Longitude, mu_l * cos(b).
;        NOTE: This is the TRUE angular proper motion
;  mub   Proper motion in Galactic Latitude.
;
; From Allyson Pollak 2006
; Modified by David Nidever   March 2006
;-

; Not enough inputs
if n_elements(ra) eq 0 or n_elements(dec) eq 0 then begin
  print,'Syntax - pmgal,ra,dec,pma,pmd,mul,mub'
  return
endif

ra1 = double(ra)
dec1 = double(dec)
pma = double(pma0)
pmd = double(pmd0)

; To convert from radians to degrees
d2r = !dpi/180.d0
r2d = 180.0d0/!dpi

; Convert RA and Dec to l and b
glactc,ra1,dec1,2000.0,l,b,1,/degree

; General Algorithm:  Convert pmra and pmdec to mul and mub 
;    by getting the star's position for next year by moving
;    it using the proper motion.  Then get the current and
;    next year's position in galactic coordinates.  The
;    difference between the two gives the proper motion in
;    galactic coordinates.  Need to take into account cos(dec)
;    and cos(b) properly.

; Convert proper motion to deg/year
pma2 = double(pma)/3600.0d0/1000.0d0
pmd2 = double(pmd)/3600.0d0/1000.0d0

; Get position for the star in one year's time
; Take into account cos(dec) properly. pma is the true
; angular proper motion.  We want it in angle/year in RA
; at a certain declination.  Therefore we need to divide
; by cos(dec)

ra2 = double(ra1+pma2/cos(dec1*d2r))
dec2 = double(dec1+pmd2)

; Get next year's position in galactic coordinates
glactc,ra2,dec2,2000.0,l2,b2,1,/degree

; Difference b/w the two positions gives the proper motion
; in deg/year.  This is the coordinate proper motion and
; not the true angular proper motion.
mul = double(l2-l)
mub = double(b2-b)

; Convert to mas/year, and true angular proper motion
; by multiplying mul by cos(b).
mul = double(mul*3600.0*1000.0*cos(b*d2r))
mub = double(mub*3600.0*1000.0)

END
