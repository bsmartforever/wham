pro lbd2xyz,l,b,d,x,y,z,R0=R0,noprint=noprint

;+
;
; LBD2XYZ
;
; Convert from LON, LAT and DISTANCE to galactocentric
; cartesian coordinates.
;
; The L,B and D (distance) are all with respect to the sun
; L and B need to be in degrees and D in kpc.
; The X, Y and Z coordinates are with respect to the Galactic Center
;  Z is going up, towards b=90
;  X is towards the galactic center from our direction, L=0
;  Y is towards the L=90 direction
;
; INPUTS:
;  l    Galactic longitude in degrees
;  b    Galactic latitude in degrees
;  d    Distance from sun in kpc
;  =R0  Distance of the sun to the galactic center. 8.5 kpc is the default
;
; OUTPUTS:
;  X  The galactocentric cartesian X coordinate in kpc.  Positive X is
;       towards the Galactic center or L=0.
;  Y  The galactocentric cartesian Y coordinate in kpc.  Positive Y is
;       towards L=90.
;  Z  The galactocentric cartesian Z coordinate in kpc.  Positive Z is
;      towards the North Galactic pole.
;
; USAGE:
;  IDL>lbd2xyz,lon,lat,dis,xgc,ygc,zgc
;
; By D.Nidever   Mar 2007
;-

if n_elements(R0) eq 0 then R0=8.5

rad2deg = (180.d)/!dpi
deg2rad = !dpi/180.d

; Not enough inputs
if n_elements(l) eq 0 or n_elements(b) eq 0 or n_elements(d) eq 0 then begin
  print,'Syntax - lbd2xyz,l,b,d,x,y,z,r0=r0,noprint=noprint'
  return
endif

brad = double(b)*deg2rad
lrad = double(l)*deg2rad
dd = double(d)

x = dd*sin(0.5*!dpi-brad)*cos(lrad)-R0
y = dd*sin(0.5*!dpi-brad)*sin(lrad)
z = dd*cos(0.5*!dpi-brad)

;if not keyword_set(noprint) then begin
;  print,'X = ',strtrim(x,2),' kpc'
;  print,'Y = ',strtrim(y,2),' kpc'
;  print,'Z = ',strtrim(z,2),' kpc'
;endif

;stop

end
