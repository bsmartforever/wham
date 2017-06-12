pro xyz2lbd,x,y,z,larr,barr,darr,R0=R0,noprint=noprint

;+
;
; XYZ2LBD
;
; The L,B and D (distance) are all with respect to the sun
; The X, Y and Z coordinates are with respect to the Galactic Center
;  Z is going up, towards b=90
;  X is towards the galactic center from our direction, L=0
;  Y is towards the L=90 direction
;
; INPUTS:
;  X    Galactocentric cartesian X coordinate in kpc.  Positive X is
;         towards the Galactic center or L=0.
;  Y    Galactocentric cartesian Y coordinate in kpc.  Positive Y is
;         towards L=90.
;  Z    Galactocentric cartesian Z coordinate in kpc.  Positive Z is
;         towards the North Galactic pole.
;  =R0  Distance of the sun to the galactic center. 8.5 kpc is the default
;
; OUTPUTS:
;  l    Galactic longitude
;  b    Galactic latitude
;  d    Distance from sun in kpc
;
; USAGE:
;  IDL>xyz2lbd,x,y,z,l,b,d
;
; By D.Nidever  Dec 2004
;-

rad2deg = (180.d)/!dpi
deg2rad = !dpi/180.d

if n_elements(R0) eq 0 then R0=8.5

; Not enough inputs
if n_elements(x) eq 0 or n_elements(y) eq 0 or n_elements(z) eq 0 then begin
  print,'Syntax - xyz2lbd,x,y,z,l,b,d,R0=R0,noprint=noprint'
  return
endif

n = n_elements(x)
larr = dblarr(n)
barr = dblarr(n)
darr = dblarr(n)


; Loop through the points
for i=0L,n-1 do begin

  xx = double(x[i])
  yy = double(y[i])
  zz = double(z[i])
  rho = sqrt( (xx+R0)^2 + yy^2)  ; distance from sun in X/Y plane

  ;d = sqrt((xx)^2.+(yy)^2.+(zz)^2.)
  ;brad = 0.5*!dpi - acos(zz/d)      ; do I need to use atan??
  ;tanl = yy/(xx+R0)

  lrad = atan(yy,xx+R0)

  brad = 0.5*!dpi - atan(rho,zz)
  ; This is garbage!
  brad = 0.5*!dpi - atan(yy,zz*sin(lrad))
  if brad gt 0.5*!dpi then brad=brad-!dpi
  if brad lt -0.5*!dpi then brad=brad+!dpi

  ; This doesn't work if z=0
  ;if cos(0.5*!dpi-brad) ne 0.0 then d = zz/cos(0.5*!dpi-brad)
  ;if cos(0.5*!dpi-brad) eq 0.0 then d = abs(zz)
  d = sqrt( (xx+R0)^2 + yy^2 + zz^2 )

  b = brad*rad2deg
  l = lrad*rad2deg

  if l lt 0. then l=l+360.

  ;theta=atan(y,x)
  ;
  ;x=d*sin(90.-b)*cos(l)-R0
  ;y=d*sin(90.-b)*sin(l)
  ;z=d*cos(90.-b)


  larr[i] = l
  barr[i] = b
  darr[i] = d

  ;stop

end

;stop

end
