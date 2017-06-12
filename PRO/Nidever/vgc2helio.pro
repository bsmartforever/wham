pro vgc2helio,vx,vy,vz,x,y,z,vhel,mul,mub,vcirc=vcirc,R0=R0

;+
; This converts galactocentric velocities to heliocentric
; radial velocity and proper motions (in LON,LAT)
;
; INPUTS:
;  vx      Velocity in the galactocentric X direction (in km/s)
;  vy      Velocity in the galactocentric Y direction (in km/s)
;  vz      Velocity in the galactocentric Z direction (in km/s)
;  x       Galactocentric X direction (in kpc)
;  y       Galactocentric Y direction (in kpc)
;  z       Galactocentric Z direction (in kpc)
;  =vcirc  The circular rotation velocity of the Milky Way
;            (by default vcirc=220 km/s)
;  =R0     The distance from the sun to the galactic center
;            (by default R0=8.5 kpc)
;
; OUTPUTS:
;  vhel    Heliocentric radial velocity (in km/s)
;  mul     True proper motion in galactic longitude (in mas/year)
;  mub     True proper motion in galactic latitude (in mas/year)
;
; The coordinate system (right-handed):
;  X = Towards the galactic center from the Sun
;  Y = Towards LON=90 (galactic rotation)
;  Z = Towards the NGP
;
; NOTE:
;  1.)  A solar motion w.r.t. the LSR of (9,11,6) km/sec is used
;       (from Mihalas & Binney)
;  2.)  The Vx,Vy,Vz galactocentric velocities are not always the
;       same as the standard UVW velocities.  Often a left-handed
;       coordinate system is used (U = -Vx) and often Vcirc is
;       removed from V (V = Vy-Vcirc).  So just be sure you know
;       what coordinate system your are working with.
;
; PROGRAMS USED:
;  XYZ2LBD.PRO
;
; Written by D.Nidever  March 2006
;-

; Checking the inputs
nvx = n_elements(vx)
nvy = n_elements(vy)
nvz = n_elements(vz)
nx = n_elements(x)
ny = n_elements(y)
nz = n_elements(z)

; Not enough inputs
if nvx eq 0 or nvy eq 0 or nvz eq 0 or nx eq 0 or ny eq 0 or nz eq 0 then begin
  print,'Syntax - uvw2helio,vx,vy,vz,x,y,z,vhel,mul,mub,vcirc=vcirc'
  return
end

if n_elements(vcirc) eq 0 then vcirc = 220.
if n_elements(R0) eq 0 then R0 = 8.5


; The Coordinate System (right-handed):
;  x = toward galactic center from sun
;  y = toward l=90
;  z = toward NGP
;  phi = 0 at x, positive toward +y (against galactic rotation)
;  r = radial distance from GC

; Get Galactic coordinates
xyz2lbd,x,y,z,l,b,d,R0=R0

; RADIAL VELOCITY

;Calculating the heliocentric radial velocity
; Vrad = Vrel dotted with unit vector from sun to the star
; Vrad = V*cos(angle)

; Solar motion wrt the galactic center
;  Solar motion w.r.t. the LSR is (9,11,6) km/sec (Mihalas & Binney)
vxsun = 9.0
vysun = 11.0+vcirc
vzsun = 6.0

; Relative velocity of the star wrt the sun
vrelvec = fltarr(nx,3)
vrelvec(*,0) = vx - vxsun
vrelvec(*,1) = vy - vysun
vrelvec(*,2) = vz - vzsun

; unit vector for our line-of-sight toward the star, rhat
;  in helio cartesian coordinate, xhel=x+R0
; rhat = (x/r)*xhat + (y/r)*yhat + (z/r)*zhat
xhel = x+R0
rhel = sqrt(xhel^2.+y^2.+z^2.)
rhat = fltarr(nx,3)
rhat(*,0) = xhel/rhel 
rhat(*,1) = y/rhel
rhat(*,2) = z/rhel

; Take the dot product of the two vectors
; vvec dot rhat = vrelvec[0]*rhat[0] + vrelvec[1]*rhat[1] + vrelvec[2]*rhat[2]
vhel = vrelvec*rhat
vhel = total(vhel,2)

; Getting Vgsr velocity
;vconv,vhel,l,b,vlsr,vgsr,vcirc=vcirc


; PROPER MOTIONS

; Calculating the heliocentric transverse velocity and proper motion

; heliocentric spherical coordinates
htheta = 90-b   ; 0 at NP, 180 at SP
hphi = l

; In the +LON direction

; unit vector in the +LON direction at the star's position, lhat
; lhat = -sin(hphi)*xhat + cos(hphi)*yhat
lhat = fltarr(nx,3)
lhat(*,0) = -sin(hphi/!radeg)
lhat(*,1) = cos(hphi/!radeg)
lhat(*,2) = 0.0

; transverse velocity in +LON direction
;  relative velocity dot lhat
vtanl = vrelvec*lhat
vtanl = total(vtanl,2)

; proper motion in +LON direction
;   mu(arcsec/year) = Vtan(km/s)/(4.74*d(pc))
mul = vtanl/(4.74*rhel*1000.0)          ; in arcsec/year
mul = mul * 1000.0                      ; in mas/year

; in +LAT direction

; unit vector in the +LAT direction at the star's position, bhat
; bhat = cos(htheta)*cos(hphi)*xhat + cos(htheta)*sin(hphi)*yhat - sin(htheta)*zhat
bhat = fltarr(nx,3)
bhat(*,0) = cos(htheta/!radeg)*cos(hphi/!radeg)
bhat(*,1) = cos(htheta/!radeg)*sin(hphi/!radeg)
bhat(*,2) = -sin(htheta/!radeg)

; transverse velocity in +LAT direction
;  relative velocity dot lhat
vtanb = vrelvec*bhat
vtanb = total(vtanb,2)

; proper motion in +LAT direction
;   mu(arcsec/year) = Vtan(km/s)/(4.74*d(pc))
mub = vtanb/(4.74*rhel*1000.0)          ; in arcsec/year
mub = mub * 1000.0                      ; in mas/year

;stop

end
