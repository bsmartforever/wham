pro galaxy_model,x,y,z,vx,vy,vz,l,b,vhel,vlsr,vgsr,mul,mub,pa,flat=flat,$
                 vcirc=vcirc,r0=r0,rscale=rscale,rmin=rmin,zscale=zscale,$
                 nstars=nstars,noplot=noplot,stp=stp,rhelcut=rhelcut,4
                 vdisk=vdisk

;+
; Model of the proper motions and rvs of the galaxy.
; Mostly copied from ggss/ggss_model.pro
;
; INPUT:
;  =vdisk    The circular velocity of the disk (default vdisk=vcirc)
;  =vcirc    The circular velocity of the sun (default vcirc=220 km/s)
;  =R0       The distance from the sun to the GC (default R0=8.5 kpc)
;  =rscale   The upper radial cutoff of the disk (default rscale=12.5 kpc)
;  =zscale   The z-height cutoff of the disk (default zscale=5.0 kpc)
;  =rmin     The lower radial cutoff of the disk(default rmin=1.5 kpc)
;  =rhelcut  Only keep stars within this distance of the sun.
;  =nstars   The number of stars in the simulation (default nstars=100,000)
;  /flat     Simulate a "flat" galaxy, i.e. no thickness, all stars at b=0.
;  /noplot   Don't plot anything
;  /stp      Stop at the end of the program
;
; OUTPUT:
;  x       Galactocentric X direction (in kpc)
;  y       Galactocentric Y direction (in kpc)
;  z       Galactocentric Z direction (in kpc)
;  vx      Velocity in the galactocentric X direction (in km/s)
;  vy      Velocity in the galactocentric Y direction (in km/s)
;  vz      Velocity in the galactocentric Z direction (in km/s)
;  l       Galactic longitude
;  b       Galactic latitude
;  vhel    Heliocentic radial velocity (in km/s)
;  vlsr    Radial velocity wrt the local standard of rest (LSR) (in km/s)
;  vgsr    Radial velocity wrt the galactic standard of rest (GSR) (in km/s)
;  mul     True proper motion in galactic longitude (in mas/yr)
;  mub     True proper motion in galactic latitude (in mas/yr)
;
; PROGRAMS USED:
;  XYZ2LBD.PRO
;  VCONV.PRO
;
; Written by D.Nidever  March 2006
;-

; Setting up basic parameters
if n_elements(rscale) eq 0 then rscale = 12.5  ; in kpc
if n_elements(zscale) eq 0 then zscale = 5.0   ; in kpc
if n_elements(vcirc) eq 0 then Vcirc = 220.0   ; in km/s
if n_elements(r0) eq 0 then R0 = 8.5           ; in kpc
if n_elements(rmin) eq 0 then rmin = 1.5       ; in kpc
if n_elements(vdisk) eq 0 then vdisk = vcirc   ; in km/s

; The Coordinate System (right-handed):
;  x = toward galactic center from sun
;  y = toward l=90
;  z = toward NGP
;  phi = 0 at x, positive toward +y (against galactic rotation)
;  r = radial distance from GC

if not keyword_set(nstars) then nstars = 100000.

; Getting the random positions
x = (randomu(seed,nstars)-0.5)*2.*rscale
y = (randomu(seed,nstars)-0.5)*2.*rscale
; Flat galaxy
if keyword_set(flat) then begin
  z = fltarr(nstars)
; Galaxy extended in the Z direction
endif else begin
  z = (randomu(seed,nstars)-0.5)*2.*zscale
endelse

; Only want stars within a radius RSCALE and greater than RMIN*RSCALE 
gd = where( sqrt(x^2.+y^2.) le rscale and sqrt(x^2.+y^2.) gt rmin,ngd)
x = x(gd)
y = y(gd)
z = z(gd)

; RHEL CUT
if n_elements(rhelcut) gt 0 then begin
  xhel = x+R0
  rhel = sqrt(xhel^2.+y^2.+z^2.)
  gd = where(rhel le rhelcut,ngd)
 
  if ngd eq 0 then begin
    print,'No stars within '+strtrim(rhelcut,2)+' kpc of the sun'
    return
  endif 

  x = x(gd)
  y = y(gd)
  z = z(gd)
end

; Cylindrical and spherical coordinates
rho = sqrt(x^2.+y^2.)    ; distance from z axis
gphi = atan(y,x)         ; the galactic azimuthal coordinate (0-2*pi), in radians
gtheta = atan(rho,z)     ; the galactic polar coordinate (0-pi), in radians
r = sqrt(x^2.+y^2.+z^2.) ; radial distance, in kpc

; Get Galactic coordinates
xyz2lbd,x,y,z,l,b,d,R0=R0

; Cartesian velocities
;  Moving in the -gphi direction
;  phihat = -sin(phi)*xhat + cos(phi)*yhat
vx = (-Vdisk)*(-sin(gphi))
vy = (-Vdisk)*cos(gphi)
vz = x*0.
vtot = sqrt(vx^2.+vy^2.+vz^2.)     ; total velocity, should be vcirc


; *** NOW WE "OBSERVE" THE STARS ***

; RADIAL VELOCITY

;Calculating the heliocentric radial velocity
; Vrad = Vrel dotted with unit vector from sun to the star
; Vrad = V*cos(angle)

;; Relative velocity of the star wrt the sun
; THIS ASSUMES THAT THE SUN IS IN COMPLETE CIRCULAR MOTION
vrelvec = fltarr(ngd,3)
vrelvec(*,0) = vx
vrelvec(*,1) = vy-vcirc    ; the sun is moving in +y direction
vrelvec(*,2) = vz

; unit vector for our line-of-sight toward the star, rhat
;  in helio cartesian coordinate, xhel=x+R0
; rhat = (x/r)*xhat + (y/r)*yhat + (z/r)*zhat
xhel = x+R0
rhel = sqrt(xhel^2.+y^2.+z^2.)
rhat = fltarr(ngd,3)
rhat(*,0) = xhel/rhel 
rhat(*,1) = y/rhel
rhat(*,2) = z/rhel

; ****THIS IS VLSR NOT VHELIO*****

; Take the dot product of the two vectors
; vvec dot rhat = vrelvec[0]*rhat[0] + vrelvec[1]*rhat[1] + vrelvec[2]*rhat[2]
vlsr = vrelvec*rhat
vlsr = total(vlsr,2)

; Getting Vgsr and Vhelio velocity
vgsr = vgsr2vlsr(vlsr,l,b,-1)
vhel = vgsr2vhelio(vgsr,l,b,vcirc=vcirc)
;vconv,vhel,l,b,vlsr,vgsr,vcirc=vcirc


; PROPER MOTIONS

; Calculating the heliocentric transverse velocity and proper motion

; heliocentric spherical coordinates
htheta = 90-b   ; 0 at NP, 180 at SP
hphi = l

; In the +LON direction

; unit vector in the +LON direction at the star's position, lhat
; lhat = -sin(hphi)*xhat + cos(hphi)*yhat
lhat = fltarr(ngd,3)
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
bhat = fltarr(ngd,3)
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

; The position angle of the proper motion, E of N
pa = atan(mul,mub)*!radeg
bd = where(pa lt 0.0,nbd)
if nbd gt 0 then pa(bd) = pa(bd)+360.0


; *** Plotting ***
if not keyword_set(noplot) then begin
  erase
  charsize = 1.2

  ; Vhelio
  !p.multi=[4,2,2]
  plot,l,vhel,ps=3,xr=[360,0],yr=[-300,300],xs=1,ys=1,xtit='LON',ytit='V!dHELIO!n',tit='Heliocentric Radial Velocity',$
       charsize=charsize
  ;in = where(r lt r0)
  ;oplot,l(in),vhel(in),ps=3,co=250

  ; Vgsr
  !p.multi=[3,2,2]
  plot,l,vgsr,ps=3,xr=[360,0],yr=[-300,300],xs=1,ys=1,xtit='LON',ytit='V!dGSR!n',tit='Galactocentric Radial Velocity',$
       charsize=charsize
  ;oplot,l(in),vgsr(in),ps=3,co=250

  ; mu_LON
  !p.multi=[2,2,2]
  plot,l,mul,ps=3,xr=[360,0],yr=[-10,10],xs=1,ys=1,xtit='LON',ytit='!7l!X!dLON!n',tit='Proper Motion in LON',$
       charsize=charsize
  ;oplot,l(in),mul(in),ps=3,co=250

  ; mu_LAT
  !p.multi=[1,2,2]
  plot,l,mub,ps=3,xr=[360,0],yr=[-10,10],xs=1,ys=1,xtit='LON',ytit='!7l!X!dLAT!n',tit='Proper Motion in LAT',$
       charsize=charsize
  ;oplot,l(in),mub(in),ps=3,co=250

  !p.multi = 0
endif ; not /noplot

if keyword_set(stp) then stop

end
