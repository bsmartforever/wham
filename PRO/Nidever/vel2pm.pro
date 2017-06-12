pro vel2pm,alpha,delta,dd,u,v,w,rvel,mualph,mudelt,outfile=outfile,$
           screen=screen,vcirc=vcirc

;+
;
;  VEL2PM.PRO
;
;  This program is similar to uvwvelgsr.pro except that it
;  is easier to use.  It is the inverse of pm2vel.pro
;
;  INPUTS:
;    alpha    right ascension in decimal degrees (hours*15)  (B1950.0)
;    delta    declination in decimal degrees     (B1950.0)
;    dd     distance in kiloparsecs
;    u     galactocentric U velocity (dXdt), towards galactic center
;    v     galactocentric V velocity (dYdt), towards galactic rotation
;    w     galactocentric W velocity (dZdt), perpendicular to the
;               galactic plane, towards NGP.
;
;  OUTPUTS:
;    rvel     heliocentric radial velocity in km/s
;    mualph   mu_alpha*cos(delta) proper motion in milliarcsec/year
;    mudelt   mu_delta proper motion in milliarcsec/year
;
;  USAGE:
;   IDL>vel2pm,alpha,delta,dd,u,v,w,rvel,mualpha,mudelta
;
;  Modification of uvwvel.pro by D.Nidever  March 2005
;-

if n_elements(alpha) eq 0 or n_elements(delta) eq 0 or n_elements(dd) eq 0 or $
   n_elements(u) eq 0 or n_elements(v) eq 0 or n_elements(w) eq 0 then begin
  print,'Syntax - vel2pm,alpha,delta,dd,u,v,w,rvel,mualpha,mudelta'
  return
endif

n = n_elements(alpha)
if not keyword_set(vcirc) then vcirc=220.

; keeping the originals
old_u = u
old_v = v
old_w = w
old_alpha = alpha
old_delta = delta
old_dd = dd
old_vcirc = vcirc

; make sure they're all floating point
u = double(u)
v = double(v)
w = double(w)
alpha = double(alpha)
delta = double(delta)
;rvel = double(rvel)
;mualph = double(mualph)
;mudelt = double(mudelt)
dd = double(dd)
vcirc = double(vcirc)

;
;
;  Convert a,d to radians, and mu to arcsec/yr
;
;
alpha(*)=(alpha(*))*(!pi/180.0d0)
delta(*)=(delta(*))*(!pi/180.0d0)
;mualph=mualph/1.00d3
;mudelt=mudelt/1.00d3

;
;   We will convert the above into U,V,W velocities using the prescription given
;   in Johnson & Soderblom AJ 93, 864.
;
;   First, we need to set up some matrices for use in the conversion.
;
T=dblarr(3,3)
T(0,*)=[0.06699,0.49273,-0.86760]
T(1,*)=[0.87276,-0.45035,-0.18837]
T(2,*)=[0.48354,0.74458,0.46020]

;
;   Now we'll loop through each entry, determining the A matrix for each
;   object and then determining its velocity.
;
uvwgcent=dblarr(6,n)
factor=(3.0857d16)*tan(!pi/6.48d5)*(1./3.15569252d7)

; loading the u,v,w velocities into uvwgcent
uvwgcent(0,*) = u
uvwgcent(2,*) = v
uvwgcent(4,*) = w

rvel = dblarr(n)
mualph = dblarr(n)
mudelt = dblarr(n)

for Q=0.,n_elements(alpha)-1 do begin


;  This is only to LSR!!!!!  You must also add the rotational velocity
;  to get to GSR!!!  But only in Y-direction (V component).
; Converting from GSR to LSR and from right-handed to left-handed system
   uvwgcent(0,Q)=-uvwgcent(0,Q)        ;U   right-handed U
   uvwgcent(2,Q)=uvwgcent(2,Q)-vcirc     ;V   220 km/s rotation velocity


;   The U,V,W above are heliocentric.  To convert them to galactocentric,
;   simply add U_sun,V_sun,W_sun (using Mihalas & Routly 1968 values)
;   Using updated values from Dehnen & Binney 1998, MNRAS, 298, 387
;   U = 10.00 +/- 0.36 km/s
;   V = 5.25 +/- 0.62 km/s
;   W = 7.17 +/- 0.38 km/s
;   This is in a right-handed system
;
;   uvwgcent(0,Q)=uvw(0)+(-9.0)    ;U
;   uvwgcent(2,Q)=uvw(1)+(12.0)    ;V
;   uvwgcent(4,Q)=uvw(2)+(6.0)     ;W
; Converting from LSR to heliocentric
   uvw=dblarr(3)
   uvw(0) = uvwgcent(0,Q)-(-10.00)    ;U left-handed right now
   uvw(1) = uvwgcent(2,Q)-(5.25)    ;V
   uvw(2) = uvwgcent(4,Q)-(7.17)     ;W



; A and B matrices
   A=dblarr(3,3)
   A(0,*)=[(cos(alpha(Q))*cos(delta(Q))),(sin(alpha(Q))*cos(delta(Q))),$
           (sin(delta(Q)))]
   A(1,*)=[(-(sin(alpha(Q)))),(cos(alpha(Q))),0.0]
   A(2,*)=[((-(cos(alpha(Q))))*(sin(delta(Q)))),((-(sin(alpha(Q))))*(sin(delta(Q)))),$
           (cos(delta(Q)))]
;
   B=T##A                ; same as B=A#T
   Binv = invert(B)


;  The U,V,W components are now determined simply by multiplying the 3 velocity 
;  components (radial,alphaprop,deltaprop) * (T*A)  (see eqn 1 in Johnson & Soderblom)
;
   velocities=dblarr(3)
;   velocities=[rvel(Q),mualphvel,mudeltvel]
;   uvw=dblarr(3)
;   uvw=B##velocities   ; same as uvw=velocities#B
   velocities = uvw#Binv

;  We need to convert the proper motions to velocities in km/sec using the
;  provided distance.  So we first have to figure out how many parsecs 
;  correspond to 1 arcsec at the distance of the object (=dconvert).  Next
;  we need to multiply the proper motions by this distance conversion factor
;  and by the number of seconds in 1 tropical year.

;
   dconvert=dd(Q)*factor
;   mualphvel=mualph(Q)*dconvert
;   mudeltvel=mudelt(Q)*dconvert
mualphvel=velocities(1)
mudeltvel=velocities(2)
rvel(Q)=velocities(0)
mualph(Q)=mualphvel/dconvert*1d3
mudelt(Q)=mudeltvel/dconvert*1d3

endfor

;
;  Ok, let's print out the sucker.
;

; print ot screen
if keyword_set(screen) then begin
  for P=0,n-1 do begin
    print,format='(6F9.2)',rvel(P),mualph(P),mudelt(P)
  endfor
endif


; print to a file
if keyword_set(outfile) then begin
  get_lun,unit
  filename=outfile
  openw,unit,filename
     for P=0,n-1 do begin
         printf,unit,format='(6F9.2)',rvel(P),mualph(P),mudelt(P)
     endfor
  close,unit
  free_lun,unit
endif; outfile

; Final velocities
u = uvwgcent(0,*)
v = uvwgcent(2,*)
w = uvwgcent(4,*)

; keeping the originals
u = old_u
v = old_v
w = old_w
alpha = old_alpha
delta = old_delta
dd = old_dd
vcirc = old_vcirc

;stop

end
