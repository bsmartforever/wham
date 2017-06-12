pro pm2vel,alpha,delta,rvel,mualph,mudelt,dd,u,v,w,outfile=outfile,$
           screen=screen,vcirc=vcirc
;+
; 
;  PM2VEL.PRO
;
;  This program is similar to uvwvelgsr.pro except that it
;  is easier to use.
;
;  INPUTS:
;    alpha    right ascension in decimal degrees (hours*15)  (B1950.0)
;    delta    declination in decimal degrees     (B1950.0)
;    rvel     heliocentric radial velocity in km/s
;    mualph   mu_alpha*cos(delta) proper motion in milliarcsec/year
;    mudelt   mu_delta proper motion in milliarcsec/year
;    dd       distance in kiloparsecs (from sun)
;
;  OUTPUTS:
;    u     galactocentric U velocity (dXdt), towards galactic center
;    v     galactocentric V velocity (dYdt), towards galactic rotation
;    w     galactocentric W velocity (dZdt), perpendicular to the
;               galactic plane, towards NGP.
; USAGE:
;  IDL>pm2vel,alpha,delta,rvel,mualph,mudelt,dd,u,v,w
;
;  Modification of uvwvel.pro by D.Nidever March 2005
;-

; Not enough inputs
if n_elements(alpha) eq 0 or n_elements(delta) eq 0 or n_elements(rvel) eq 0 or $
   n_elements(mualph) eq 0 or n_elements(mudelt) eq 0 or n_elements(dd) eq 0 then begin
  print,'Syntax - pm2vel,alpha,delta,rvel,mualph,mudelt,dd,u,v,w'
  return
endif

n = n_elements(alpha)
if not keyword_set(vcirc) then vcirc=220.

; keeping the originals
old_alpha = alpha
old_delta = delta
old_rvel = rvel
old_mualph = mualph
old_mudelt = mudelt
old_dd = dd
old_vcirc = vcirc

; make sure they're all floating point
alpha = double(alpha)
delta = double(delta)
rvel = double(rvel)
mualph = double(mualph)
mudelt = double(mudelt)
dd = double(dd)
vcirc = double(vcirc)


;
;
;  Convert a,d to radians, and mu to arcsec/yr
;
;
alpha(*)=(alpha(*))*(!pi/180.0d0)
delta(*)=(delta(*))*(!pi/180.0d0)
mualph=mualph/1.00d3
mudelt=mudelt/1.00d3

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

for Q=0,n_elements(alpha)-1 do begin
   A=dblarr(3,3)
   A(0,*)=[(cos(alpha(Q))*cos(delta(Q))),(sin(alpha(Q))*cos(delta(Q))),$
           (sin(delta(Q)))]
   A(1,*)=[(-(sin(alpha(Q)))),(cos(alpha(Q))),0.0]
   A(2,*)=[((-(cos(alpha(Q))))*(sin(delta(Q)))),((-(sin(alpha(Q))))*(sin(delta(Q)))),$
           (cos(delta(Q)))]
;
   B=T##A     ; same as B=A#T

;
;
;  We need to convert the proper motions to velocities in km/sec using the
;  provided distance.  So we first have to figure out how many parsecs 
;  correspond to 1 arcsec at the distance of the object (=dconvert).  Next
;  we need to multiply the proper motions by this distance conversion factor
;  and by the number of seconds in 1 tropical year.

;
   dconvert=dd(Q)*factor
   mualphvel=mualph(Q)*dconvert
   mudeltvel=mudelt(Q)*dconvert
;
;  The U,V,W components are now determined simply by multiplying the 3 velocity 
;  components (radial,alphaprop,deltaprop) * (T*A)  (see eqn 1 in Johnson & Soderblom)
;
   velocities=dblarr(3)
   velocities=[rvel(Q),mualphvel,mudeltvel]
   uvw=dblarr(3)
   uvw=B##velocities

;  So far this is in the left-handed UVW coordinate system
;  Must change the sign of U: U->-U
;   uvw(0) = -uvw(0)
;  DO IT BELOW!!!


;
;   The U,V,W above are heliocentric.  To convert them to galactocentric,
;   simply add U_sun,V_sun,W_sun (using Mihalas & Routly 1968 values)
;   Using updated values from Dehnen & Binney 1998, MNRAS, 298, 387
;   U = 10.00 +/- 0.36 km/s
;   V = 5.25 +/- 0.62 km/s
;   W = 7.17 +/- 0.38 km/s
;   This is in a right-handed system
;   
;  uvwgcent(0,Q)=uvw(0)+(-10.4)
;  uvwgcent(2,Q)=uvw(1)+(14.8)
;  uvwgcent(4,Q)=uvw(2)+(7.3)
; Converting from heliocentric to LSR
   uvwgcent(0,Q)=uvw(0)+(-10.0)    ;U left-handed right now
   uvwgcent(2,Q)=uvw(1)+(5.25)     ;V
   uvwgcent(4,Q)=uvw(2)+(7.17)     ;W
;
;

;  This is only to LSR!!!!!  You must also add the rotational velocity
;  to get to GSR!!!  But only in Y-direction (V component).
   uvwgcent(0,Q)=-uvwgcent(0,Q)        ;U   right-handed U
   uvwgcent(2,Q)=uvwgcent(2,Q)+vcirc     ;V   220 km/s rotation velocity
endfor

;
;  Ok, let's print out the sucker.
;

; print ot screen
if keyword_set(screen) then begin
  for P=0,n-1 do begin
    print,format='(6F9.2)',uvwgcent(0,P),uvwgcent(2,P),uvwgcent(4,P)
  endfor
endif


; print to a file
if keyword_set(outfile) then begin
  get_lun,unit
  filename=outfile
  openw,unit,filename
     for P=0,n-1 do begin
         printf,unit,format='(6F9.2)',uvwgcent(0,P),uvwgcent(2,P),uvwgcent(4,P)
     endfor
  close,unit
  free_lun,unit
endif; outfile

; Final velocities
u = reform(uvwgcent[0,*])
v = reform(uvwgcent[2,*])
w = reform(uvwgcent[4,*])

; putting back the originals
alpha = old_alpha
delta = old_delta
rvel = old_rvel
mualph = old_mualph
mudelt = old_mudelt
dd = old_dd
vcirc = old_vcirc

;stop

end
