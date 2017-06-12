function vgsr2vhelio,vgsr,lon,lat,vcirc=vcirc

;+
; 
;  VGSR2VHELIO.PRO
;
;  Given an input array called vhel with values of heliocentric
;  radial velocities and input arrays of the same length with
;  the Galactic coordinates of the object (gl,gb),
;  this code calculates Vlsr and Vgsr.
;
;  INPUTS:
;   vgsr     Galactocentric Standard of Rest Radial Velocity
;   lon      Galactic longitude (in degrees)
;   lat      Galactic latitude (in degrees)
;   =vcirc   MW circular velocity.  220 km/s by default.
;   
;  OUTPUTS:
;   Vhelio
;
;  By D.Nidever 2005
;-

if not keyword_set(vgsr) then begin
  print,'Syntax: vhelio = vgsr2vhelio(vgsr,l,b)
  return,-1
endif

if not keyword_set(vcirc) then vcirc = 220.0d0

;  code assumes gl & gb given in degrees.

gl=lon*(!dpi/180.0d0)
gb=lat*(!dpi/180.0d0)

cgl=cos(gl) & sgl=sin(gl)
cgb=cos(gb) & sgb=sin(gb)

;  This equation takes the solar motion w.r.t. the LSR as
;  (9,11,6) km/sec (Mihalas & Binney)
;vlsr=vhel+((9.0d0*cgb*cgl)+(11.0d0*cgb*sgl)+(6.0d0*sgb))

;  This equation takes the rotation velocity of the LSR to
;  be 220 km/sec

;vgsr=vlsr+(220.0d0*cgb*sgl)
vlsr = vgsr-(vcirc*cgb*sgl)

;   Using updated values from Dehnen & Binney 1998, MNRAS, 298, 387
;   U = 10.00 +/- 0.36 km/s
;   V = 5.25 +/- 0.62 km/s
;   W = 7.17 +/- 0.38 km/s
;   This is in a right-handed system
;vlsr=vhel+((9.0d0*cgb*cgl)+(11.0d0*cgb*sgl)+(6.0d0*sgb))
;vhel=vlsr-( (9.0d0*cgb*cgl)+(11.0d0*cgb*sgl)+(6.0d0*sgb) )
vhel=vlsr-( (10.0d0*cgb*cgl)+(5.25d0*cgb*sgl)+(7.17d0*sgb) )

;stop

return,vhel

end

