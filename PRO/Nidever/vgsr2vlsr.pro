function vgsr2vlsr,v,l,b,dir,vcirc=vcirc

;  Given an input array called vhel with values of heliocentric
;  radial velocities and input arrays of the same length with
;  the Galactic coordinates of the object (gl,gb),
;  this code calculates Vlsr and Vgsr.

;  code assumes gl & gb given in degrees.

if not keyword_set(v) then begin
  print,'Syntax: vlsr = vgsr2vlsr(vgsr,l,b,1)
  print,'        vgsr = vgsr2vlsr(vlsr,l,b,-1)
  return,-1
endif

if dir ne -1 and dir ne 1 then begin
  print,'Direction must be 1 or -1'
  print,'Direction=+1   Vgsr -> Vlsr'
  print,'Direction=-1   Vlsr -> Vgsr'
  return,-1
end

if not keyword_set(vcirc) then vcirc=220.0d0

gl = l
gb = b

gl=gl*(!dpi/180.0d0)
gb=gb*(!dpi/180.0d0)

cgl=cos(gl) & sgl=sin(gl)
cgb=cos(gb) & sgb=sin(gb)

;;  This equation takes the solar motion w.r.t. the LSR as
;;  (9,11,6) km/sec (Mihalas & Binney)
;
;vlsr=vhel+((9.0d0*cgb*cgl)+(11.0d0*cgb*sgl)+(6.0d0*sgb))


;  This equation takes the rotation velocity of the LSR to
;  be 220 km/sec
;
;vgsr=vlsr+(220.0d0*cgb*sgl)

;vgsr=vlsr+(220.0d0*cgb*sgl)
;vlsr=vgsr-(220.0d0*cgb*sgl)

; input vgsr, output vlsr
if dir eq 1 then v2=v-(vcirc*cgb*sgl)

; input vlsr, output vgsr
if dir eq -1 then v2=v+(vcirc*cgb*sgl)

;stop

return,v2

end

