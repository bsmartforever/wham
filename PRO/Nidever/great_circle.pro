pro great_circle,p1,p2,l,b,npole,equator

; this program creates a great circle
; it needs two points in L,B format or a North pole.

deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

;p1 = [20.,30.]   ; north pole
;p2 = [0.,0.]    ; gal. center

; getting cartesian unit vectors
p1_phi = double(p1(0))*deg2rad
p1_theta = double(90.-p1(1))*deg2rad
p1vec = [ sin(p1_theta)*cos(p1_phi), sin(p1_theta)*sin(p1_phi), cos(p1_theta)]

p2_phi = double(p2(0))*deg2rad
p2_theta = double(90.-p2(1))*deg2rad
p2vec = [ sin(p2_theta)*cos(p2_phi), sin(p2_theta)*sin(p2_phi), cos(p2_theta)]

; finding the north pole by the cross product
Npole_vec = crossp( p1vec, p2vec )
Npole_vec = Npole_vec/norm(Npole_vec)

; Getting the new theta and phi and converting to degrees
Npole_theta = atan( norm( Npole_vec(0:1) ), Npole_vec(2) ) * rad2deg
Npole_phi = atan( Npole_vec(1), Npole_vec(0) ) * rad2deg

; Converting Npole_theta and Npole_phi to L and B
Npole_l = Npole_phi
Npole_b = 90.-Npole_theta
if (Npole_l lt 0.) then Npole_l = Npole_l + 360.d
Npole = [Npole_l,Npole_b]

; using p1 as the equator
equator = p1

;Use rotate_lb to get l,b in the new coordinate system
larr=findgen(360)
barr=fltarr(360)
rotate_lb,larr,barr,Npole,equator,nlarr,nbarr,/noprint,/reverse

si = sort(nlarr)
l = nlarr(si)
b = nbarr(si)

;stop

end
