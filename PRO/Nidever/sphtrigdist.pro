function sphtrigdist,lon1,lat1,lon2,lat2

; This program calculates the angle between two points on a sphere.
; Also see angledistvec.pro - uses vectors

; From this website;
; http://www2.sjsu.edu/faculty/watkins/sphere.htm

; input in degrees
deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

l1 = double(lon1)*deg2rad
l2 = double(lon2)*deg2rad
b1 = double(lat1)*deg2rad
b2 = double(lat2)*deg2rad

; not correct!!
;cosa = sin(l1)*sin(l2)+cos(l2)*cos(l1)*cos(b2-b1)
;a = acos(cosa)*rad2deg

;another possibility
cosa=sin(b1)*sin(b2)+cos(b1)*cos(b2)*cos(l1-l2)
a = acos(cosa)*rad2deg

;sinhalfa = sqrt(sin((l2-l1)/2.)^2. + cos(l2)*cos(l1)*sin((b2-b1)/2.)^2.)
;a2 = asin(sinhalfa)*2.*rad2deg

;stop

return,a

end
