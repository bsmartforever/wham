function angledistvec,lon1,lat1,lon2,lat2

; This program calculates the angle between two points on a sphere.
; Also see sphtrigdist.pro - uses spherical trigonometry

; From this website;
; http://www2.sjsu.edu/faculty/watkins/sphere.htm

; input in degrees
deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

l1 = double(lon1)*deg2rad
l2 = double(lon2)*deg2rad
b1 = double(lat1)*deg2rad
b2 = double(lat2)*deg2rad

; using vectors
phi1 = l1
phi2 = l2
theta1 = !dpi*0.5-b1
theta2 = !dpi*0.5-b2
vec1 = [sin(theta1)*cos(phi1),sin(theta1)*sin(phi1),cos(theta1)]
vec2 = [sin(theta2)*cos(phi2),sin(theta2)*sin(phi2),cos(theta2)]
;v1dotv2 = total(vec1*vec2)
cosangle = total(vec1*vec2)/(norm(vec1)*norm(vec2))
angle = acos(cosangle)*rad2deg

;stop

return,angle

end
