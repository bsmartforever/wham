pro rotate_lb,LON,LAT,Npole,equator,NLON,NLAT,noprint=noprint,$
              ROT=ROT, R_inv=R_inv,nvec=nvec,reverse=reverse,$
              contin=contin

;+
; This program rotates Latitude and Longitude to a new coordinate
; system given L,B coordinates of the new Z and X axes in the
; old coordinate system.
;
; If the REVERSE keyword is set then the points are actually
; rotated.
;
;  INPUTS
;   lon       Original longitude (scalar or array)
;   lat       Original latitude (scalar or array)
;   npole     The north pole of the new coordinate system
;             in the old coordinate system [L,B].
;   equator   The zero-point of the equator of the new coordinate
;             system in the old coordinate system [L,B].
;   /noprint  Don't print anything.
;   /reverse  Actually rotate the points, not the coordinates.
;   /contin   Make the points continuous.
;
;  OUTPUTS
;   nlon      The longitude(s) in the new coordinate system
;   nlat      The latitude(s) in the new coordinate system
;   rot       The rotation matrix
;   r_inv     The inverse rotation matrix.  The actual operation
;             to get the new coordinates is: nvec = R_inv # vec
;             Where vec is the cartesian vector on a unit circle
;             for the old coordinates and nvec for the new ones.
;   nvec      The cartesian vector on a unit circle for the new
;             coordinates.  Only for the last scalar.
;
; Written by David Nidever Dec 2004
;-

; Bad Input Values
if (n_params() eq 0) then begin
  print,'Syntax = rotate_lb,LON,LAT,Npole,equator,NLON,NLAT,noprint=noprint,'
  print,'                   ROT=ROT,R_inv=R_inv,nvec=nvec,reverse=reverse'
  return
endif

nvec = 0.

;L = 0.
;B = 0.
;Npole = [0.,90.]
;equator = [45.,0.]

;L = 45.
;B = 0.
;Npole = [45.,45.]
;equator = [45.,-45.]

; INPUT:
;  LON - galactic longitude
;  LAT - galactic latitude
;  Npole - 2-element array for (L,B) of North pole of new 
;          coordinate system
;  equator - 2-element array for (L,B) of equator zero-point
;            (x-axis) of new coordinate system

; have three euler angles: A, B, G

; 1st Rotation: Rotate A about original Z-axis in +phi direction
; 2nd Rotation: Rotate B about the new Y-axis (Y2) in +theta direction
; 3rd Rotation: Rotate G about the new Z-axis (Z2=Z3) in +phi2
;      direction (in the X2-Y2 / X3-Y3 plane).
;

; I should convert L and B coordinates into a vector in a right hand
; reference frame.  That frame is: Z is at B=+90, X is at B=0,L=0
; (galactic center) and Y is at B=0,L=+90. (where L is measured
; counterclockwise looking down onto the z axis).

; Given L and B we get x1,y1,z1 (1 meaning in the original coordinate
; system).  We can convert L and B into the usual PHI and THETA.
; Where PHI is identical to L and THETA=0 at the +Z axis and 180 at
; the -Z axis:
;    PHI = L,    THETA = 90-B
; Then our conversion from PHI and THETA to x1, y1 and z1 is:
;    x1 = sin(THETA)*cos(PHI)
;    y1 = sin(THETA)*sin(PHI)
;    z1 = cos(THETA)
; Where r=1 because we don't have a distance, we only care about the
; angles.

deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

; Converting new coordinate system L and B to PHI and THETA and
; vectors
Npole_PHI = double(Npole(0))*deg2rad
Npole_THETA = double(90.-Npole(1))*deg2rad
equator_PHI = double(equator(0))*deg2rad
equator_THETA = double(90.-equator(1))*deg2rad

; Getting vectors for the new axes:
; Npole - New Z-axis
; equator - New X-axis
; Y2 - New Y-axis
Npole_vec = dblarr(3)
Npole_vec = [ sin(Npole_THETA)*cos(Npole_PHI), sin(Npole_THETA)*sin(Npole_PHI), cos(Npole_THETA)]
equator_vec = dblarr(3)
equator_vec = [ sin(equator_THETA)*cos(equator_PHI), sin(equator_THETA)*sin(equator_PHI),$
                   cos(equator_THETA) ]
Y2_vec = crossp( Npole_vec, equator_vec )   ; cross product
Y2_vec = Y2_vec / norm(Y2_vec)   ;make sure it's a unit vector

; From: http://casgm3.anorg.chemie.uni-tuebingen.de/klaus/nmr/conventions/euler/euler.html
; angle B is the angle between the two poles (i.e. the z axes).
; angle A is the angle between the X axis of the reference (original)
;   coordinate system and the projection of z onto the X,Y plane.
; angle G is the angle between the y axis and the line of nodes.
;   I can find G by first getting the line of nodes (or the vector
;   that points in that direction).  I just need to rotate the Y unit
;   vector by A around the Z-axis.  Then I just need to find the angle
;   between that and the new Y-axis.  Can use dot product to do that.


A = Npole_PHI      
B = Npole_THETA

; Getting the line of nodes, Rotate Y by A about Z axis
R1 = [[cos(A),sin(A),0.],[-sin(A),cos(A),0.],[0.,0.,1]]
line_nodes = R1 # [0.,1.,0.]

; Getting the final angle which is the angle between the line of nodes
; and the new Y axis (Y3).
COSG = total( line_nodes * Y2_vec ) / ( norm(line_nodes) * norm(Y2_vec) )
if COSG gt 1.0 then COSG = 1.d - (COSG-1.0d)
if COSG lt -1.0 then COSG = -1.d - (COSG+1.0d)
G = acos(COSG)
; G is only the absolute angle b/w line_nodes and Y2

crs = crossp(line_nodes, Y2_vec)
;this will point either in the + or - Npole direction.

;getting the right G
updown = total( Npole_vec * crs )
G = signs(updown) * G
if (G lt 0.) then G = G + !dpi*2.d

; Here are the other two rotation matrices
R2 = [[cos(B),0.,-sin(B)],[0.,1.,0.],[sin(B),0.,cos(B)]]
R3 = [[cos(G),sin(G),0.],[-sin(G),cos(G),0.],[0.,0.,1.]]

; creating the transformation matrix using the Euler angles
; A, B and G need to be in radians
; R = R1 # R2 # R3
cA = double( cos(A) )
cB = double( cos(B) )
cG = double( cos(G) )
sA = double( sin(A) )
sB = double( sin(B) )
sG = double( sin(G) )
ROT = dblarr(3,3)
ROT = [ [  cA*cB*cG-sA*sG,  sA*cB*cG+cA*sG, -sB*cG  ],$
      [ -cA*cB*sG-sA*cG, -sA*cB*sG+cA*cG,  sB*sG  ],$
      [           cA*sB,           sA*sB,     cB  ] ]

; Getting the inverted rotation matrix
R1_rev = [[cos(-A),sin(-A),0.],[-sin(-A),cos(-A),0.],[0.,0.,1.]]
R2_rev = [[cos(-B),0.,-sin(-B)],[0.,1.,0.],[sin(-B),0.,cos(-B)]]
R3_rev = [[cos(-G),sin(-G),0.],[-sin(-G),cos(-G),0.],[0.,0.,1.]]
R_inv = R3_rev # R2_rev # R1_rev

n = n_elements(LON)
nlon = dblarr(n)
nlat = dblarr(n)

for i=0.,n-1 do begin

  Li = LON(i)
  Bi = LAT(i)

  ;converting L and B to PHI and THETA, and converting to radians
  PHI = double(Li)*deg2rad
  THETA = double(90.-Bi)*deg2rad

  ;converting PHI and THETA to x1, y1, and z1 in the original
  ;coord. system.
  vec = dblarr(3)
  vec = [ sin(THETA)*cos(PHI), sin(THETA)*sin(PHI), cos(THETA)]

  ; Getting the new, transformed vector
  if not keyword_set(reverse) then nvec = R_inv # vec
  if keyword_set(reverse) then nvec = ROT # vec

  ; Getting the new theta and phi and converting to degrees
  NTHETA = atan( norm( nvec(0:1) ), nvec(2) ) * rad2deg
  NPHI = atan( nvec(1), nvec(0) ) * rad2deg

  ; Converting NTHETA and NPHI to new L and B
  NL = NPHI
  NB = 90.-NTHETA
  if not keyword_set(contin) then if (NL lt 0.) then NL = NL + 360.d
 ; if (NB lt 0.) then NB = NB + 360.d

  if not keyword_set(noprint) then $
  print,'(',strtrim(float(Li),2),',',strtrim(float(Bi),2),') -> (',strtrim(float(nl),2),',',strtrim(float(nb),2),')'
 ; print,'New L = ',strtrim(nl,2)
 ; print,'New B = ',strtrim(nb,2)

  nlon(i) = nl
  nlat(i) = nb

  ;stop

end

;stop

end
