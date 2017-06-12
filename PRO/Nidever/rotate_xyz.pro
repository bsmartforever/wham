pro rotate_xyz,vec,nvec,az,ax

;  This rotates a vector given a rotation about the z-axis
; (done first) and a rotation about the x-axis.
; AZ and AX should be input as degrees
;
; If you are giving multiple vectors as a matrix the first
; dimension should have x,y,z, i.e. the dimensionality of
; the matrix should be [3,N].
;
; Created by David Nidever June 2005

if n_params() eq 0 then begin
  print,'Syntax - rotate_xyz,vec,nvec,az,ax'
  return
end

deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

raz = deg2rad * az
rax = deg2rad * ax

; Rotation about z-axis

rotz = [[ cos(-raz), sin(-raz), 0. ],$ 
       [-sin(-raz), cos(-raz), 0. ],$
       [      0.,      0., 1.]]

vec1 = rotz#vec

;stop

; Rotation about x-axis

rotx = [[      1.,       0.,      0.],$
        [      0.,  cos(-rax), sin(-rax)],$
        [      0., -sin(-rax), cos(-rax)]]

nvec = rotx#vec1

;stop

end
