pro makegal,input,arr,nbody=nbody,noprint=noprint,stp=stp


;+
; This program runs an N-test-body simulation of a
; galaxy with a plummer distribution of stars that
; orbits the Milky Way.
;
; INPUTS:
;  input    Input parameters [mass,soft,x,y,z,vx,vy,vz] of the galaxy CM
;  =nbody   Number of bodies to use in N-test-body simulation
;  /noprint Don't print out the parameters
;  /stp     Stop at the end of the program
;
; OUTPUTS:
;  arr      The array of parameters for the galaxy.
;
; CALLS:
;  plummer   To get the distribution of the N-bodies
;
; By David Nidever   March 2006
;-

; INITIAL CONDITIONS
;input = [mass,soft,xstart,ystart,zstart,vxstart,vystart,vzstart]
mass = input(0)
soft = input(1)
x0 = input(2)
y0 = input(3)
z0 = input(4)
vx0 = input(5)
vy0 = input(6)
vz0 = input(7)


; GETTING THE PLUMMER DISTRIBUTION OF STARS
;mass = 1e7
;rscale = 0.9*(mass/1d9)^(1./3.)

print,'USING ',strtrim(long(nbody),2),' STARS'
print,'Getting Plummer distribution of stars'
PLUMMER,arr,nbody,mass,soft,/soft    


; VELOCITIES FROM PLUMMER ARE IN KPC/GYR
; CONVERT TO KM/S FOR GALSAT INPUT

; CONVERTING VELOCITIES (kpc/Gyr -> km/s)
; 1 kpc = 3.0857E16 km, 1 Gyr = 3.1557E16 sec
; 1 kpc/Gyr = 3.0857/3.1557 = 0.977818
kms2kpcGyr = 3.0857d/3.1557d
kpcGyr2kms = 3.1557d/3.0857d
npar = 8
arr(*,npar-3) = arr(*,npar-3)*kpcGyr2kms
arr(*,npar-2) = arr(*,npar-2)*kpcGyr2kms
arr(*,npar-1) = arr(*,npar-1)*kpcGyr2kms


; ADDING INITIAL CONDITIONS
; [mass,soft,x,y,z,Vx,Vy,Vz]
arr(*,0) = 0.   ; mass=0, test particles
arr(*,1) = 0.   ; no softening parameter
arr(*,2) = arr(*,2) + x0
arr(*,3) = arr(*,3) + y0
arr(*,4) = arr(*,4) + z0
arr(*,5) = arr(*,5) + vx0
arr(*,6) = arr(*,6) + vy0
arr(*,7) = arr(*,7) + vz0


; FIRST PARTICLE IS THE GALAXY
;arr(0,*) = [mass,rscale,0.,0.,0.,0.,0.,0.]
arr(0,*) = [mass,soft,x0,y0,z0,vx0,vy0,vz0]


; PRINTING PARAMETERS TO THE SCREEN

; GETTING STRINGS
if not keyword_set(dhalo) then dhalo=13.0
strvxvyvz = strtrim(vx0,2)+'  '+strtrim(vy0,2)+'  '+strtrim(vz0,2)
strxyz = strtrim(x0,2)+'  '+strtrim(y0,2)+'  '+strtrim(z0,2)
strsoft = strtrim(soft,2)
strmmass = strtrim(mass,2)

; PRINTING INFO
if not keyword_set(noprint) then begin
  ; Printing the param inputs
  print,''
  print,'PARAM INPUTS'
  print,'-----------------------------------------------------'
  print,'XYZ    = ',strxyz,'   kpc'
  print,'VxVyVz = ',strvxvyvz,'   km/s'
  print,'Soft   = ',strsoft,'   kpc'
  print,'Mass   = ',strmmass,' Msun'
  print,'-----------------------------------------------------'
endif ; not noprint

if keyword_set(stp) then stop

end
