pro mwsat_sim,input,output,nbody=nbody,movie=movie,df=df,R0=R0,vcirc=vcirc,$
             noplot=noplot,step=step,ostep=ostep,dstep=dstep,fstep=fstep,$
             dhalo=dhalo

;+
; This program runs an N-test-body simulation of a
; galaxy with a plummer distribution of stars that
; orbits the Milky Way.
;
; INPUTS:
;  input    Input parameters [t,mass,soft,x,y,z,vx,vy,vz]
;  =nbody   Number of bodies to use in N-test-body simulation
;  /movie   Show the movie
;  /df      Use dynamical friction
;  =R0      Galactocentric distance of sun
;  =Vcirc   Circular rotation velocity of the sun
;  /noplot  Don't plot anything
;
; OUTPUTS:
;  output   Output parameters 
;
; CALLS:
;  plummer   To get the distribution of the N-bodies
;  galsat    To run the N-test-body simulation
;  plotnbody To plot the results or show the movie
; 
;
; By David Nidever   March 2006
;-

; INITIAL CONDITIONS
;input = [tstart,mass,soft,xstart,ystart,zstart,vxstart,vystart,vzstart]
tmin = input(0)
mass = input(1)
soft = input(2)
x0 = input(3)
y0 = input(4)
z0 = input(5)
vx0 = input(6)
vy0 = input(7)
vz0 = input(8)


; GETTING THE PLUMMER DISTRIBUTION OF STARS
;mass = 1e7
;rscale = 0.9*(mass/1d9)^(1./3.)

print,'USING ',strtrim(long(nbody),2),' STARS'
print,'Getting Plummer distribution of stars'
plummer,arr,nbody,mass,soft,/soft    


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
strvcirc = strtrim(vcirc,2)
strds = strtrim(dhalo,2)
strsoft = strtrim(soft,2)
strmmass = strtrim(mass,2)
strtmin = strtrim(tmin,2)

; PRINTING INFO
if not keyword_set(noprint) then begin
  ; Printing the param inputs
  print,''
  print,'PARAM INPUTS'
  print,'-----------------------------------------------------'
  print,'Tmin   = ',strtmin,'   Gyrs'
  print,'Vcirc  = ',strvcirc,'   km/s'
  print,'Dsoft  = ',strds,'   kpc'
  print,'XYZ    = ',strxyz,'   kpc'
  print,'VxVyVz = ',strvxvyvz,'   km/s'
  print,'Soft   = ',strsoft,'   kpc'
  print,'Mass   = ',strmmass,' Msun'
  print,'-----------------------------------------------------'
endif ; not noprint


t0 = systime(1)

; SETTING GALSAT PARAMETERS
;minstep = 0.001    ; minimum step
;maxstep = 0.001    ; maximum step
if not keyword_set(step) then step = 0.001       ; step size control parameter 
if keyword_set(movie) then ostep=0.01 else ostep=tmin  ; output interval
dstep = tmin       ; diagnostic interval
if not keyword_set(fstep) then fstep = 0.001  ; fixed timestep

; RUNNING GALSAT
galsat,arr,output,step=step,ostep=ostep,dstep=dstep,tmin=tmin,df=df,dhalo=dhalo,$
           minstep=minstep,maxstep=maxstep,fstep=fstep,/forward,vcirc=vcirc,prog='galsat7'

; Correct the time
output(*,*,0) = output(*,*,0)-tmin

print,systime(1)-t0,' seconds'


; PLOTTING THE LAST SNAP (CURRENT)
nsnap = n_elements(output(*,0,0))
if not keyword_set(noplot) then plotnbody,arr=output,/last,R0=R0

; SHOWING THE MOVIE
if keyword_set(movie) then begin

  ; Use the napo at the end
  colarr = reform(output(nsnap-1,*,9))

  ; Making reasonable colors, bound stars are white
  colarr2 = (colarr gt 0)*(colarr*30.+80) + (colarr eq 0)*255.
  colarr2(0) = 250   ; satellite is red

  plotnbody,arr=output,/movie,/notrail,ps=3,/dotfirst,R0=R0,colarr=colarr2
endif


;stop

end
