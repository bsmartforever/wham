pro plummer,arr,n,mass,rscale,soft=soft

; This program returns a plummer model of N stars
; with the given mass and spatial scale.  The velocity
; scale is set by the mass and spatial scale.
; 
; If rscale is not set then a scale is computed for
; an average "fluffy" galaxy us the relation:
;
;   rscale = 0.9 * (Mass/1e9)^(1/3)
;
; INPUT
;
;   N       Number of particles
;   MASS    Total mass of system in solar masses
;   RSCALE  Spatial scale of galaxy in kpc
;   /SOFT   Put in a column for softening parameters (set to 0.)
;
; OUTPUT
;
;   ARR     Array of particles and their properties, [N,7 or 8]
;           soft = 0,   mass, x, y, z, vx, vy, vz
;           soft = 1,   mass, 0, x, y, z, vx, vy, vz

; no parameters set
if n_params() eq 0 then begin
  print,' plummer, arr, n, mass, rscale, /soft'
  return
endif

if n_elements(n) eq 0 then n=100000.
if n_elements(mass) eq 0 then mass=1.
if n_elements(rscale) eq 0 then rscale = 0.9*(mass/1e9)^(1./3.)

; getting the data from "BI"
dir = userdir()+'orbit/'
;dir = '/home/frosty/dln5q/orbit/'
biarr = fltarr(8,100000)
openr,unit,dir+'BI',/get_lun
readf,unit,nbodies,tnow
readf,unit,biarr
close,unit
free_lun,unit

; put in a softening parameter column
if keyword_set(soft) then begin

  arr = fltarr(n,8)
  arr(*,0) = mass/float(n)
  arr(*,2:4) = biarr(1:3,0:n-1)  ;x,y,z
  arr(*,5:7) = biarr(4:6,0:n-1)  ;vx,vy,vz

  ; scaling the spatial part
  rold = 1.5
  arr(*,2:4) = arr(*,2:4)*rscale/rold

  ; scaling the velocity part
  G = 4.4967e-6              ; G in kpc/Gyr^2
  vscale = sqrt(G*mass/rscale)
  arr(*,5:7) = arr(*,5:7)*vscale

; no softening parameter column
endif else begin

  arr = fltarr(n,7)
  arr(0,*) = mass/float(n)
  arr(*,1:3) = biarr(1:3,0:n-1)   ;x,y,z
  arr(*,4:6) = biarr(4:6,0:n-1)   ;vx,vy,vz

  ; scaling the spatial part
  rold = 1.5
  arr(*,1:3) = arr(*,1:3)*rscale/rold

  ; scaling the velocity part
  G = 4.4967e-6              ; G in kpc/Gyr^2
  vscale = sqrt(G*mass/rscale)
  arr(*,4:6) = arr(*,4:6)*vscale

endelse

;stop

end
