function importnbody,file

; This function import the output data from an nbody run of galsat
; The output is a 3D array: [Nsnaps, Nbodies, [t,mass,x,y,z,Vx,Vy,Vz]]
; If there is a softening parameter included then:
;  [Nsnaps, Nbodies, [t,mass,soft,x,y,z,Vx,Vy,Vz]]
; NOTE:  Velocities will be returned in km/s.  They are automatically converted

d = findfile(file)
if d eq '' then return,-1

spawn,'wc '+file,out
nlines = float(first_el(getwrd(out,0)))

; figuring out how many parameters there are
openr,unit,file,/get_lun
dum = ''
readf,unit,dum   ; # bodies
readf,unit,dum   ; time
readf,unit,dum   ; pars for 1st body
idum = strsplit(dum)
npar = n_elements(idum)
close,unit
free_lun,unit

; opening file
openr,unit,file,/get_lun

; some variables
n = 0.
t = 0.0d
arr = dblarr(npar)
;arr = dblarr(7)
;arr = dblarr(13)
str = ''

; read number of bodies
readf,unit,n

; number of snaps
nsnap = nlines/(n+2.)

; creating array
;dum = {mass:0.d,x:0.d,y:0.d,z:0.d,vx:0.d,vy:0.d,vz:0.d}
;dum = {mass:0.d,x:dblarr(3),vel:dblarr(3)}
;dumn = replicate(dum,n)
;dum = {t:0.d,data:dblarr(7)}
;array = replicate(dum,nsnap)
;array = dblarr(nsnap,n,14)
;array = dblarr(nsnap,n,8)
array = dblarr(nsnap,n,npar+1)

; looping through the snaps
for i=0.,nsnap-1 do begin
  if i gt 0 then readf,unit,str     ; n

  readf,unit,t

  ; looping through the bodies
  for j=0.,n-1 do begin

    readf,unit,arr
    array(i,j,*) = [t,arr]

  end      ; for j

end     ; for i

close,unit
free_lun,unit

; CONVERTING VELOCITIES (kpc/Gyr -> km/s)
; 1 kpc = 3.0857E16 km, 1 Gyr = 3.1557E16 sec
; 1 kpc/Gyr = 3.0857/3.1557 = 0.977818
kms2kpcGyr = 3.0857d/3.1557d
kpcGyr2kms = 3.1557d/3.0857d
array(*,*,npar-2) = array(*,*,npar-2)*kpcGyr2kms
array(*,*,npar-1) = array(*,*,npar-1)*kpcGyr2kms
array(*,*,npar) = array(*,*,npar)*kpcGyr2kms

;stop

return,array

end
