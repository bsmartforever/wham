pro galsat,input,output,step=step,dstep=dstep,ostep=ostep,prog=prog,$
           vcirc=vcirc,dhalo=dhalo,tmin=tmin,forward=forward,mw=mw,df=df,$
           minstep=minstep,maxstep=maxstep,fstep=fstep,noprint=noprint,$
           xtra=xtra

;+
; This program runs the c++ galsat program.  It's probably best to
; call it from a batch file. 
; NOTE: All velocities should be input in KM/S.  They will also be
;       output this way.  The velocity internal to the C++ galsat
;       program is kpc/Gyr, but galsat.pro and importnbody.pro 
;       automatically makes the right conversions.
;
; INPUT
;   input   Array of inputs [Nbodies, [mass,soft,x,y,z,Vx,Vy,Vz]]
;   step    Step size control parameter (default: step = 0.01)
;   dstep   Diagnostic interval (default: dstep = tmin)
;   ostep   Output interval (default: ostep = 0.01)
;   minstep Minimum timestep (positive), when using test particles
;   maxstep Maximum timestep (positive), when using test particles
;   fstep  Fixed timestep (positive), when using test particles
;   tmin    Minimum time to integrate to, ending time value (default tmin = 4.0)
;   prog    Name of the program to use (default prog='galsat6')
;   vcirc   Rotation velocity of the Milky Way disk (default vcirc = 220.0)
;   dhalo   Milky Way halo softening parameter (default ds = 13.0)
;   /forward  Integrate forward instead of backwards (default: forward=0)
;   /mw     Do NOT use the Milky Way potential (default: mw=0)
;   /df     USE dynamical friction (default df=0)
;   /xtra   Output extra debugging information
;
; OUTPUT
;   output  Array of galsat output. [Nsnap,Nbodies,[t,mass,soft,x,y,z,Vx,Vy,Vz]]
;
; Created by David Nidever July 2005
;-

; Bad Input Parameters
if n_params() eq 0 or n_elements(input) eq 0 then begin
  print,'syntax - galsat,input,output,step=step,dstep=dstep,ostep=ostep,'
  print,'                prog=prog,vcirc=vcirc,dhalo=dhalo,tmin=tmin,foward=forward'
  print,'                mw=mw,df=df,minstep=minstep,maxstep=maxstep,fstep=fstep'
  print,'                noprint=noprint'
endif

; Where am I?
dir = userdir()
bindir = dir+'nbody/'

; 1 kpc = 3.0857E16 km, 1 Gyr = 3.1557E16 sec
; 1 kpc/Gyr = 3.0857/3.1557 = 0.977818
kms2kpcGyr = 3.0857d/3.1557d
kpcGyr2kms = 3.1557d/3.0857d

if n_elements(prog) eq 0 then prog='galsat6'
if n_elements(tmin) eq 0 then tmin=4.0
if n_elements(step) eq 0 then step=0.01
if n_elements(dstep) eq 0 then dstep=tmin
if n_elements(ostep) eq 0 then ostep=0.01
if n_elements(vcirc) eq 0 then vcirc=220.
if n_elements(dhalo) eq 0 then dhalo=13.0
if n_elements(forward) eq 0 then forward=0
if n_elements(mw) eq 0 then mw=0
if n_elements(df) eq 0 then df=0

; # of Bodies
sz = size(input)
case sz(0) of
  1: nbody = 1
  2: nbody = sz(1)
endcase

; Getting inputs ready
if tmin lt 0. then strtmin = stringize(-tmin,ndec=2) else strtmin=strtrim(tmin,2)
strstep = strtrim(step,2)
strdstep = strtrim(dstep,2)
strostep = strtrim(ostep,2)
strvcirc = strtrim(vcirc,2)
strdhalo = strtrim(dhalo,2)
strnbody = strtrim(nbody,2)

; Setting flags
flags = ''
if keyword_set(forward) then flags = flags+' -f'
if keyword_set(mw) then flags = flags+' -m'
if keyword_set(df) then flags = flags+' -y'
if keyword_set(xtra) then flags = flags+' -x'

; CONVERTING VELOCITIES (km/s -> kpc/Gyr)
input2 = input
input2(*,5) = input2(*,5)*kms2kpcgyr
input2(*,6) = input2(*,6)*kms2kpcgyr
input2(*,7) = input2(*,7)*kms2kpcgyr

; CREATING INPUT FILE
tfile = maketemp('tgal')
openw,unit,/get_lun,tfile+'.in'
printf,unit,strnbody
printf,unit,'0'
printf,unit,strvcirc
printf,unit,strdhalo
if nbody gt 1 then $
    for i=0.,nbody-1 do printf,unit,strtrim(transpose(input2[i,*]),2)
;    for i=0.,nbody-1 do printf,unit,strtrim(input2(i,*),2)
if nbody eq 1 then printf,unit,strtrim(input2,2)
;printf,unit,'2.0e10 ',strsoft,' ',strxyz,' ',strvxvyvz
;printf,unit,'7.5e8 0.0 16.1556 2.27043 -5.88738  237.832 -42.3568 221.998'
close,unit
free_lun,unit

; Printing info
if not keyword_set(noprint) then begin

  ; Printing the param inputs
  print,'PARAM INPUTS'
  print,'-----------------------------------------------------'
  print,'Prog = ',prog
  print,'Nbodies = ',strnbody
  print,'Tmin   = ',strtmin,'   Gyrs'
  print,'Vcirc  = ',strvcirc,'   km/s'
  print,'Dhalo  = ',strdhalo,'   kpc'
  print,'Step   = ',strstep
  print,'Ostep  = ',strostep,' Gyrs'
  print,'Dstep  = ',strdstep,' Gyrs'
  if keyword_set(forward) then print,'Integrating FORWARDS'
  if keyword_set(mw) then print,'NOT Using Milky Way Potential' else $
     print,'Using Milky Way Potential'
  if keyword_set(df) then print,'Using Dynamical Friction'
  print,'-----------------------------------------------------'
endif ; not noprint

; RUNNING GALSAT
print,' '
;print,'Running GALSAT program ... 3 sec.'
print,'Running GALSAT program ...'
;spawn,'\rm '+tfile+'.out'
cmd = '( '+bindir+prog+' -d '+strstep+' -o '+strostep+' -t '+strtmin+' -i -e '+strdstep
if keyword_set(minstep) then cmd = cmd+' -l '+strtrim(minstep,2)
if keyword_set(maxstep) then cmd = cmd+' -c '+strtrim(maxstep,2)
if keyword_set(fstep) then cmd = cmd+' -s '+strtrim(fstep,2)
cmd = cmd + flags+' < '+tfile+'.in > '+tfile+'.out ) > & '+tfile+'.diag'

stop

spawn,cmd,dum

;spawn,'./'+prog+' -d 0.01 -o 0.01 -t '+strtmin+' -i -e '+strtmin+' < '+tfile+'.in > '+tfile+'.out',dum

; GETTING THE OUTPUT
fileinfo = file_info(tfile+'.out')
fsize = fileinfo.size

;stop

; Everything in one file
if (fsize gt 0) then begin
  output = importnbody(tfile+'.out')

; In separate files
endif else begin
  galfiles = file_search('gal*.out')
  nfiles = n_elements(galfiles)

  d = importnbody(galfiles(0))
  sz = size(d)
  output = dblarr(nfiles,sz(2),sz(3))

  ; Restoring the files
  for i=0L,nfiles-1 do begin
    d = importnbody(galfiles(i))
    output(i,*,*) = d
  end

  ; Sort by time
  si = sort(output(*,0,0))
  output = output(si,*,*)

  stop

endelse


;stop

; REMOVING TEMPORARY FILES
spawn,'\rm '+tfile+'.in'
spawn,'\rm '+tfile+'.out'
spawn,'\rm '+tfile+'.diag'

;stop

end
