pro rdhispecm,lonr,latr,spec,v,l,b,vr=vr

; This does multiple rdhispec,/all

; where am I?
spawn,'echo $HOST',host
unixdir = '/home/frosty/dln5q/radio/'
unix = findfile(unixdir)
if unix(0) ne '' then fdir = unixdir
if unix(0) eq '' then begin
  print,'NO FILES FOUND.  YOU MUST BE ON THE UVA ASTRO SUN SYSTEM'
  return
endif

restore,fdir+'lbv_coords.dat'

; Longitude Range
dum = closest(lonr(0),l,ind=indl1)
dum = closest(lonr(1),l,ind=indl2)
l = l(indl1:indl2)
nl = indl2-indl1+1

; Latitude Range
dum = closest(latr(0),b,ind=indb1)
dum = closest(latr(1),b,ind=indb2)
b = b(indb1:indb2)
nb = indb2-indb1+1

; Velocity Range
if not keyword_set(vr) then vr=[-458.60452, 413.25943]
dum = closest(vr(0),v,ind=indv1)
dum = closest(vr(1),v,ind=indv2)
nv = indv2-indv1+1
v = v(indv1:indv2)

spec = fltarr(nl,nv,nb)

for i=0,nl-1 do begin
  if i mod 20 eq 0 then print,i*0.5
  rdhispec,lonr(0)+i*0.5,0,spec1,/all
  spec(i,*,*) = spec1(indv1:indv2,indb1:indb2)
end

;stop

end
