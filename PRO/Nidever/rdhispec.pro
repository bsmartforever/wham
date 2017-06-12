pro rdhispec,lon,lat,spec,v,allb=allb,lds=lds,stp=stp

;  This gets an HI spectrum at a particular
;  longitude and latitude
;  If allb=1 then it will return all latitudes

if n_params() eq 0 then begin
  print,'Syntax - rdhispec,lon,lat,spec,v,[/allb,/lds,/stp]'
  return
endif

; are the inputs reasonable?
if (lon lt 0.) or (lon gt 359.5) or $
   (lat lt -90.) or (lat gt 90.) then begin
   spec = -1
   v = -1
   return
endif

; where am I?
spawn,'echo $HOST',host
unixdir = '/home/frosty/dln5q/radio/'
unix = findfile(unixdir)
if unix(0) ne '' then fdir = unixdir
if unix(0) eq '' then begin
  print,'NO FILES FOUND.  YOU MUST BE ON THE UVA ASTRO SUN SYSTEM'
  return
endif

if keyword_set(lds) then begin
  if unix(0) ne '' then fdir = '/net/ultrafly.grass/catalogs/atlas_of_galactic_hi/data/'
  if unix(0) eq '' then begin
    print,'LDS not availabe on this system!'
    return
  endif
endif

; getting the spectrum
strnum = strtrim(long(lon*10.),2)
if lon lt 100 then strnum = '0'+strnum
if lon lt 10 then strnum = '0'+strnum
if lon lt 1 then strnum = '0'+strnum
if lon eq 0 then strnum = '0000'
filename = 'l'+strnum+'~1.fit'
if keyword_set(lds) then filename = 'l'+strnum+'.fit'
;fdir = '/home/frosty/dln5q/radio/'
dum = findfile(fdir+filename)
if dum eq '' then begin
  print,'This file does not exist'
  return
endif
fits_read,fdir+filename,arr,head     ; Read in the SPECTRUM

crval1=sxpar(head,'CRVAL1')     ;Set the wavelength scale
cd1=sxpar(head,'CDELT1')
crpix1=sxpar(head,'CRPIX1')
dim=size(arr)

crval2=sxpar(head,'CRVAL2')
cd2=sxpar(head,'CDELT2')
crpix2=sxpar(head,'CRPIX2')

v=findgen(dim(1))
v=crval1+cd1*(v+1.-crpix1)
v=v/1000.

;stop

; getting the spectrum, if allb=1 then return the whole array
if not keyword_set(allb) then begin
  bind = 180.+2.*lat
  spec = arr(*,bind)
endif else spec = arr

if keyword_set(stp) then stop

end
