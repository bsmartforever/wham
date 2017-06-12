function flux_distance, lon, lat, d=d, f=f, magellanic=magellanic, egb=egb, log=log, ha=ha, T=T, mR=mR

;+
; Purpose: To determine the cartesion and spherical coordinants and ionizing fluxes of 
;    the MW and EGB along a sightline.
;
; Assumptions:
;    1) That the MW escaping ionizing radiation follows the Fox et al., 2005 model.
;       This model is the updated version of JBH 1999, 2001
;    2) That the EGB follows the Haardt & Madau 2001 model.
;    3) The Magellanic Coordinant system is defined by Nidever et al., 2008
;    4) If H-alpha intensity is returned, all assumptions listed in ha2lc.pro apply. 
;         
; Limitations: Does NOT include the contribution of the LMC or SMC.        
;
; lon - Longitude in degrees. Default: Galactic Lontitude unless "magellanic" keyword is passed
; lat - Latitude in degrees. Default: Galactic Latitude unless "magellanic" keyword is passed
;
; d   - Distance from the Sun in Kpc. Will return only the MW ionizing flux if passed. 
;       If egb is passed, then the MW+EGB ionizing flux is returned.
; f   - Total incident ionizing radiation (MW+EGB) in photons/(cm^2 s). 
;       Returns the distance from the Sun along the sightline with that predicted MW+EGB flux. 
;       Values less than 10 are assumed to be logrithmic.       
; magellanic - Sets passed lon and lat to Magellanic coordinants. 
; egb - Sets the returned flux, when d is passed, to MW+EGB
; log - Returns the flux in log.
;
; ha  - H-alpha intensity in Rayleighs. Returns the distance to an optically thick cloud with 
;       the passed H-alpha intensity. Assumes all the cloud's ionization is dominated by 
;       photoionization from the MW+EGB. 
; T   - Electron temperature of the cloud in K. Used in conjunction with "ha".
;       Values less than 10 are assumed to be logrithmic.   
; mR  - Converts passed ha intensity to mR. Used in conjunction with "ha".
;
; Output: Returns a structure containing the glon, glat, mlon, mlat, d, x, y, z, r, MW ionizing, and MW+EGB ionizing flux,
;   with all returned spatial lengths are in kpc, unless "d", "f", or "ha" is passed.
;
;   e.g.,
;   IDL> help,flux_distance(20.,30.),/str
;   ** Structure <a3b1e08>, 11 tags, length=104000, data length=104000, refs=1:
;     GLON            FLOAT     Array[2000]
;     GLAT            FLOAT     Array[2000]
;     MLON            DOUBLE    Array[2000]
;     MLAT            DOUBLE    Array[2000]
;     D               FLOAT     Array[2000]
;     X               FLOAT     Array[2000]
;     Y               FLOAT     Array[2000]
;     Z               FLOAT     Array[2000]
;     R               FLOAT     Array[2000]
;     MW              FLOAT     Array[2000]
;     MW_EGB          FLOAT     Array[2000]
;
;   Determine the MW ionizing flux at 20 kpc.
;   IDL> print,flux_distance(20.,30.,d=20.)
;      413048.
;
;   Determine the MW+EBG ionizing flux at 20 kpc.
;   IDL> print,flux_distance(20.,30.,d=20.,/egb)
;      445407.
;
;   Determine the log(MW+EGB) ionizing flux at 20 kpc.
;   IDL> print,flux_distance(20.,30.,d=20.,/egb,/log)
;      5.64876
;
;   Determine the distance in kpc where the MW+EBG radiation
;   is equal to 10^5 photons/(cm^2 s)
;   IDL> print,flux_distance(20.,30.,f=5)            
;      30.0000
;
;   Determine the distance in kpc to an optically thick cloud
;   with H-alpha intensity 0.1 R.
;   IDL> print,flux_distance(20.,30.,ha=.1)
;      25.6000
; 
; Created by Dr. Kat Barger 05/2013
;-

longitude=lon
latitude=lat

if keyword_set(ha) then begin
   egb=1
   log=0
endif

;mag2gal,mlon,mlat,glon,glat
IF keyword_set(magellanic) THEN $
   mag2gal, longitude, latitude, lon, lat

IF keyword_set(f) THEN IF f[0] LT 10. THEN f=10.^f

Flux=FLTARR(102,100)
get_lun,lun
dir='$HOME/PRO/KAT_PRO/Ionization_Processes/MW_MCs/'
;image 100x100 in 2kpc sampling. Only contains quadrent 1.
openr,lun,dir+'JBH_MW.dat'
readf,lun,flux
close,lun
free_lun,lun
flux=flux[0:99,0:99]

size=2000.
;Rebin Flux to 0.1 kpc bins.
;congrid = rebin, but does a better job at interpolating.
flux_kpc=congrid(flux,size,size,/interp)

r=findgen(size)/10.
z=reverse(findgen(size)/10.)

;Percision of tenths of a kpc
darr=findgen(200.*10.)/10.
lonarr=lon+fltarr(n_elements(darr))
latarr=lat+fltarr(n_elements(darr))
lbd2xyz,lonarr,latarr,darr,xarr,yarr,zarr
rarr=significant(sqrt(xarr^2.0+yarr^2.0),-1)
zarr=significant(zarr,-1)

xarr=significant(xarr,-1)
yarr=significant(yarr,-1)

rindex=fix((rarr)*10.)
zindex=fix(abs(zarr)*10.)

mw_flux=flux_kpc[rindex,zindex]
if keyword_set(egb) then egb=4.51 else egb=0
if keyword_set(log) then total_flux=alog10(10.^mw_flux+10.^egb) else total_flux=10.^mw_flux+10.^egb

if n_elements(d) ne 0 then BEGIN
   num=n_elements(d)
   indexarr=intarr(num)
   bad_index=where(d gt 199,bad_count)
   if bad_count ne 0 then begin
      d[bad_index]=199.0
      print,'*** Only distances of 199 kpc and less are valid. Assuming d=199 kpc. ***'
   endif   
   for i=0, num-1 do indexarr[i]=where(darr eq significant(d[i],-1))
   return, total_flux[indexarr]
endif

if keyword_set(f) then BEGIN
   num=n_elements(f)
   indexarr=intarr(num)
   for i=0, num-1 do begin
       temp=min(abs(10.^mw_flux+10.^egb-f[i]),index)
       indexarr[i]=index
   endfor   
   return, darr[indexarr]
endif

if keyword_set(ha) then begin
   if (NOT keyword_set(T)) then T=10.0^4.
   LC=ha2lc(ha,T=T,mR=n_elements(mR))
   num=n_elements(ha)
   indexarr=intarr(num)
   for i=0, num-1 do begin
       temp=min(abs(total_flux-LC[i]),index)
       indexarr[i]=index
   endfor
   return,darr[indexarr];
endif

gal2mag, lonarr, latarr, mlonarr, mlatarr

egb=4.51
if keyword_set(log) then $
   return,{glon:lonarr,glat:latarr,mlon:mlonarr,mlat:mlatarr,d:darr,x:xarr,y:yarr,z:zarr,r:rarr,mw:mw_flux,mw_egb:alog10(10.^mw_flux+10.^egb)} $
else $
   return,{glon:lonarr,glat:latarr,mlon:mlonarr,mlat:mlatarr,d:darr,x:xarr,y:yarr,z:zarr,r:rarr,mw:10.^mw_flux,mw_egb:10.^mw_flux+10.^egb};

end