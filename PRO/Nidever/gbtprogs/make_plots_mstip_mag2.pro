pro make_plots_mstip_mag2,bigcube,head

; Make MS plots from the BIGCUBE
dir = '/Volumes/data/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/data/'
;dir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/data/'

plotdir = '/Volumes/data/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/plots/'
;plotdir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT11B-082/plots/'

setdisp

;tag = '_psm'
tag = '_pvsm'
;tag = ''
if n_elements(bigcube) eq 0 then begin
  fits_read,dir+'grid_mstip_mag'+tag+'.fits',bigcube,head
endif
fits_read,dir+'grid_mstip_mag'+tag+'_rmsmap.fits',rmsmap,rmshead


bd = where(finite(bigcube) eq 0,nbd)
if nbd gt 0 then bigcube[bd]=0.0

; Make the GBT only plots
; Column density all
; Column density, velocity cut
; Column density, velocity and sigma cut
; Vlsr vs. mlon
; Vlsr vs mlat


fits_arrays,head,mlon,mlat,vel
dvel = abs(vel[1]-vel[0])
step = sxpar(head,'CDELT1')

sz = size(bigcube)
nx = sz[1]
ny = sz[2]
nchan = sz[3]

goto,SKIP1
;goto,velsigcut

; Make final RMS map
;----------------------
;print,'Making RMS map'
;tot3 = total(bigcube,3)
;gdints = where(tot3 ne 0.0,ngdints)
;rmsmap = fltarr(sz[1],sz[2])+1d30
;
;for i=0L,ngdints-1 do begin
;  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
;  ind2 = array_indices(tot3,gdints[i])
;  xind = ind2[0]
;  yind = ind2[1]
;  spec = reform(bigcube[xind,yind,*])
;  rmsmap[xind,yind] = MAD(spec)
;end



;posim = [0.10, 0.08, 0.97, 0.86]
;poscol = [0.10, 0.96, 0.97, 0.985]
posim = [0.07, 0.10, 0.97, 0.86]
poscol = [0.07, 0.96, 0.97, 0.985]


; Column density
tot3 = total(bigcube,3)

;; Getting Boundary
;;------------------
;mask = float(tot3 ne 0.0)
;gd = where(mask eq 0 and (shift(mask,-1,0) eq 1 or shift(mask,1,0) eq 1 or shift(mask,0,-1) eq 1 or shift(mask,0,1) eq 1 $
;                          or shift(mask,-1,-1) eq 1 or shift(mask,-1,1) eq 1 or shift(mask,1,-1) eq 1 $
;                          or shift(mask,1,1) eq 1))
;gd2 = array_indices(mask,gd)
;xbound = reform(gd2[0,*])
;ybound = reform(gd2[1,*])
;nbound = n_elements(xbound)
;
;; Remove little dents
;bd = where(xbound eq 0,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound ge 140 and xbound le 146 and ybound ge 18 and ybound le 30,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound ge 107 and xbound le 112 and ybound ge 25 and ybound le 34,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 239 and ybound eq 47,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 240 and ybound eq 47,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 77 and ybound eq 36,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 77 and ybound eq 37,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 82 and ybound eq 30,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 109 and ybound eq 24,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 141 and ybound eq 16,nbd)
;remove,bd,xbound,ybound
;bd = where(xbound eq 141 and ybound eq 15,nbd)
;remove,bd,xbound,ybound
;; Add missing point
;xbound = [xbound,277]
;ybound = [ybound,63]
;
;xtemp = xbound
;ytemp = ybound
;REMOVE,[0,1],xtemp,ytemp
;;xtemp[0] = -1
;;ytemp[0] = -1
;xbound2 = xbound
;ybound2 = ybound
;i = 2
;flag = 1
;WHILE (flag eq 1) do begin
;
;  ix = xbound2[i-1]
;  iy = ybound2[i-1]
;
;  ; KLUDGE
;  ;   there are two rows right next to each other
;  ;if ix eq 10 and iy eq 310 then begin
;  ;  bd = where(ytemp eq 310 and xtemp lt 50,nbd)
;  ;  xx = xtemp[bd]
;  ;  yy = ytemp[bd]
;  ;  si = sort(xx)
;  ;  xx = xx[si]
;  ;  yy = yy[si]
;  ;  xbound2[i:i+nbd-1] = xx
;  ;  ybound2[i:i+nbd-1] = yy
;  ;  REMOVE,bd,xtemp,ytemp
;  ;  i = i+nbd
;  ;  goto,bomb
;  ;endif
;
;  xdiff = xtemp-xbound2[i-1]
;  ydiff = ytemp-ybound2[i-1]
;  diff = sqrt(xdiff^2 + ydiff^2)
;  gg = where(diff eq 1,ngg)
;  if ngg ne 1 then stop
;  xbound2[i] = xtemp[gg[0]]
;  ybound2[i] = ytemp[gg[0]]
;
;  if n_elements(xtemp) eq 1 then begin
;    flag = 0
;    goto,bomb
;  end
;
;  REMOVE,gg[0],xtemp,ytemp
;  ;xtemp[gg[0]] = -1
;  ;ytemp[gg[0]] = -1
;  ;stop
;
;  i++
;
;  BOMB:
;
;ENDWHILE
;
;; get MLON boundary
;ml_bound = mlon[xbound2]
;mb_bound = mlat[ybound2]

;save,ml_bound,mb_bound,file='mstip_survey_boundary.dat'

; MS velocity range
zlo = 90 ;150 ; 90  ;101
zhi = 250  ;210 ;250 ;180 ;160
; this velocity range includes the CHVCs
;zlo = 43
;zhi = 165
zmin=-1.2 & zmax=1.2



; Column Density plot
;-----------------------
col3 = tot3 * dvel * 1.83d18 / 1d18   ; column density


;; Column density with MS velocity cut and 3*sigma points
;tot3a = tot3*0.
;x = findgen(nx)
;z = findgen(nchan)
;mask = fltarr(nx,nchan)
;xx = findgen(nx)#(fltarr(nchan)+1.0)
;yy = (fltarr(nx)+1.0)#findgen(nchan)
;zlo = 101
;zhi = 160
;for i=0,ny-1 do begin
;  if (i+1) mod 200 eq 0 then print,strtrim(i+1,2),'/',strtrim(ny,2)
;  slice = reform(bigcube[*,i,*])
;  slicerms = reform(rmsmap[*,i])#(fltarr(nchan)+1.0)
;  slice = slice[*,zlo:zhi]
;  slice = slice*float(slice gt 3.0*slicerms)
;  totslice = total(slice,2)  ; sum in vel
;  tot3a[*,i] = totslice
;end
;
;; Column Density plot
;col3a = tot3a * dvel * 1.83d18 / 1d18   ; column density


charsize = 1.4

file = plotdir+'grid_mstip_mag_coldens'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7
;device,/cmyk
displayc,col3,mlon,mlat,min=0.01,max=30,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='MS Latitude',charsize=charsize,tit='MS Velocities',maskv=0.0,maskc=255
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=1
xr = reverse(minmax(mlon))
yr = minmax(mlat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps

;goto,skiptomlon

;stop


; Column density with MS velocity cut and some sigma cut
;---------------------------------------------------------



zlo = 90
zhi = 250 ;250
tot3c = col3*0
med = median(bigcube[*,*,0:300],dim=3)
for i=zlo,zhi do begin
  tot3c += (bigcube[*,*,i]-med) > 0.001 ;0.002
endfor
col3c = tot3c * dvel * 1.83d18 / 1d18   ; column density
zmin = 0.5 ;0.95
zmax = 2.5 ;2.3 ;2.0
file = plotdir+'grid_mstip_mag_coldens_velcut_sigmamed_z'+string(zlo,format='(I03)')+'-'+string(zhi,format='(I03)')

zlo = 90
zhi = 250 ;250
tot3c = total(bigcube[*,*,zlo:zhi] > 0.002,3)
col3c = tot3c * dvel * 1.83d18 / 1d18   ; column density
zmin = 0.95
zmax = 2.3 ;2.0
file = plotdir+'grid_mstip_mag_coldens_velcut_sigma_z'+string(zlo,format='(I03)')+'-'+string(zhi,format='(I03)')

!p.font = 0

bd = where(rmsmap gt 1000,nbd)
col3c[bd]=0

ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7
;device,/cmyk
;displayc,col3c,mlon,mlat,min=0.01,max=30,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,gsmooth(col3c,3),mlon,mlat,min=-1,max=1,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,col3c,mlon,mlat,min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='MS Latitude',charsize=charsize,tit='MS Column Density ('+stringize(vel[zlo],ndec=1)+' <V!dLSR!n< '+$
         stringize(vel[zhi],ndec=1)+' km/s)',maskv=0,maskc=255
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=1

; overplot galactic coorinates

; b=-10
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)-10.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-137,9,'b=-10',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; b=0
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)+0.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-147.5,9,'b=0',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; b=10
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)+10.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-157.5,9,'b=+10',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; l=110
gbarr = scale_vector(dindgen(1000),-90,30)
glarr = dblarr(1000)+110.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-135,12,'l=110',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; l=120
gbarr = scale_vector(dindgen(1000),-90,30)
glarr = dblarr(1000)+120.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-135,21.5,'l=120',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4


xr = reverse(minmax(mlon))
yr = minmax(mlat)
;plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
;           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps
;ps2jpg,file+'.eps',/eps

stop

; Column density with MS velocity cut
;------------------------------------------
;for i=0,29 do begin
for i=0,11 do begin

dz = 25
;dz = 10
zlo = i*dz
zhi = zlo+dz

;;zlo=150 & zhi=160

tot3c = total(bigcube[*,*,zlo:zhi],3)
col3c = tot3c * dvel * 1.83d18 / 1d18   ; column density

gg = where(col3c ne 0.0,ngg)
sig = mad(col3c[gg])
zmin=-3.5*sig & zmax=3.5*sig

;;zmin=-0.35 & zmax=0.35
;zmin=-0.60 & zmax=0.60

!p.font = 0

;file = plotdir+'grid_mstip_mag_coldens_velcut_z'+string(zlo,format='(I03)')+'-'+string(zhi,format='(I03)')
file = plotdir+'grid_mstip_mag_coldens_velcut_s'+strtrim(dz,2)+'_z'+string(zlo,format='(I03)')+'-'+string(zhi,format='(I03)')
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7
;device,/cmyk
;displayc,col3c,mlon,mlat,min=0.01,max=30,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,gsmooth(col3c,3),mlon,mlat,min=-1,max=1,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,col3c,mlon,mlat,min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='MS Latitude',charsize=charsize,tit='MS Column Density ('+stringize(vel[zlo],ndec=1)+' <V!dLSR!n< '+$
         stringize(vel[zhi],ndec=1)+' km/s)',maskv=0,maskc=255
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=1

; overplot galactic coorinates

; b=-10
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)-10.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-137,9,'b=-10',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; b=0
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)+0.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-147.5,9,'b=0',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; b=10
glarr = scale_vector(dindgen(1000),0,360)
gbarr = dblarr(1000)+10.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-157.5,9,'b=+10',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; l=110
gbarr = scale_vector(dindgen(1000),-90,30)
glarr = dblarr(1000)+110.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-135,12,'l=110',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4

; l=120
gbarr = scale_vector(dindgen(1000),-90,30)
glarr = dblarr(1000)+120.0
gal2mag,glarr,gbarr,mlarr,mbarr
gd = where(mlarr ge min(mlon) and mlarr le max(mlon) and mbarr ge min(mlat) and mbarr le max(mlat),ngd)
xyouts,-135,21.5,'l=120',align=0.5,charsize=1.1,charthick=2
oplot,mlarr[gd],mbarr[gd],linestyle=2,thick=4


xr = reverse(minmax(mlon))
yr = minmax(mlat)
;plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
;           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps
;ps2jpg,file+'.eps',/eps

endfor

stop


;------------------------
; Velocity vs. MLON
;------------------------
SKIPTOMLON:
charsize = 1.4

;posim = [0.12, 0.11, 0.98, 0.85]
;poscol = [0.12, 0.96, 0.98, 0.985]

posim = [0.12, 0.09, 0.97, 0.85]
poscol = [0.12, 0.96, 0.97, 0.985]

for i=0,45 do begin

dy = 5
ylo = i*dy
yhi = ylo+dy

; All MLAT
tot2 = total(bigcube[*,ylo:yhi,*],2)
;tot2 = reverse(tot2,2)  ; invert

;tot2a = tot2*0.0
;for i=0,nchan-1 do begin
;  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
;  slice = reform(bigcube[*,*,i])
;  slice = slice*float(slice gt 3.0*rmsmap)
;  totslice = total(slice,2)  ; sum in Y/MLAT
;  tot2a[*,i] = totslice
;end

imtot2 = tot2*step  ; multiply by angular step

;zmin = -0.01
;zmax = 0.01
zmin = -0.005
zmax = 0.005

;dum = closest(-600.0,vel,ind=ind2)
;dum = closest(200.0,vel,ind=ind1)
ind1 = 43   ;0    ;zlo
ind2 = 280 ;192  ;zhi
ind1=0 & ind2=300

file = plotdir+'grid_mstip_mag_velmlon_s'+strtrim(dy,2)+'_y'+string(ylo,format='(I03)')+'-'+string(yhi,format='(I03)')
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7.0
;device,/cmyk
;displayc,tot2,mlon,vel,min=-0.1,max=10,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,imtot2[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=0.7,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,imtot2[*,ind1:ind2],mlon,vel[ind1:ind2],min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,$
         xtit='MS Longitude',ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,$
         tit=stringize(mlat[ylo],ndec=1)+' < Y <'+stringize(mlat[yhi],ndec=1)
xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2jpg,file+'.eps',/eps

;stop

endfor

stop


; Column density with MS velocity cut and 3*sigma points
;---------------------------------------------------------
velsigcut:
zlo=90 & zhi=250
;zlo=150 & zhi=210
;zmin=-0.5 & zmax=2
zmin=0.5 & zmax=2

;zmin=-1
;zmax=3

posim = [0.12, 0.09, 0.97, 0.85]
poscol = [0.12, 0.96, 0.97, 0.985]

tot3 = total(bigcube,3)
tot3d = tot3*0.0
for i=zlo,zhi do begin
  slice = reform(bigcube[*,*,i])
  ;tot3d = tot3d + slice*float(slice gt 3.0*rmsmap)
  ;tot3d = tot3d + slice*float(slice gt 2.0*rmsmap)
  ;tot3d = tot3d + slice*float(slice gt 2.0*0.0041)
  tot3d = tot3d + slice*float(slice gt 0.002)
end
;tot3d = total(bigcube[*,*,zlo:zhi]>0.002,3)
;tot3d = total(bigcube[*,*,489:636]>3.0*0.01,3)
col3d = tot3d * dvel * 1.83d18 / 1d18   ; column density

bb = where(rmsmap gt 1)
col3d[bb]=100

file = plotdir+'grid_mstip_mag_coldens_velsigcut_'+string(zlo,format='(I03)')+'-'+string(zhi,format='(I03)')
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7
;device,/cmyk
;displayc,col3d,mlon,mlat,min=0.01,max=30,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,col3d,mlon,mlat,min=0.01,max=20,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,col3d,mlon,mlat,min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;         ytit='MS Latitude',charsize=charsize,tit='MS Velocities and 3'+textoidl('\sigma')+' cut',maskv=0
         ytit='MS Latitude',charsize=charsize,tit='HI Column Density',maskv=100
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=2 ;1


xyouts,106.3,-30.2,'(a)',charsize=charsize,co=255,align=0.5
xr = reverse(minmax(mlon))
yr = minmax(mlat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


; The RMS in column density (10^18) is roughly 0.01 or 10^16 atoms/cm^2 
; over 1.6 km/s. That's with velocity and spatial smoothing.
;
; What are the peak N(HI) and Tb for the MS cloudlets

stop



; Velocity map with velocity cut

; Column density with MS velocity cut and 3*sigma points

vmap1 = tot3*0.0
for i=zlo,zhi do begin
  slice = reform(bigcube[*,*,i])
  slice = slice*float(slice gt 3.0*rmsmap)
  vmap1 = vmap1 + slice*vel[i]
end
vmap1 = vmap1/tot3d
bd = where(finite(vmap1) eq 0)
vmap1[bd] = -265.77
;imwt = alog10((100*tot3d+1)<400)   ; <1000
imwt = alog10((100*tot3d+1)<1000)   ; <1000

file = plotdir+'grid_mstip_mag_vmap'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7
;device,/cmyk
display_avg,vmap1,imwt,mlon,mlat,zmin=-450,zmax=-300,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='MS Latitude',charsize=charsize,tit='Velocity Map',colformat='(I5)',coldivisions=3,$
         minbright=0.0
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=2 ;1
xyouts,106.3,-30.2,'(b)',charsize=charsize,co=255,align=0.5
xr = reverse(minmax(mlon))
yr = minmax(mlat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


stop


;------------------------
; Velocity vs. MLON
;------------------------
SKIP1:
charsize = 1.4

zlo = 90
zhi = 250

;posim = [0.12, 0.11, 0.98, 0.85]
;poscol = [0.12, 0.96, 0.98, 0.985]

posim = [0.12, 0.09, 0.97, 0.85]
poscol = [0.12, 0.96, 0.97, 0.985]

; All MLAT
tot2 = total(bigcube,2)
;tot2 = reverse(tot2,2)  ; invert

;tot2a = tot2*0.0
;for i=0,nchan-1 do begin
;  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
;  slice = reform(bigcube[*,*,i])
;  slice = slice*float(slice gt 3.0*rmsmap)
;  totslice = total(slice,2)  ; sum in Y/MLAT
;  tot2a[*,i] = totslice
;end

imtot2 = tot2*step  ; multiply by angular step

;dum = closest(-600.0,vel,ind=ind2)
;dum = closest(200.0,vel,ind=ind1)
ind1 = 43   ;0    ;zlo
ind2 = 280 ;192  ;zhi

file = plotdir+'grid_mstip_mag_velmlon_all'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7.0
;device,/cmyk
;displayc,tot2,mlon,vel,min=-0.1,max=10,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,imtot2[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=0.7,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,imtot2[*,ind1:ind2],mlon,vel[ind1:ind2],min=-0.01,max=0.05,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='All Latitudes'
xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps

;stop

; Sigma cut
tot2b = tot2*0.0
for i=0,nchan-1 do begin
  slice = reform(bigcube[*,*,i])
  ;slice = slice*float(slice gt 3.0*rmsmap)
  ;slice = slice*float(slice gt 2.0*rmsmap)
  slice = slice*float(slice gt 0.002)
  totslice = total(slice,2)
  tot2b[*,i] = totslice
end
;tot2b = reverse(tot2b,2)  ; invert

; mask with total column density image
tot2c = tot2*0.0
tot3c = total(bigcube[*,*,zlo:zhi]>0.002,3)
mask = float(tot3c gt 2*rmsmap*sqrt(zhi-zlo+1))
for i=0,nchan-1 do begin
  slice = reform(bigcube[*,*,i])
  slice = slice*mask
  totslice = total(slice,2)
  tot2c[*,i] = totslice
end

imtot2c = tot2c*step  ; multiply by angular step

file = plotdir+'grid_mstip_mag_velmlon_sigcut'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7.0
;device,/cmyk
;displayc,tot2b,mlon,vel,min=0.01,max=10,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,imtot2b,mlon,vel,min=0.0006,max=0.6,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,imtot2b[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=1.0,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
;displayc,imtot2b[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=0.5,/log,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
displayc,imtot2c[*,ind1:ind2],mlon,vel[ind1:ind2],min=-0.01,max=0.05,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='All Latitudes',colformat='(F6.4)',$
         coltickname=['0.001','0.01','0.1','1'],maskv=0
xyouts,106.3,420,'(a)',charsize=charsize,co=255,align=0.5
xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


; Another attempt
;-----------------

tot2d = total(bigcube>0.002,2)
imtot2d = tot2d*step  ; multiply by angular step
zmin = 0.3*step
zmax = 1.5*step

file = plotdir+'grid_mstip_mag_velmlon_sigma'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7.0
displayc,imtot2d[*,ind1:ind2],mlon,vel[ind1:ind2],min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='All MS Latitudes',colformat='(F6.4)',$
         maskv=0
;xyouts,106.3,420,'(a)',charsize=charsize,co=255,align=0.5
xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


; mask out CHVCs
;----------------

bigcube2 = bigcube
bigcube2[405:432,107:132,*] = 0.0
bigcube2[288:314,178:200,*] = 0.0
bigcube2[369:380,63:76,*] = 0.0

tot2e = total(bigcube2>0.002,2)
imtot2e = tot2e*step  ; multiply by angular step
zmin = 0.035
zmax = 0.065

file = plotdir+'grid_mstip_mag_velmlon_sigma_masked'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=7.0
displayc,imtot2e[*,ind1:ind2],mlon,vel[ind1:ind2],min=zmin,max=zmax,/xflip,posim=posim,poscol=poscol,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='CHVCs Masked',colformat='(F6.4)',$
         maskv=0
;xyouts,106.3,420,'(a)',charsize=charsize,co=255,align=0.5
xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


stop

;--------------------
; The 2-panel figure
;--------------------
file = plotdir+'grid_mstip_mag_2panel'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=10

posim1 =  [0.09, 0.40, 0.97, 0.89]
poscol1 = [0.09, 0.96, 0.97, 0.985]
posim2 =  [0.09, 0.07, 0.97, 0.40]
;poscol2 = [0.07, 0.96, 0.97, 0.985]


; Column density
displayc,col3d,mlon,mlat,min=0.01,max=20,/log,/xflip,posim=posim1,poscol=poscol1,xtit=' ',xtickformat='(A1)',$
         ytit='MS Latitude',charsize=charsize,tit='MS HI Column Density (10!u18!n atoms/cm!u2!n; -386 < V!dLSR!n < -291 km/s)',maskv=0,/interp
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=2 ;1
xyouts,-134.5,19,'(a)',charsize=charsize,co=255,align=0.5

; Overplot BT04 boundary, at DEC=+50.0
bt04_ra = scale_vector(findgen(200),-20,340)
bt04_dec = fltarr(200)+50.0
glactc,bt04_ra,bt04_dec,2000.,bt04_gl,bt04_gb,1,/deg
gal2mag,bt04_gl,bt04_gb,bt04_ml,bt04_mb,/wrap
;adxy,head,bt04_ml,bt04_mb,bt04_x,bt04_y
oplot,bt04_ml,bt04_mb,thick=5,color=255,linestyle=2
xyouts,-134.5,18.3,'Edge of',align=0,charsize=1.5,charthick=4,color=255
xyouts,-134.5,17.7,'BT04 Survey',align=0,charsize=1.5,charthick=4,color=255

; Overplot Galactic midplane
gl_midplane = scale_vector(findgen(200),0,359)
gb_midplane = fltarr(200)+0.0
gal2mag,gl_midplane,gb_midplane,ml_midplane,mb_midplane,/wrap
oplot,ml_midplane,mb_midplane,thick=10,color=250
xyouts,-150.9,18.0,'GALACTIC',align=0.5,charsize=1.9,charthick=5,color=250
xyouts,-150.9,17.5,'MIDPLANE',align=0.5,charsize=1.9,charthick=5,color=250

; New MS detections
xyouts,-146.3,11.9,'New MS Detections',align=0.5,charsize=2.0,charthick=5,color=255
;arrow,-146,12.5,-142,13.5,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;arrow,-146,12.5,-145.5,13.5,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
arrow,-147.5,12.4,-146.5,13.4,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
arrow,-147.5,12.4,-150.6,14.1,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
tvellipse,3.8,2.1,-144,14.6,-15,color=255,/data
tvellipse,0.8,0.8,-151.2,14.7,0,color=255,/data

xr = reverse(minmax(mlon))
yr = minmax(mlat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim1,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'

; Vlsr vs. MLON
displayc,imtot2b[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=1.0,/log,/xflip,posim=posim2,poscol=poscol2,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit=' ',colformat='(F6.4)',$
         coltickname=['0.001','0.01','0.1','1'],maskv=0,/noerase,/nocolorbar
xyouts,-134.5,-260,'(b)',charsize=charsize,co=255,align=0.5

; Overplot velocity range
oplot,[-120,-170],[0,0]+vel[zlo],linestyle=1,color=255
oplot,[-120,-170],[0,0]+vel[zhi],linestyle=1,color=255
;xyouts,-149,-280,'MS Velocity Range Used',align=0.5,charsize=1.5,charthick=5,color=255
xyouts,-149.5,-405,'MS Velocity Range Used',align=0.5,charsize=1.5,charthick=5,color=255

; CHVC labels
xyouts,-137,-430,'CHVC 402',align=0.5,charsize=1.2,charthick=4,color=255
xyouts,-143.4,-450,'CHVC 391',align=0.5,charsize=1.2,charthick=4,color=255

; MS Velocity Fiducial curve, two polynomials
restore,'/net/halo/dln5q/gbt/make_plots_mag_velfiducial2.dat'
g1 = where(mlon lt -115.7) ;-116.0, -115.7, -117
vfid = poly(mlon,coef3)
vfid[g1] = poly(mlon[g1],coef4b)
;oplot,mlon,vfid,co=255,thick=2
;oplot,mlon,vfid,co=0,thick=2,linestyle=2
oplot,mlon,vfid,co=255,thick=5,linestyle=2

xyouts,-139,-280,'MS Velocity Fiducial',align=0.5,charsize=1.5,charthick=4,co=255
arrow,-139.0,-285,-139.8,-352,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255

xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim2,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)',xticklen=0.04
ps_close
ps2gif,file+'.eps',/eps


;--------------------------------------
; BLACK AND WHITE 2 PANEL
;--------------------------------------
file = plotdir+'grid_mstip_mag_2panel_bw'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=13,ysize=10
loadcol,3

posim1 =  [0.09, 0.40, 0.97, 0.89]
poscol1 = [0.09, 0.96, 0.97, 0.985]
posim2 =  [0.09, 0.07, 0.97, 0.40]
;poscol2 = [0.07, 0.96, 0.97, 0.985]


; Column density
displayc,col3d,mlon,mlat,min=0.01,max=20,/log,/xflip,posim=posim1,poscol=poscol1,xtit=' ',xtickformat='(A1)',color=255,framecolor=255,$
         ytit='MS Latitude',charsize=charsize,tit='MS HI Column Density (10!u18!n atoms/cm!u2!n; -386 < V!dLSR!n < -291 km/s)',maskv=0,/interp
;oplot,ml_bound,mb_bound,linestyle=2,co=255,thick=2 ;1
xyouts,-134.5,19,'(a)',charsize=charsize,co=255,align=0.5

; Overplot BT04 boundary, at DEC=+50.0
bt04_ra = scale_vector(findgen(200),-20,340)
bt04_dec = fltarr(200)+50.0
glactc,bt04_ra,bt04_dec,2000.,bt04_gl,bt04_gb,1,/deg
gal2mag,bt04_gl,bt04_gb,bt04_ml,bt04_mb,/wrap
;adxy,head,bt04_ml,bt04_mb,bt04_x,bt04_y
oplot,bt04_ml,bt04_mb,thick=5,color=255,linestyle=2
xyouts,-134.5,18.3,'Edge of',align=0,charsize=1.5,charthick=4,color=255
xyouts,-134.5,17.7,'BT04 Survey',align=0,charsize=1.5,charthick=4,color=255

; Overplot Galactic midplane
;gl_midplane = scale_vector(findgen(200),0,359)
;gb_midplane = fltarr(200)+0.0
;gal2mag,gl_midplane,gb_midplane,ml_midplane,mb_midplane,/wrap
;oplot,ml_midplane,mb_midplane,thick=10,color=250
;xyouts,-150.9,18.0,'GALACTIC',align=0.5,charsize=1.9,charthick=5,color=250
;xyouts,-150.9,17.5,'MIDPLANE',align=0.5,charsize=1.9,charthick=5,color=250

;; New MS detections
;xyouts,-146.3,11.9,'New MS Detections',align=0.5,charsize=2.0,charthick=5,color=255
;;arrow,-146,12.5,-142,13.5,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;;arrow,-146,12.5,-145.5,13.5,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;arrow,-147.5,12.4,-146.5,13.4,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;arrow,-147.5,12.4,-150.6,14.1,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;tvellipse,3.8,2.1,-144,14.6,-15,color=255,/data
;tvellipse,0.8,0.8,-151.2,14.7,0,color=255,/data

xr = reverse(minmax(mlon))
yr = minmax(mlat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim1,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'

; Vlsr vs. MLON
displayc,imtot2b[*,ind1:ind2],mlon,vel[ind1:ind2],min=0.001,max=1.0,/log,/xflip,posim=posim2,poscol=poscol2,xtit='MS Longitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit=' ',colformat='(F6.4)',color=255,framecolor=255,$
         coltickname=['0.001','0.01','0.1','1'],maskv=0,/noerase,/nocolorbar
xyouts,-134.5,-260,'(b)',charsize=charsize,co=255,align=0.5

; Overplot velocity range
oplot,[-120,-170],[0,0]+vel[zlo],linestyle=1,color=255
oplot,[-120,-170],[0,0]+vel[zhi],linestyle=1,color=255
;xyouts,-149,-280,'MS Velocity Range Used',align=0.5,charsize=1.5,charthick=5,color=255
xyouts,-149.5,-405,'MS Velocity Range Used',align=0.5,charsize=1.5,charthick=5,color=255

; CHVC labels
xyouts,-137,-430,'CHVC 402',align=0.5,charsize=1.2,charthick=4,color=255
xyouts,-143.4,-450,'CHVC 391',align=0.5,charsize=1.2,charthick=4,color=255

; MS Velocity Fiducial curve, two polynomials
restore,'/net/halo/dln5q/gbt/make_plots_mag_velfiducial2.dat'
g1 = where(mlon lt -115.7) ;-116.0, -115.7, -117
vfid = poly(mlon,coef3)
vfid[g1] = poly(mlon[g1],coef4b)
;oplot,mlon,vfid,co=255,thick=2
;oplot,mlon,vfid,co=0,thick=2,linestyle=2
oplot,mlon,vfid,co=255,thick=5,linestyle=2

xyouts,-139,-280,'MS Velocity Fiducial',align=0.5,charsize=1.5,charthick=4,co=255
arrow,-139.0,-285,-139.8,-352,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255

xr = reverse(minmax(mlon))
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim2,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)',xticklen=0.04
ps_close
ps2gif,file+'.eps',/eps




stop


; Velocity vs. MLAT
tot1 = total(bigcube,1)
tot1 = reverse(tot1,2)  ; invert

imtot1 = tot1*step  ; multiply by angular scale

file = plotdir+'grid_mstip_mag_velmlat_all'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=8.5,ysize=7.0
;device,/cmyk
;displayc,tot1,mlat,vel,min=-0.1,max=10,posim=posim,poscol=poscol,xtit='MS Latitude',$
displayc,imtot1[*,ind1:ind2],mlat,vel[ind1:ind2],min=0.0,max=0.7,posim=posim,poscol=poscol,xtit='MS Latitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='All Longitudes'
xr = minmax(mlat)
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps

stop

; Sigma cut
tot1b = tot1*0.0
for i=0,nchan-1 do begin
  slice = reform(bigcube[*,*,i])
  slice = slice*float(slice gt 3.0*rmsmap)
  totslice = total(slice,1)
  tot1b[*,i] = totslice
end
tot1b = reverse(tot1b,2)  ; invert
vel2 = reverse(vel)

imtot1b = tot1b*step   ; multiply by angular scale

file = plotdir+'grid_mstip_mag_velmlat_sigcut'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=8.5,ysize=7.0
;device,/cmyk
;displayc,tot1b,mlat,vel,min=0.01,max=10,/log,posim=posim,poscol=poscol,xtit='MS Latitude',$
displayc,imtot1b[*,ind1:ind2],mlat,vel2[ind1:ind2],min=0.001,max=1.0,/log,posim=posim,poscol=poscol,xtit='MS Latitude',$
         ytit='V!dLSR!n (km s!u-1!n)',charsize=charsize,tit='All Longitudes',maskv=0,colformat='(F8.4)',$
         coltickname=['0.001','0.01','0.1','1']
xyouts,-50.0,420,'(b)',charsize=charsize,co=255,align=0.5
xr = minmax(mlat)
yr = minmax(vel[ind1:ind2])
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'
ps_close
ps2gif,file+'.eps',/eps


stop

end
