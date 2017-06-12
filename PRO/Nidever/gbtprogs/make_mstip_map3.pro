pro make_mstip_map3,bigcube,head

; Make MS plots from the BIGCUBE
dir = '/net/halo/dln5q/gbt/'

; The RMS for the Arecibo and GBT surveys is about 0.01 K
; similar to Bruens I think
; RMS for Braun is 0.0004 K, about 20x lower

setdisp

goto,starthere

if n_elements(bigcube) eq 0 or n_elements(head) eq 0 then $
  FITS_READ,dir+'data/combine_all_final_mag_lowres.fits',bigcube,head
head = headfits(dir+'data/combine_all_final_mag_lowres.fits')
FITS_READ,dir+'data/combine_all_final_mag_lowres_rmsmap.fits',rmsmap,head2


fits_arrays,head,mlon,mlat,vel
mlon = mlon-360.0
dvel = vel[1]-vel[0]
step = sxpar(head,'CDELT1')

sz = size(bigcube)
nx = sz[1]
ny = sz[2]
nchan = sz[3]

;xx = findgen(nx)#(fltarr(ny)+1.0)
;yy = (fltarr(nx)+1.0)#findgen(ny)
;xyad,head,xx,yy,ra,dec
;xx2 = reverse(xx,1)


;dum = findgen(nx,ny)
;displayc,dum,out_posim=out_posim,out_poscol=out_poscol
;posim = [0.08, 0.10, 0.95, 0.85]
;poscol = [0.08, 0.92, 0.95, 0.95]
;posim = [0.08, 0.10, 0.92, 0.85]
;poscol = [0.08, 0.92, 0.92, 0.95]
;posim = [0.10, 0.08, 0.92, 0.87]
;poscol = [0.10, 0.95, 0.92, 0.98]
posim = [0.10, 0.08, 0.92, 0.86]
poscol = [0.10, 0.96, 0.92, 0.985]

; Load the Westmeier sources
;fieldnames=['Num','RAS','DECS','GLON','GLAT','VLSR']
;wstr = importascii('data/westmeier_catalog.txt',fieldnames=fieldnames,skip=3)
;add_tag,wstr,'RA',0.0d0,wstr
;add_tag,wstr,'DEC',0.0d0,wstr
;wstr.ra = sexig2ten(wstr.ras)*15.0d0
;wstr.dec = sexig2ten(wstr.decs)
;save,wstr,file='data/westmeier_catalog.dat'
restore,dir+'data/westmeier_catalog.dat'
;adxy,head,wstr.ra,wstr.dec,wx,wy
gal2mag,wstr.glon,wstr.glat,ml,mb,wrap=0
adxy,head,ml,mb,wx,wy

; Load the LAB data at DEC=+60
; load the FITS files or the Gaussians??
; The CHVC is at GLON=119.0, GLAT=-3.5,-3.0  just two positions
;restore,'data/ms_chvc.dat'

; I wonder if this is a galaxy because the peak T(K) is ~1 K
; YEP, it's IC10, it's part of the M31 group at a distance of ~830 kpc
; and it's not in the de Heij et al.(2002) paper on CHVCs.
; But that paper has two CHVCs at DEC=+70.

; Add two de Heij et al. (2002) CHVC clouds
restore,dir+'data/deheij02.dat'
;ind = [229,236,303,306,331,352,390,401,411,413,420,422,424,465,467,508,520,532]
;ind = [236,229,303,306,390,401]  ; 236,229,306 don't seem real in LAB data
; 303,390, 401 definitely real in LAB data
;  229 has CRAZY wide velocity FWHM
ind = [236,303,306,390,401]
;ind = [401,390]  ;,423]
chvc = arr[ind]
nind = n_elements(ind)

; Add to bigcube
;  represent each point as a spatial Gaussian with FWHM=36 arcmin
mlon2 = mlon#(fltarr(ny)+1.0)
mlat2 = (fltarr(nx)+1.0)#mlat
;sig = (36.0/60.0)/ 2.35482
for i=0,nind-1 do begin

  dum = closest(chvc[i].mlon,mlon,ind=xmid)
  dum = closest(chvc[i].mlat,mlat,ind=ymid)


  ;gmap = exp( -0.5*((mlon2-chvc[i].mlon)^2 + (mlat2-chvc[i].mlat)^2)/sig^2 )
  sig = sqrt( chvc[i].fwhm_maj^2 + chvc[i].fwhm_min^2)/2.35482
  xlo = min(where(mlon ge chvc[i].mlon-3.0*sig))
  xhi = max(where(mlon le chvc[i].mlon+3.0*sig))
  ylo = min(where(mlat ge chvc[i].mlat-3.0*sig))
  yhi = max(where(mlat le chvc[i].mlat+3.0*sig))
  mlon2b = mlon2[xlo:xhi,ylo:yhi]
  mlat2b = mlat2[xlo:xhi,ylo:yhi]
  gmap2 = exp( -0.5*((mlon2b-chvc[i].mlon)^2 + (mlat2b-chvc[i].mlat)^2)/sig^2 )

  par = [chvc[i].ptb, chvc[i].lrv, chvc[i].fwhm/2.35482]
  gf = gfunc(vel,par)

  ; bigcube hasn't been modified yet
  if total(bigcube[xmid,ymid,*]) lt 0.0001 then begin

    print,'Adding in de Heij CHVC ',strtrim(long(chvc[i].seq),2)

    for j=0,nchan-1 do $
      bigcube[xlo:xhi,ylo:yhi,j] = bigcube[xlo:xhi,ylo:yhi,j] + gmap2*gf[j]

  endif

  ; Update RMS map
  rmsmap[xlo:xhi,ylo:yhi] = 0.001

  ; Now normalize it
  ;tot = garea(chvc[i].par)
  ;totb = total(bigcube[xlo:xhi,ylo:yhi,*])*dvel
  ;bigcube[xlo:xhi,ylo:yhi,*] = bigcube[xlo:xhi,ylo:yhi,*]*tot/totb

  ;stop
end


;goto,SKIP1

; Set up the coordinate array.
ralevels = [0.0,1.0,2.0,3.0,20.0,21.0,22.0,23.0]
;ralevels = [0.0,1.0,2.0,3.0,4.0,20.0,21.0,22.0,23.0]
;ralevels = [0.0,0.5,1.0,1.5,2.0,2.5,3.0,21.5,22.0,22.5,23.0,23.5]
declevels = [-20.0,0.0,20.0,40.0,60.0]
;ralevels = [4.0,4.5,5.0,5.5,6.0,6.5,7.0]
;declevels = [-74.0,-72.0,-70.0,-68.0,-66.0,-64.0,-62.0]
;position = out_posim
ratickn = stringize(ralevels,ndec=0)
dectickn = stringize(declevels,ndec=0)
;ratickn[4] = ' '
;dectickn[4] = ' '
;fits_grid,head,ralev=ralevels,declev=declevels,position=position,thick=1,$
;          xtit='Right Ascension',ytit='Declination',xtickn=xtickn,ytickn=ytickn,$
;          framecolor=co
;position = [0,0,1,1]


charsize = 1.4

;goto,SKIP0

; Column density
print,'Summing in vel'
tot3 = total(bigcube,3)

;charsize=1.2
;displayc,tot3,min=-1,max=10,/log,/xflip,posim=posim,poscol=poscol,xtickformat='(A1)',$
;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize
;oplot,wx,wy,ps=1,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize
;AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize
;fits_grid,head,lonlev=ralevels,latlev=declevels,thick=1,$
;          lontit='Right Ascension',lattit='Declination',lontickn=ratickn,lattickn=dectickn,$
;          framecolor=255,color=255,/xflip,charsize=charsize,/celestial,/translabel,/lonlabelsflip,$
;          /latlabelsflip,/stp ;position=posim
;stop

col3 = tot3 * dvel * 1.83d18 / 1d18   ; column density

charsize = 1.4


; Column density only with 3*sigma points
;tot3b = tot3*0.
;for i=0,nchan-1 do begin
;  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
;  slice = reform(bigcube[*,*,i])
;  tot3b = tot3b + slice*float(slice gt 3.0*rmsmap)
;end
;
;col3b = tot3b * dvel * 1.83d18 / 1d18   ; column density
;
;stop

; Column density with MS velocity cut
;tot3c = tot3*0.
;x = findgen(nx)
;z = findgen(nchan)
;mask = fltarr(nx,nchan)
;xx = findgen(nx)#(fltarr(nchan)+1.0)
;yy = (fltarr(nx)+1.0)#findgen(nchan)
;restore,'make_plots_mag_mlonvelcut3.dat'
;roi_cut,xcut,ycut,xx,yy,ind,cutind
;mask[cutind]=1.0
;for i=0,ny-1 do begin
;  if (i+1) mod 200 eq 0 then print,strtrim(i+1,2),'/',strtrim(ny,2)
;  slice = reform(bigcube[*,i,*])
;  slice = slice*mask
;  totslice = total(slice,2)  ; sum in vel
;  tot3c[*,i] = totslice
;  ;tot3c = tot3c + slice*mask
;end
;
;col3c = tot3c * dvel * 1.83d18 / 1d18   ; column density



; Column density with MS velocity cut and 3*sigma points
;tot3d = fltarr(nx,ny)
;x = findgen(nx)
;z = findgen(nchan)
;mask = fltarr(nx,nchan)
;xx = findgen(nx)#(fltarr(nchan)+1.0)
;yy = (fltarr(nx)+1.0)#findgen(nchan)
;restore,'make_plots_mag_mlonvelcut3.dat'
;roi_cut,xcut,ycut,xx,yy,ind,cutind
;mask[cutind]=1.0
;for i=0,ny-1 do begin
;  if (i+1) mod 200 eq 0 then print,strtrim(i+1,2),'/',strtrim(ny,2)
;  slice = reform(bigcube[*,i,*])
;  slicerms = reform(rmsmap[*,i])#(fltarr(nchan)+1.0)
;  slice = slice*mask
;  slice = slice*float(slice gt 3.0*slicerms)
;  totslice = total(slice,2)  ; sum in vel
;  tot3d[*,i] = totslice
;  ;tot3c = tot3c + slice*mask
;end

restore,dir+'cube_boundary_final.dat'

;col3d = tot3d * dvel * 1.83d18 / 1d18   ; column density
;col3d = col3d > 1d-10
;logcol3d = alog10(col3d*1d18)

;file = 'plots/ms_mag_lowres_coldens_velsigcut'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=8.5,ysize=8.4
;device,/cmyk
;;displayc,col3d,min=0.03,max=200,/log,/xflip,posim=posim,poscol=poscol,xtickformat='(A1)',$
;;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(G7.3)'
;displayc,logcol3d,min=15.5,max=20.2,/xflip,posim=posim,poscol=poscol,xtickformat='(A1)',$
;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(I3)',$
;         coltickvalue=[16,17,18,19,20],coldivisions=4
;oplot,xbound2,ybound2,linestyle=2,co=255,thick=1
;oplot,wx,wy,ps=1,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize,co=1,ys=1
;fits_grid,head,lonlev=ralevels,latlev=declevels,thick=2,$
;          lontit='Right Ascension (h)',lattit='Declination',lontickn=ratickn,lattickn=dectickn,$
;          framecolor=1,color=255,/xflip,charsize=charsize,/celestial,/translabel,/lonlabelsflip,$
;          /latlabelsflip,linestyle=1
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xtickformat='(A1)',xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit=' ',charsize=charsize,co=255,ytickformat='(A1)',ys=1
;ps_close
;ps2gif,file+'.eps',/eps

;fits_write,dir+'ms_mag_lowres_coldens_velsigcut.fits',col3d,head

fits_read,dir+'data/ms_mag_lowres_coldens_velsigcut.fits',col3d,head
col3d = col3d > 1d-10
logcol3d = alog10(col3d*1d18)


; The W&K 2008 survey covers 0<RA<1:30, -30<DEC<+25 deg
; make a line for 1:30
rawk_bound = dblarr(100)+1.5*15.0
decwk_bound = scale_vector(dindgen(100),-20.0,19.0)
glactc,rawk_bound,decwk_bound,2000.0,glwk_bound,gbwk_bound,1,/deg
gal2mag,glwk_bound,gbwk_bound,mlwk_bound,mbwk_bound,wrap=0
adxy,head,mlwk_bound,mbwk_bound,xwk_bound,ywk_bound


;; Galactic coordinate overlaid
;posim2 = [0.10, 0.08, 0.915, 0.86]
;poscol2 = [0.10, 0.96, 0.915, 0.985]
;file = 'plots/ms_mag_lowres_coldens_velsigcut_galover'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=8.5,ysize=8.35
;device,/cmyk
;;displayc,col3d,min=0.03,max=200,/log,/xflip,posim=posim2,poscol=poscol2,xtickformat='(A1)',$
;;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(G7.3)'
;displayc,logcol3d,min=15.5,max=20.2,/xflip,posim=posim2,poscol=poscol2,xtickformat='(A1)',$
;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(I3)',$
;         coltickvalue=[16,17,18,19,20],coldivisions=4
;oplot,xbound2,ybound2,linestyle=2,co=255,thick=1
;oplot,xwk_bound,ywk_bound,linestyle=4,co=255,thick=1
;oplot,wx,wy,ps=1,co=255
;
;; east arm line
;;x1 = findgen(1261)+152
;;oplot,x1,-0.293*x1+883.,co=0,thick=5
;;oplot,x1,-0.293*x1+883.,linestyle=2,co=255,thick=5
;
;xyouts,1644,1510,'(b)',align=0,charsize=1.4,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize,co=1,ys=1
;
;lonlevels = [40,60,80,100,120,140,160,180]
;latlevels = [-80,-60,-40,-20,0,20]
;lontickn = stringize(lonlevels,ndec=0)
;lattickn = stringize(latlevels,ndec=0)
;fits_grid,head,lonlev=lonlevels,latlev=latlevels,thick=2,$
;          lontit='Galactic Longitude',lattit='Galactic Latitude',lontickn=lontickn,lattickn=lattickn,$
;          framecolor=1,color=255,/xflip,charsize=charsize,/galactic,/translabel,/lonlabelsflip,$
;          /latlabelsflip,linestyle=1
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xtickformat='(A1)',xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit=' ',charsize=charsize,co=255,ytickformat='(A1)',ys=1
;ps_close
;ps2gif,file+'.eps',/eps


; Extend the map out to include the third de Heij CHVC
nextend = 500L ;300L 
chvc2 = arr[423]
col3e = fltarr(nx+nextend,ny)
col3e[nextend:*,*] = col3d
mlon2 = [ (findgen(nextend)-nextend)*step+mlon[0], mlon]
;displayc,tot3e,mlon2,mlat,min=0.01,max=10,/log,/xflip
;oplot,[dum.mlon],[dum.mlat],ps=1
;stop

; Add to bigcube
;  represent each point as a spatial Gaussian with FWHM=36 arcmin
mlon3 = mlon2#(fltarr(ny)+1.0)
mlat3 = (fltarr(nx+nextend)+1.0)#mlat

sig = sqrt( chvc2.fwhm_maj^2 + chvc2.fwhm_min^2)/2.35482
xlo = min(where(mlon2 ge chvc2.mlon-3.0*sig))
xhi = max(where(mlon2 le chvc2.mlon+3.0*sig))
ylo = min(where(mlat ge chvc2.mlat-3.0*sig))
yhi = max(where(mlat le chvc2.mlat+3.0*sig))
mlon3b = mlon3[xlo:xhi,ylo:yhi]
mlat3b = mlat3[xlo:xhi,ylo:yhi]
gmap2 = exp( -0.5*((mlon3b-chvc2.mlon)^2 + (mlat3b-chvc2.mlat)^2)/sig^2 )

par = [chvc2.ptb, chvc2.lrv, chvc2.fwhm/2.35482]
gf = gfunc(vel,par)
area = garea(par)
colarea = area *  1.83d18 / 1d18   ; column density

gmap3 = gmap2*colarea

col3e[xlo:xhi,ylo:yhi] = gmap3

col3e = col3e > 1d-10
logcol3e = alog10(col3e*1d18)

head2 = head
sxaddpar,head2,'CRPIX1',sxpar(head,'CRPIX1')+nextend
sxaddpar,head2,'NAXIS1',sxpar(head,'NAXIS1')+nextend



; tighter stretch to bring out the faint features
;file = 'plots/ms_mag_lowres_coldens_velsigcut3'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=10.2,ysize=8.4
;device,/cmyk
;;displayc,col3e,min=0.01,max=5,/log,/xflip,posim=posim,poscol=poscol,xtickformat='(A1)',$
;;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,mask=0
;displayc,logcol3e,min=15.5,max=20.2,/xflip,posim=posim2,poscol=poscol2,xtickformat='(A1)',$
;         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(I3)',$
;         coltickvalue=[16,17,18,19,20],coldivisions=4
;oplot,xbound2+300,ybound2,linestyle=2,co=255,thick=1
;oplot,wx+300,wy,ps=1,co=255
;
;
;; Eastern arm line
;x1 = findgen(1691)+22
;
;ll = findgen(360)
;bb = fltarr(360)
;;rotsph,ll,bb,204.85,-16.30,ll2,bb2,/reverse
;rotsph,ll,bb,205.20,-15.40,ll2,bb2,/reverse
;gal2mag,ll2,bb2,ml2,mb2,wrap=0
;adxy,head2,ml2,mb2,x2,y2
;g2 = where(x2 ge 0 and x2 le 2000 and y2 ge 0 and y2 le 1600,ng2)
;oplot,x2[g2],y2[g2],co=0,thick=5
;oplot,x2[g2],y2[g2],co=255,thick=5,linestyle=2
;
;; Western arm line
;ll = findgen(360)
;bb = fltarr(360)
;rotsph,ll,bb,189.90,-15.70,ll3,bb3,/reverse
;gal2mag,ll3,bb3,ml3,mb3,wrap=0
;adxy,head2,ml3,mb3,x3,y3
;g3 = where(x3 ge 0 and x3 le 2000 and y3 ge 0 and y3 le 1600,ng3)
;;oplot,x3[g3],y3[g3],co=0,thick=5
;;oplot,x3[g3],y3[g3],co=255,thick=5,linestyle=2
;
;ralevels2 = [0.0,1.0,2.0,3.0,4.0,19.0,20.0,21.0,22.0,23.0]
;declevels2 = [-20.0,0.0,20.0,40.0,60.0]
;ratickn2 = stringize(ralevels2,ndec=0)
;dectickn2 = stringize(declevels2,ndec=0)
;
;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize,co=1,ys=1
;fits_grid,head2,lonlev=ralevels2,latlev=declevels2,thick=2,$
;          lontit='Right Ascension (h)',lattit='Declination',lontickn=ratickn2,lattickn=dectickn2,$
;          framecolor=1,color=255,/xflip,charsize=charsize,/celestial,/translabel,/lonlabelsflip,$
;          /latlabelsflip,linestyle=1
;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xtickformat='(A1)',xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit=' ',charsize=charsize,co=255,ytickformat='(A1)',ys=1
;ps_close
;ps2gif,file+'.eps',/eps


;------------------------
; Velocity vs. MLON
;------------------------
SKIP1:
charsize = 1.4

posim = [0.12, 0.11, 0.98, 0.85]
poscol = [0.12, 0.96, 0.98, 0.985]

; Fiducial curve, two polynomials
;restore,'make_plots_mag_velfiducial.dat'
;g1 = where(mlon gt -110)
;vfid = poly(mlon,coef1)  ; coef3
;vfid[g1] = poly(mlon[g1],coef2)
restore,dir+'make_plots_mag_velfiducial2.dat'
g1 = where(mlon lt -115.7) ;-116.0, -115.7, -117
vfid = poly(mlon,coef3)
vfid[g1] = poly(mlon[g1],coef4b)
;oplot,mlon,vfid,co=255,thick=2
;oplot,mlon,vfid,co=0,thick=2,linestyle=2

; All MLAT
tot2 = total(bigcube,2)
;XYAD,head,fltarr(ny)+620,findgen(ny),ra1,dec1

tot2a = tot2*0.0
for i=0,nchan-1 do begin
  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
  slice = reform(bigcube[*,*,i])
  slice = slice*float(slice gt 3.0*rmsmap)
  totslice = total(slice,2)  ; sum in Y/MLAT
  tot2a[*,i] = totslice
end

; Save the image
head3 = head
sxaddpar,head3,'CDELT2',sxpar(head3,'CDELT3')
sxaddpar,head3,'CRPIX2',sxpar(head3,'CRPIX3')
sxaddpar,head3,'CRVAL2',sxpar(head3,'CRVAL3')
sxaddpar,head3,'CTYPE2',sxpar(head3,'CTYPE3')
sxaddpar,head3,'NAXIS2',sxpar(head3,'NAXIS3')
sxaddpar,head3,'NAXIS',2
sxdelpar,head3,'NAXIS3'
sxdelpar,head3,'CDELT3'
sxdelpar,head3,'CRPIX3'
sxdelpar,head3,'CRVAL3'
sxdelpar,head3,'CTYPE3'
;fits_write,dir+'ms_mag_lowres_velmlon_sigcut.fits',tot2a,head3

gal2mag,wstr.glon,wstr.glat,ml,mb,wrap=0
bd = where(ml gt 180.,nbd)
if nbd gt 0 then ml[bd]=ml[bd]-360.0

imtot2a = tot2a * step  ; multiply by the angular size

;file = 'plots/ms_mag_lowres_velmlon_all'
;ps_open,file,/color,thick=5,/encap
;;device,/inches,xsize=8.5,ysize=6
;device,/inches,xsize=8.5,ysize=6.3
;device,/cmyk
;;displayc,tot2,mlon,vel,min=-1,max=10,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;displayc,tot2a,mlon,vel,min=0.01,max=50,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;         tit='All B!dMS!n',/xflip,charsize=charsize,posim=posim,poscol=poscol
;displayc,imtot2a,mlon,vel,min=0.001,max=5,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;         tit='All B!dMS!n',/xflip,charsize=charsize,posim=posim,poscol=poscol,mask=0,colformat='(F6.3)',$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;;AXIS,yaxis=0,yr=minmax(vel),ytit='V!dLSR!n',charsize=charsize,co=1,ys=1
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;oplot,ml,wstr.vlsr,ps=1,co=255
;ps_close
;ps2gif,file+'.eps',/eps

;stop

; Various slices or MLAT ranges
; Need to use MLON/MLAT cuts


;tot2b = total(bigcube[*,0:572,*],2)
;;tot1b = total(bigcube[200:600,*,*],1)
;;XYAD,head,fltarr(ny)+400,findgen(ny),ra2,dec2

; west
tot2b = tot2*0.0
x = findgen(nx)
y = findgen(ny)
mask2b = fltarr(nx,ny)
xx = findgen(nx)#(fltarr(ny)+1.0)
yy = (fltarr(nx)+1.0)#findgen(ny)
restore,dir+'make_plots_mag_mlonmlatcutwest.dat'
roi_cut,xcut,ycut,xx,yy,ind,cutind
mask2b[cutind]=1.0
for i=0,nchan-1 do begin
  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
  slice = reform(bigcube[*,*,i])
  ;slicerms = reform(rmsmap[*,i])#(fltarr(ny)+1.0)
  slice = slice*float(slice gt 3.0*rmsmap)
  slice = slice*mask2b
  totslice = total(slice,2)  ; sum in Y/MLAT
  tot2b[*,i] = totslice
  ;tot3c = tot3c + slice*mask
end

;tot

;tot2b = tot2b*dvel  ; in units of K*km/s

roi_cut,xcut,ycut,wx,wy,wind,wcutind2b

imtot2b = tot2b * step  ; multiply by angular step

;file = 'plots/ms_mag_lowres_velmlon_west'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=8.5,ysize=6.3
;device,/cmyk
;;displayc,tot2b,mlon,vel,min=-0.5,max=3,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;displayc,tot2b,mlon,vel,min=0.01,max=20,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;         tit='West',/xflip,charsize=charsize,posim=posim,poscol=poscol
;displayc,imtot2b,mlon,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;         tit='West',/xflip,charsize=charsize,posim=posim,poscol=poscol,mask=0,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;;AXIS,yaxis=0,yr=minmax(vel),ytit='V!dLSR!n',charsize=charsize,co=1,ys=1
;;oplot,ml,wstr.vlsr,ps=1,co=255
;oplot,ml[wcutind2b],wstr[wcutind2b].vlsr,ps=1,co=255
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;ps_close
;ps2gif,file+'.eps',/eps

;stop

;; west 2 include all the western emission
tot2b2 = tot2*0.0
x = findgen(nx)
y = findgen(ny)
mask2b2 = fltarr(nx,ny)
xx = findgen(nx)#(fltarr(ny)+1.0)
yy = (fltarr(nx)+1.0)#findgen(ny)
restore,dir+'make_plots_mag_mlonmlatcutwest2.dat'
roi_cut,xcut,ycut,xx,yy,ind,cutind
mask2b[cutind]=1.0
for i=0,nchan-1 do begin
  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
  slice = reform(bigcube[*,*,i])
  ;slicerms = reform(rmsmap[*,i])#(fltarr(ny)+1.0)
  slice = slice*float(slice gt 3.0*rmsmap)
  slice = slice*mask2b
  totslice = total(slice,2)  ; sum in Y/MLAT
  tot2b2[*,i] = totslice
  ;tot3c = tot3c + slice*mask
end

;tot

;tot2b2 = tot2b2*dvel  ; in units of K*km/s

imtot2b2 = tot2b2*step  ; multiply by angular step

roi_cut,xcut,ycut,wx,wy,wind,wcutind

;file = 'plots/ms_mag_lowres_velmlon_west2'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=8.5,ysize=6.3
;device,/cmyk
;;displayc,tot2b,mlon,vel,min=-0.5,max=3,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;displayc,tot2b2,mlon,vel,min=0.01,max=20,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;         tit='West',/xflip,charsize=charsize,posim=posim,poscol=poscol
;displayc,imtot2b2,mlon,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;         tit='West',/xflip,charsize=charsize,posim=posim,poscol=poscol,mask=0,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;;AXIS,yaxis=0,yr=minmax(vel),ytit='V!dLSR!n',charsize=charsize,co=1,ys=1
;;oplot,ml,wstr.vlsr,ps=1,co=255
;oplot,ml[wcutind],wstr[wcutind].vlsr,ps=1,co=255
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;ps_close
;ps2gif,file+'.eps',/eps
;
;stop

; center
;tot2c = total(bigcube[*,572:850,*],2)
;;tot1c = total(bigcube[600:800,*,*],1)
;;XYAD,head,fltarr(ny)+700,findgen(ny),ra3,dec3

tot2c = tot2*0.0
x = findgen(nx)
y = findgen(ny)
mask2c = fltarr(nx,ny)
xx = findgen(nx)#(fltarr(ny)+1.0)
yy = (fltarr(nx)+1.0)#findgen(ny)
restore,dir+'make_plots_mag_mlonmlatcutcenter.dat'
roi_cut,xcut,ycut,xx,yy,ind,cutind
mask2c[cutind]=1.0
for i=0,nchan-1 do begin
  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
  slice = reform(bigcube[*,*,i])
  ;slicerms = reform(rmsmap[*,i])#(fltarr(ny)+1.0)
  slice = slice*float(slice gt 3.0*rmsmap)
  slice = slice*mask2c
  totslice = total(slice,2)  ; sum in Y/MLAT
  tot2c[*,i] = totslice
  ;tot3c = tot3c + slice*mask
end

;tot2c = tot2c*dvel  ; in units of K*km/s

roi_cut,xcut,ycut,wx,wy,wind,wcutind2c

imtot2c = tot2c*step   ; multiply by angular step

;file = 'plots/ms_mag_lowres_velmlon_center'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=8.5,ysize=6.3
;;;displayc,tot2c,mlon,vel,min=-0.5,max=3,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;displayc,tot2c,mlon,vel,min=0.01,max=20,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;         tit='Center',/xflip,charsize=charsize,posim=posim,poscol=poscol
;displayc,imtot2c,mlon,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;         tit='Center',/xflip,charsize=charsize,posim=posim,poscol=poscol,mask=0,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;;AXIS,yaxis=0,yr=minmax(vel),ytit='V!dLSR!n',charsize=charsize,co=1,ys=1
;oplot,ml[wcutind2c],wstr[wcutind2c].vlsr,ps=1,co=255
;;oplot,ml,wstr.vlsr,ps=1,co=255
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;ps_close
;ps2gif,file+'.eps',/eps



; Center 2, Extend the map out to include the third de Heij CHVC
chvc2 = arr[423]
tot2c2 = fltarr(nx+nextend,nchan)
tot2c2[nextend:*,*] = tot2c
mlon2 = [ (findgen(nextend)-nextend)*step+mlon[0], mlon]

mlon3 = mlon2#(fltarr(ny)+1.0)
mlat3 = (fltarr(nx+nextend)+1.0)#mlat

sig = sqrt( chvc2.fwhm_maj^2 + chvc2.fwhm_min^2)/2.35482
xlo = min(where(mlon2 ge chvc2.mlon-3.0*sig))
xhi = max(where(mlon2 le chvc2.mlon+3.0*sig))
ylo = min(where(mlat ge chvc2.mlat-3.0*sig))
yhi = max(where(mlat le chvc2.mlat+3.0*sig))
mlon3b = mlon3[xlo:xhi,ylo:yhi]
mlat3b = mlat3[xlo:xhi,ylo:yhi]
gmap2 = exp( -0.5*((mlon3b-chvc2.mlon)^2 + (mlat3b-chvc2.mlat)^2)/sig^2 )

par = [chvc2.ptb, chvc2.lrv, chvc2.fwhm/2.35482]
gf = gfunc(vel,par)

; make small cube
snx = xhi-xlo+1
sny = yhi-ylo+1
scube = fltarr(snx,sny,nchan)
for i=0,nchan-1 do scube[*,*,i] =  gmap2*gf[i]
stot = total(scube,2)  ; sum in mlat
tot2c2[xlo:xhi,*] = tot2c2[xlo:xhi,*] + stot

imtot2c2 = tot2c2*step   ; multiply by angular step

;displayc,tot2c2,mlon2,vel,min=0.01,max=10,/log,/xflip

; getting second velocity fiducial curve
mag2gal,mlon2,mlon2*0.0+11.6,glon,glat
vfid2 = vgsr2vlsr(260.*sin((mlon2+40.0)/!radeg),glon,glat,1)

;file = 'plots/ms_mag_lowres_velmlon_center2'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=10.2,ysize=6.3
;;displayc,tot2c,mlon,vel,min=-0.5,max=3,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;displayc,tot2c2,mlon2,vel,min=0.01,max=20,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km/s)',$
;;         tit='Center',/xflip,charsize=charsize,posim=posim,poscol=poscol
;displayc,imtot2c2,mlon2,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km s!u-1!n)',$
;         tit='Center',/xflip,charsize=charsize,posim=posim,poscol=poscol,mask=0,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3
;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit='L!dMS!n',charsize=charsize,co=1,xs=1
;;AXIS,yaxis=0,yr=minmax(vel),ytit='V!dLSR!n',charsize=charsize,co=1,ys=1
;oplot,ml[wcutind],wstr[wcutind].vlsr,ps=1,co=255
;;oplot,ml,wstr.vlsr,ps=1,co=255
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;oplot,mlon2,vfid2,co=255
;oplot,mlon2,vfid2,co=0,linestyle=2
;ps_close
;ps2gif,file+'.eps',/eps




;tot2d = tot2*0.0
;x = findgen(nx)
;y = findgen(ny)
;mask2d = fltarr(nx,ny)
;xx = findgen(nx)#(fltarr(ny)+1.0)
;yy = (fltarr(nx)+1.0)#findgen(ny)
;restore,'make_plots_mag_mlonmlatcuteast.dat'
;roi_cut,xcut,ycut,xx,yy,ind,cutind
;mask2d[cutind]=1.0
;for i=0,nchan-1 do begin
;  if (i+1) mod 50 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)
;  slice = reform(bigcube[*,*,i])
;  ;slicerms = reform(rmsmap[*,i])#(fltarr(ny)+1.0)
;  slice = slice*float(slice gt 3.0*rmsmap)
;  slice = slice*mask2d
;  totslice = total(slice,2)  ; sum in Y/MLAT
;  tot2d[*,i] = totslice
;  ;tot3c = tot3c + slice*mask
;end
;
;;tot2d = tot2d*dvel  ; in units of K*km/s
;
;roi_cut,xcut,ycut,wx,wy,wind,wcutind2d
;
;imtot2d = tot2d*step  ; multiply by angular step
;


; Vlsr vs. Mlon FOUR PANEL
;--------------------------------
;file = 'plots/ms_mag_lowres_velmlon_4panel'
;ps_open,file,/color,thick=2.5,/encap
;;device,/inches,xsize=4,ysize=10
;;device,/inches,xsize=8.5,ysize=25
;device,/inches,xsize=4.5,ysize=10
;
;charsize = 0.9 ;1.0 ;1.4
;sym = 0.7  ;0.8
;thk = 2.5
;y0 = 0.05
;dy = 0.20
;;x0 = 0.16
;x1 = 0.98
;
;; ALL
;displayc,imtot2a,mlon,vel,min=0.001,max=5,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km s!u-1!n)',$
;         tit=' ',/xflip,charsize=charsize,mask=0,colformat='(F6.3)',$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3,$
;         posim=[x0, y0+3*dy+0.09, x1, y0+4*dy+0.09],$
;         poscol=[x0, y0+4*dy+0.12, x1,  y0+4*dy+0.13]
;xyouts,-75.0,-140,'(a) All B!dMS!n',charsize=charsize,align=0.,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;oplot,ml,wstr.vlsr,ps=1,co=255,sym=sym,thick=thk
;
;; West
;displayc,imtot2b2,mlon,vel,min=0.0005,max=1,/log,xtit=' ',ytit='V!dLSR!n (km s!u-1!n)',xtickformat='(A1)',$
;         tit=' ',/xflip,charsize=charsize,mask=0,coltickname=['0.001','0.01','0.1','1'],/noerase,$
;         coltickval=[0.001,0.01,0.1,1.0],coldiv=3,posim=[x0, y0+2*dy, x1, y0+3*dy],$
;         poscol=[x0, y0+3*dy+0.03, x1, y0+3*dy+0.04]
;xyouts,-75.0,-140,'(b) West',charsize=charsize,align=0.,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;oplot,ml[wcutind2b],wstr[wcutind2b].vlsr,ps=1,co=255,sym=sym,thick=thk
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;
;; Center
;displayc,imtot2c,mlon,vel,min=0.0005,max=1,/log,xtit=' ',ytit='V!dLSR!n (km s!u-1!n)',xtickformat='(A1)',$
;         tit=' ',/xflip,charsize=charsize,mask=0,/nocolorbar,/noerase,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3,$
;         posim=[x0, y0+dy, x1, y0+2*dy]
;xyouts,-75.0,-140,'(c) Center',charsize=charsize,align=0.,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;oplot,ml[wcutind2c],wstr[wcutind2c].vlsr,ps=1,co=255,sym=sym,thick=thk
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;
;; East
;displayc,imtot2d,mlon,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km s!u-1!n)',$
;         tit=' ',/xflip,charsize=charsize,mask=0,/nocolorbar,/noerase,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3,$
;         posim=[x0, y0, x1, y0+dy]
;xyouts,-75.0,-140,'(d) East',charsize=charsize,align=0.,co=255
;AXIS,xaxis=0,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;oplot,ml[wcutind2d],wstr[wcutind2d].vlsr,ps=1,co=255,sym=sym,thick=thk
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
;
;ps_close
;ps2gif,file+'.eps',/eps


STARTHERE:

;save,logcol3e,head,imtot2c2,mlon2,mlon,vel,vfid,vfid2,mlat,file='make_mstip_map.dat'
;restore,'make_mstip_map.dat'

;save,nextend,logcol3e,head,imtot2c2,mlon2,mlon,vel,vfid,vfid2,mlat,file='make_mstip_map3.dat'
restore,'make_mstip_map3.dat'
restore,dir+'cube_boundary_final.dat'
;nextend = 500L

restore,dir+'data/westmeier_catalog.dat'
;adxy,head,wstr.ra,wstr.dec,wx,wy
gal2mag,wstr.glon,wstr.glat,ml,mb,wrap=0
adxy,head,ml,mb,wx,wy

head2 = head
sxaddpar,head2,'CRPIX1',sxpar(head,'CRPIX1')+nextend
sxaddpar,head2,'NAXIS1',sxpar(head,'NAXIS1')+nextend

restore,dir+'make_plots_mag_mlonmlatcutwest2.dat'
roi_cut,xcut,ycut,wx,wy,wind,wcutind

restore,dir+'gbt_survey_boundary.dat'
gal2mag,gl_bound,gb_bound,ml_bound,mb_bound,wrap=0
adxy,head2,ml_bound,mb_bound,xgbt_bound,ygbt_bound


;-------------------------------
; EXTENDED MS-TIP 2-PANEL PLOT
;-------------------------------
file = 'mstip_map3'
save = 0 ;1 ;0
bw = 1
if keyword_set(bw) then begin
  loadcol,9 ;3
  ;black = fsc_Color('black',255)
  ;loadct,0
  linco = 255
endif else begin
  loadct,39
  linco = 255
endelse

if keyword_set(save) then begin
  ps_open,file,/color,thick=3,/encap
  device,/inches,xsize=7.7,ysize=8.5
  ;device,/inches,xsize=7,ysize=8.5
  ;device,/inches,xsize=7,ysize=9.5
  ;device,/cmyk
endif
charsize = 1.2
;posim3a  = [0.125, 0.435, 0.915, 0.90]
;poscol3a = [0.125, 0.97, 0.915, 0.99]
;posim3b  = [0.125, 0.115, 0.915, 0.435]
;poscol3b = [0.125, 0.03, 0.915, 0.05]
poscol3a = [0.125, 0.97, 0.915, 0.99]
posim3a  = [0.125, 0.37, 0.915, 0.89]
posim3b  = [0.125, 0.065, 0.915, 0.37]
; 8.4 vs. 6.3, 1/3 larger
displayc,logcol3e,min=15.5,max=20.2,/xflip,posim=posim3a,poscol=poscol3a,xtickformat='(A1)',$
         ytickformat='(A1)',xticks=1,yticks=1,charsize=charsize,colformat='(I3)',$
         coltickvalue=[16,17,18,19,20],coldivisions=4
oplot,xgbt_bound,ygbt_bound,linestyle=0,co=255,thick=6
;oplot,wx+300,wy,ps=1,co=255

;xyouts,-70.0,50,'(a)',charsize=charsize,align=0.,co=255
xyouts,1930+200,1494,'(a)',charsize=charsize,align=0.,co=255

; Our previous GBT survey, GBT06A-066
oplot,xbound2+nextend,ybound2,linestyle=2,co=255,thick=3 ;2 ;1
xyouts,1500+200,970,'GBT06A-066',align=0.5,co=255,charsize=1.4,charthick=4
xyouts,1500+200,900,'survey',align=0.5,co=255,charsize=1.4,charthick=4
arrow,1450+200,870,1350+200,740,/data,hsize=!d.x_size/50,co=255,/solid,thick=7,hthick=1.5

; Overplot the spatial cut for the Vel vs. Mlon panel
restore,dir+'make_plots_mag_mlonmlatcutcenter.dat'
xcut2 = xcut+nextend ;300
ycut2 = ycut
xcut2[[16,17]] = -1
;ycut2[[16,17]] = [790,1200]
;ycut2[[16,17]] = [800,1300]
ycut2[[16,17]] = [750,1200]
;oplot,xcut2,ycut2,linestyle=3,co=250,thick=3

; de Heij CHVC 424
;xyouts,20,550,'de Heij',align=1,charsize=1.3,charthick=4.0,co=255
;xyouts,20,470,'CHVC 424',align=1,charsize=1.2,charthick=4.0,co=255
;arrow,60,620,20,800,/data,hsize=!d.x_size/50,co=255,/solid,thick=7,hthick=1.5
xyouts,20+200,590-20,'de Heij',align=1,charsize=1.2,charthick=4.0,co=255
xyouts,20+200,515-20,'CHVC 424',align=1,charsize=1.1,charthick=4.0,co=255
arrow,60+200+50,650-20,30+200+5,850,/data,hsize=!d.x_size/50,co=255,/solid,thick=7,hthick=1.5

; Eastern arm line
x1 = findgen(1691)+22
ll = findgen(360)
bb = fltarr(360)
rotsph,ll,bb,205.20,-15.40,ll2,bb2,/reverse
gal2mag,ll2,bb2,ml2,mb2,wrap=0
adxy,head2,ml2,mb2,x2,y2
g2 = where(x2 ge 0 and x2 le 2000 and y2 ge 0 and y2 le 1600,ng2)
oplot,x2[g2],y2[g2],co=0,thick=5
oplot,x2[g2],y2[g2],co=255,thick=5,linestyle=2

; Galactic plane, and b=+10 line
ll = findgen(360)
bb = fltarr(360)
gal2mag,ll,bb,mll,mbb,wrap=0
adxy,head2,mll,mbb,x3,y3
g3 = where(x3 ge 0 and x3 le 2000 and y3 ge 0 and y3 le 1600,ng3)
oplot,x3[g3],y3[g3],thick=7,co=200
adxy,head2,360.0-153.0,47.0,x4,y4
xyouts,430,1340,'MW disk plane',align=0.5,charsize=1.5,charthick=4.5,co=200
;xyouts,x4,y4,'MW disk plane',align=1,charsize=1.5,charthick=4.5,co=200

; S0 filament
xyouts,650+200,200,'S0 filament',align=0.5,charsize=1.5,charthick=4.3,co=255
arrow,700+200,280,800+200,710,/data,hsize=!d.x_size/50,co=255,/solid,thick=7,hthick=1.5

; GBT10B-035 survey area
; dec=+50 - +75, RA=
;;x5 = [525.7,4.3,37.47,567.9,525.7]
;;y5 = [707.35,796.6,947.73,851.58,707.35]
;;x5 = [557.8,538.6,7.7,19.9,557.8]
;;x5 = [552.8,533.6,13.0,25.0,552.8]
;;x5 = [547.8,528.6,18.0,30.0,547.8]
;;y5 = [841,743.0,810.0,910.0,841.0]
;x5 = [548, 529, 231, 253, 548]+200
;y5 = [841, 743.0, 787, 885, 841.0]
x5 = [453, 748, 745, 802, 793, 736, 729, 693, 687, 610, 616, 431, 453]
y5 = [885, 841, 825, 817, 769, 777, 743, 748, 723, 734, 759, 787, 885]
oplot,x5,y5,co=255,thick=5 ;7 ;1
;polyfill,x5,y5,/line_fill,co=255,thick=3,orientation=45
;xyouts,320.0+200+90,990.0-20,'GBT10B-035',align=0.5,co=255,charsize=1.4,charthick=4,orientation=7 ;5
;xyouts,320.0+200+90,920.0-20,'Survey',align=0.5,co=255,charsize=1.4,charthick=4,orientation=7 ;5
xyouts,610+220,970+150,'GBT10B-035',align=0.5,co=255,charsize=1.4,charthick=4
xyouts,610+220,900+150,'survey',align=0.5,co=255,charsize=1.4,charthick=4
arrow,610+220,880+150,770,820,/data,hsize=!d.x_size/50,co=255,/solid,thick=7,hthick=1.5

; NEW Survey Area
;x6 = [695, 167, 220, 759, 695]
;y6 = [641, 766, 1056, 954, 641]
;x6 = [695, 167-30, 220-30, 759, 695]
;y6 = [641+20, 766-50+20, 1056-50-20, 954-20-20, 641+20]
;x6 = [695, 137, 190, 750, 695]
;y6 = [661, 736, 986, 914, 661]
;x6 = [750, 738, 453, 431, 616, 610, 687, 693, 721, 695, 137, 190, 750 ]
;y6 = [914, 842, 885, 787, 759, 734, 723, 748, 745, 661, 736, 986, 914 ]
; previous best
x6 = [750, 738, 453, 431, 616, 610, 687, 693, 721, 695, 137, 190, 750 ]
y6 = [894, 842, 885, 787, 759, 734, 723, 748, 745, 681, 766, 976, 894 ]
; new area
;x6 = [750, 738, 453, 431, 616, 610, 687, 693, 721, 700.0, 89, 129, 745 ]
;y6 = [894, 842, 885, 787, 759, 734, 723, 748, 745, 701.0, 786, 955, 878 ]
; 107.3, 116.0, 124.9, 115.8
; -8.9, 20.0, 17.1, -11.9
oplot,x6,y6,co=255,thick=5 ;7 ;1
polyfill,x6,y6,/line_fill,co=255,thick=3,orientation=45
xyouts,510,970,'Survey Area',align=0.5,co=255,charsize=1.4,charthick=4,orientation=6

nxx = range(x6)+1
nyy = range(y6)+1
xx = (findgen(nxx)+min(x6))#(fltarr(nyy)+1)
yy = (fltarr(nxx)+1)#(findgen(nyy)+min(y6))
roi_cut,x6,y6,xx,yy,ind,cutind
nel = n_elements(cutind)
totarea = nel*0.05*0.05
timehr = (110./75.)*totarea
print,'Total Area = ',stringize(totarea),' deg^2'
print,'Total time = ',stringize(timehr),' hours'

; The whole rectangle is 92,549 pixels
; using roi_cut and the vertices
;  each pixel is 0.05x0.05 deg^2 = 0.0025 deg^2
;  so that's a total of 231 deg^2
;  we did 75 deg^2 in 110 hrs so 231 deg^2 would take us 339 hours
;  that's probably doable

;stop

;print,sqrt( (mean(x5[0:1])-mean(x5[2:3]))^2 + (mean(y5[0:1])-mean(y5[2:3]))^2 )
;print,sqrt( (x5[0]-x5[1])^2 + (y5[0]-y5[1])^2)
;print,sqrt( (x5[2]-x5[3])^2 + (y5[2]-y5[3])^2)
;stop

; Mid-bottom: ra=23:43:43, dec=50.2,  23:28 < RA < 23:59
; Mid-top: ra=23:44:30 dec=+64.64,    23:21 < RA < 00:07:30

ralevels2 = [0.0,1.0,2.0,3.0,4.0,5.0,6.0, 17.0,18.0,19.0,20.0,21.0,22.0,23.0]
declevels2 = [-20.0,0.0,20.0,40.0,60.0,80.0]
;ralevels2 = [0.0,1.0,2.0,3.0,4.0,19.0,20.0,21.0,22.0,23.0]
;declevels2 = [-20.0,0.0,20.0,40.0,60.0]
ratickn2 = stringize(ralevels2,ndec=0)
dectickn2 = '+'+stringize(declevels2,ndec=0)+'!uo!n'

AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',xticklen=0.03
AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize,co=255,ys=1
;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=1,xs=1,xtickformat='(A1)'
;AXIS,yaxis=0,yr=minmax(mlat),ytit='B!dMS!n',charsize=charsize,co=1,ys=1
fits_grid,head2,lonlev=ralevels2,latlev=declevels2,thick=2,$
          lontit='Right Ascension',lattit='Declination',lontickn=ratickn2,lattickn=dectickn2,$
          framecolor=255,color=255,/xflip,charsize=charsize,/celestial,/translabel,/lonlabelsflip,$
          /latlabelsflip,linestyle=1,/nolonticklevels
;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xtickformat='(A1)',xs=1
;AXIS,yaxis=0,yr=minmax(mlat),ytit=' ',charsize=charsize,co=255,ytickformat='(A1)',ys=1

; More annotations
xyouts,10,1460,'+60!uo!n',align=1,co=255,charsize=charsize,charthick=3
;xyouts,10,1460,'0!uh!n',align=1,co=255,charsize=charsize,charthick=3
xyouts,110,1000,'2!uh!n',align=1,co=255,charsize=charsize,charthick=3
xyouts,50,1180,'4!uh!n',align=1,co=255,charsize=charsize,charthick=3
;xyouts,90,710,'22!uh!n',align=1,co=255,charsize=charsize,charthick=3
xyouts,100,700,'22!uh!n',align=1,co=255,charsize=charsize,charthick=3
xyouts,60,540,'20!uh!n',align=1,co=255,charsize=charsize,charthick=3


;stop

;file = 'plots/ms_mag_lowres_velmlon_center2'
;ps_open,file,/color,thick=5,/encap
;device,/inches,xsize=10.2,ysize=6.3
;posim = [0.12, 0.11, 0.98, 0.85]
;poscol = [0.12, 0.96, 0.98, 0.985]
;displayc,imtot2c2,mlon2,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km s!u-1!n)',$
;         tit=' ',/xflip,charsize=charsize,posim=posim3b,poscol=poscol3b,mask=0,xticklen=0.03,$
;         coltickname=['0.001','0.01','0.1','1'],coltickval=[0.001,0.01,0.1,1.0],coldiv=3,/noerase
display,imtot2c2,mlon2,vel,min=0.0005,max=1,/log,xtit='L!dMS!n',ytit='V!dLSR!n (km s!u-1!n)',$
         tit=' ',/xflip,charsize=charsize,position=posim3b,maskv=0,xticklen=0.03,/noerase
xyouts,-75.0,-140,'(b)',charsize=charsize,align=0.,co=255

;AXIS,xaxis=0,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,xaxis=1,xr=reverse(minmax(mlon2)),xtit=' ',charsize=charsize,co=255,xs=1,xtickformat='(A1)',$
;     xticklen=0.03
;AXIS,yaxis=0,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
;AXIS,yaxis=1,yr=minmax(vel),ytit=' ',charsize=charsize,co=255,ys=1,ytickformat='(A1)',yminor=5
oplot,ml[wcutind],wstr[wcutind].vlsr,ps=1,co=255
;oplot,mlon,vfid,co=255
;oplot,mlon,vfid,co=0,linestyle=2
oplot,mlon2,vfid2,co=255
oplot,mlon2,vfid2,co=0,linestyle=2

if keyword_set(save) then begin
  ps_close
  ps2gif,file+'.eps',/eps
endif


xyad,head2,x5,y5,ml5,mb5
mag2gal,ml5,mb5,gl5,gb5
glactc,ra5,dec5,2000.,gl5,gb5,2,/deg
bd = where(ra5 gt 180,nbd)
ra5[bd] -= 360

xyad,head2,x6,y6,ml6,mb6
mag2gal,ml6,mb6,gl6,gb6
glactc,ra6,dec6,2000.,gl6,gb6,2,/deg
bd = where(ra6 gt 180,nbd)
ra6[bd] -= 360

plot,ra6,dec6,xr=[23,-28],yr=[45,80],xs=1,ys=1,xtit='RA',ytit='DEC'
oplot,ra5,dec5


racen = 357.5
decen = 56.5
xx = (findgen(85)*4.0/60.)#replicate(1,225)
xx -= max(xx)*0.5
yy = replicate(1,85)#findgen(225)*4.0/60.
yy -= max(yy)*0.5
rotsphcen,xx,yy,racen,decen,ll,bb,/reverse
bd = where(ll gt 180)
ll[bd] -= 360.
oplot,ll,bb,ps=3

racen = 346.5
decen = 70.0
xx = (findgen(85)*4.0/60.)#replicate(1,180)
xx -= max(xx)*0.5
yy = replicate(1,85)#findgen(180)*4.0/60.
yy -= max(yy)*0.5
rotsphcen,xx,yy,racen,decen,ll,bb,/reverse
bd = where(ll gt 180)
ll[bd] -= 360.
oplot,ll,bb,ps=3

racen2 = 7.0
decen2 = 69.5
xx2 = (findgen(85)*4.0/60.)#replicate(1,180)
xx2 -= max(xx2)*0.5
yy2 = replicate(1,85)#findgen(180)*4.0/60.
yy2 -= max(yy2)*0.5
rotsphcen,xx2,yy2,racen2,decen2,ll2,bb2,/reverse
bd2 = where(ll2 gt 180)
ll2[bd2] -= 360.
oplot,ll2,bb2,ps=3,co=250

racen3 = 357.5
decen3 = 71.0
xx3 = (findgen(210)*4.0/60.)#replicate(1,210)
xx3 -= max(xx3)*0.5
yy3 = replicate(1,210)#findgen(210)*4.0/60.
yy3 -= max(yy3)*0.5
rotsphcen,xx3,yy3,racen3,decen3,ll3,bb3,/reverse
bd3 = where(ll3 gt 180)
ll3[bd3] -= 360.
oplot,ll3,bb3,ps=3,co=150

; rotsphcen doesn't seem to be working right!!
nx = 211
ny = 211
racen = 357.5 ;357.5
decen = 70.0
mkhdr,hd1,fltarr(nx,ny)
sxaddpar,hd1,'CTYPE1','RA---TAN'
sxaddpar,hd1,'CRPIX1',nx/2
sxaddpar,hd1,'CRVAL1',racen
sxaddpar,hd1,'CDELT1',4.0/60.
sxaddpar,hd1,'CTYPE2','DEC--TAN'
sxaddpar,hd1,'CRPIX2',ny/2
sxaddpar,hd1,'CRVAL2',decen
sxaddpar,hd1,'CDELT2',4.0/60.
x = findgen(nx)#replicate(1,ny)
y = replicate(1,nx)#findgen(ny)
xyad,hd1,x,y,ra1,dec1
bd = where(ra1 gt 180,nbd)
if nbd gt 0 then ra1[bd]-=360
plot,ra1,dec1,ps=3,/xsty,/ysty,xr=reverse(minmax(ra1))

stop

end
