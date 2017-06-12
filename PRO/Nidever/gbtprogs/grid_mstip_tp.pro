pro grid_mstip_tp

; Put all of the MS-tip survey observations
; into a 3D datacube (grid)
; No interpolation is needed since they are all
; on the same grid

forward_function func_cos, func_cospoly
resolve_routine,'remove_baseline_indiv',/compile_full_file

dir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT10B-035/data/'
tag = 'red1'
;tag = 'red2'
files = file_search(dir+'MSTIP-survey*_tp_'+tag+'.dat',count=nfiles)

npix = 7012
;vel = (findgen(800)+1-400)*0.322*5
vel = (findgen(npix)+1-npix/2)*3.22

cen_ra = 357.5d0
cen_dec = 56.5d0
step = 4.0/60.0

dec0 = cen_dec-7.5  ; DEC value of first row

; Initialize the cube
cube = fltarr(74,226,npix)
raarr = fltarr(74,226)
decarr = fltarr(74,226)
maxy = -1

For i=0,nfiles-1 do begin

  print,'Adding ',file_basename(files[i])
  restore,files[i]

  if tag_exist(final,'RA') then begin
    ra = final.ra
    dec = final.dec
  endif else begin
    ra = final.glon
    dec = final.glat
  endelse

  ; Convert coordinates
  rotsphcen,ra,dec,cen_ra,cen_dec,zeta,eta,/gnomic
  
  plot,zeta,eta,ps=1,/ysty

  ; Y-index is just determined by the DEC value
  yind = round( (dec-dec0)/step )

  ; X-index

  ui = uniq(final.scan,sort(final.scan))
  scans = final[ui].scan
  nscans = n_elements(scans)


  ; Loop through the scans
  yindarr = intarr(nscans)
  For j=0,nscans-1 do begin

    ind = where(final.scan eq scans[j],nind)
    ifinal = final[ind]

    if tag_exist(final,'RA') then begin
      ra = ifinal.ra
      dec = ifinal.dec
    endif else begin
      ra = ifinal.glon
      dec = ifinal.glat
    endelse

    ; Make the RA continuous
    bd = where(ra lt 180,nbd)
    if nbd gt 0 then ra[bd]=ra[bd]+360

    ; Y-index is just determined by the DEC value
    yind = long(median(round( (dec-dec0)/step )))
    yindarr[j] = yind

    ; Which scan direction are we in?
    scandir = sign(median(slope(ra)))

    ; Stuff the data in the cube
    data = transpose(ifinal.spec)
    if scandir eq 1 then begin
      cube[*,yind,*] = data
      raarr[*,yind] = ra
    ; reverse order
    endif else begin
      cube[*,yind,*] = reverse(data,1)
      raarr[*,yind] = reverse(ra)
    endelse
    decarr[*,yind] = dec

    print,scans[j],yind,scandir

    maxy = maxy > yind

    ;stop

  End

  goto,skip

  ; Remove a polynomial baseline from this session
  subcube = cube[*,min(yindarr):max(yindarr),*]
  sz = size(subcube)
  tot1 = total(subcube,1)/sz[1]
  sig1 = mad(tot1)
  bd1 = where(tot1 gt 4*sig1,nbd1)
  if nbd1 gt 0 then tot1[bd1] = !values.f_nan
  tot = total(tot1,1)/sz[2]
  xx1 = findgen(sz[3])
  yy1 = tot
  sig = mad(tot)
  bd = where(xx1 gt 275 or (xx1 ge 23 and xx1 le 37) or finite(tot) eq 0,nbd)
  ;bd = where(xx gt 275 or (xx ge 37 and xx le 53),nbd)
  remove,bd,xx1,yy1

  if file_basename(files[i]) eq 'MSTIP-survey_ses2_s10-42_red1.dat' then begin
    bd = where(xx1 ge 122 and xx1 le 160,nbd)
    remove,bd,xx1,yy1
  endif

  initpars = [0.004d0,159, 44.5, 0.0, 0.0, 0.0]
  npar = n_elements(initpars)
  parinfo = replicate({value:0.0,fixed:0,limited:[1,1],limits:[0.0,0.0]},npar)
  parinfo[0].limits = [0.00,0.04]
  parinfo[1].limits = [150,170]
  parinfo[2].limits = [0.0,100]
  parinfo[3:*].limited = 0

  ; func_cos
  fpar1 = MPFITFUN('func_cospoly',xx1,yy1,xx1*0+1,initpars,/quiet,parinfo=parinfo,$
                  status=status1,niter=niter1)
  ;if status1 lt 1 then stop,'STATUS1<1'

  swave1 = func_cospoly(xx1,fpar1)

  ; do a second cut
  diff = yy1-swave1
  bd2 = where(diff gt 3*sig,nbd2)
  if nbd2 gt 0 then remove,bd2,xx1,yy1

  fpar = MPFITFUN('func_cospoly',xx1,yy1,xx1*0+1,initpars,/quiet,parinfo=parinfo,$
                  status=status1,niter=niter1)

  plot,xx1,yy1,ps=1
  xx = findgen(sz[3])
  swave = func_cospoly(xx,fpar)
  oplot,xx,swave,co=250

  ; Now remove it
  swave2 = replicate(1.0,sz[1]*sz[2])#swave
  swave3 = fltarr(sz[1],sz[2],sz[3])
  swave3[*] = swave2
  subcube -= swave3

  ; Might also need a linear component

  ; stuff it back in
  cube[*,min(yindarr):max(yindarr),*] = subcube

  skip:

  ;stop

End

;stop


; Fix messed up scans
; Y=9, X=0-40
; Y=10, X=0-10
; Y=19, X=40-73
; Y=32, X=0-35
; Y=41-42, X=0-25
; Y=54-55, X=45-73
cube[0:40,9,*] = cube[0:40,8,*]
cube[0:10,10,*] = cube[0:10,11,*]
cube[40:73,19,*] = cube[40:73,20,*]
cube[0:35,32,*] = cube[0:35,33,*]
cube[0:25,41,*] = cube[0:25,40,*]
cube[0:25,42,*] = cube[0:25,43,*]
cube[45:73,54,*] = cube[45:73,53,*]
cube[45:73,55,*] = cube[45:73,56,*]
; WOW, this makes a huge difference!

raarr2 = raarr[*,0:maxy]
decarr2 = decarr[*,0:maxy]
cube2 = cube[*,0:maxy,*]
sz = size(cube2)
tot1 = total(cube2,1)
tot2 = total(cube2,2)
tot3 = total(cube2,3)

;decarr1 = findgen(maxy+1)*step+dec0
ra1 = reform(raarr2[*,maxy/2])
dec1 =  reform(decarr2[36,*])

; Bin spatially and in velocity
;bincube2 = REBIN(cube2[*,0:81,*],sz[1]/2,41,sz[3]/5)
bincube2 = REBIN(cube2[*,0:81,*],sz[1]/2,41,sz[3])
btot1 = total(bincube2,1)
btot2 = total(bincube2,2)
btot3 = total(bincube2,3)

; spectral smmothing
print,'Spectral smoothing'
smcube2 = cube2*0
fwhm = 2.0 ; 5.0
for i=0,73 do begin
  for j=0,maxy do begin
    smspec = GSMOOTH(reform(cube2[i,j,*]),fwhm)
    smcube2[i,j,*] = smspec
  end
end

; spatial smoothing
print,'Spatial smoothing'
smcube3 = smcube2*0
fwhm2 = 2.0 ;3
for i=0,npix-1 do begin
  im = reform(smcube2[*,*,i])
  smim = GSMOOTH(im,fwhm2)
  smcube3[*,*,i] = smim
end

smtot1 = total(smcube3,1)
smtot2 = total(smcube3,2)
smtot3 = total(smcube3,3)

displayc,tot3

dv = 5*0.322

stop

; This should be of the MS velocity range
tot3b = total(cube2[*,*,69:195],3)
col3b = tot3b * dv ; * 1.83e18
displayc,col3b,/log

; SMOOTH!!!!

; Make a peak/maximum image
max1 = max(cube2,dim=1)
max2 = max(cube2,dim=2)
max3 = max(cube2[*,*,69:195],dim=3)

; there's something in max2 between 0 and 30

; Check the noise, is it at the expected level?

; I think we've detected something at X=4-16 Y=68-76, Z=156-176
; It's basically in the top/left corner.
; OH YEAH, do this!!!!
; smtot1b = total(smcube3[0:35,*,*],1)
;displayc,smtot1b[*,45:250],/z
; you can also see it in the unsmoothed cube
; I HOPE IT'S NOT A SCREWED UP BASELINE!!

; There might even be something at even more negative velocity.
; X=??, Y=57, Z=24

; There might even be some faint emission that "goes through"
; (in velocity) the HVC.  You can see it here:
; displayc,smtot1[*,45:250],/z
; but it doesn't go through in the other dimension

stop

col3b = tot3b*1.61*1.83e18 / 1e18
; the noise level in the column density image is 1.3E18 cm^-2

; Make plots
; Vlsr vs. DEC for ALL RA
ps_open,'gbt_mstip_all',/color,thick=4,/encap
device,/inches,xsize=12.3,ysize=12
displayc,tot1[*,45:250],dec1,vel[45:250],min=-1.5,max=10,xtit='DEC (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (all RA)',position=[0.02,0,0.5,0.5],colcharsize=0.8
arrow,50.62,-463.15,51.19,-424.31,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
displayc,tot2[*,45:250],ra1,vel[45:250],min=-1.5,max=10,/xflip,xtit='RA (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (all DEC)',position=[0.02,0.5,0.5,1.0],/noerase,colcharsize=0.8
arrow,359.22,-478.388,358.47,-441.08,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
displayc,col3b,ra1,dec1,min=-3,max=30,xtit='RA (deg)',ytit='DEC (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -530<VLSR<-330 km/s',position=[0.5,0.5,1.0,1.0],/noerase
arrow,359.144,50.246,358.555,50.983,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
xyouts,0.75,0.3,'Centered near:',align=0.5,charsize=1.5,co=0,/norm
xyouts,0.75,0.27,'RA=357.8, DEC=51.395, VLSR=-412 km/s',align=0.5,charsize=1.5,co=0,/norm
ps_close
ps2gif,'gbt_mstip_all.eps',/eps

; The new detection
; Vlsr vs. DEC for ALL RA
tot1c = total(smcube3[0:35,*,*],1)
tot2c = total(smcube3[*,65:*,*],2)
tot3c = total(smcube3[*,*,150:178],3)
col3c = tot3c*1.61*1.83e18 / 1e18
ps_open,'gbt_mstip_new',/color,thick=4,/encap
device,/inches,xsize=12.3,ysize=12
displayc,tot1c[*,45:250],dec1,vel[45:250],/z,xtit='DEC (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (352.89<RA<356.66)',position=[0.02,0,0.5,0.5],colcharsize=0.8
arrow,53.13,-447.26,53.525,-392.535,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
displayc,tot2c[*,45:250],ra1,vel[45:250],/z,/xflip,xtit='RA (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (53.33<DEC<54.47)',position=[0.02,0.5,0.5,1.0],/noerase,colcharsize=0.8
arrow,354.42,-444.61,353.9,-400.48,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
displayc,col3c,ra1,dec1,/z,xtit='RA (deg)',ytit='DEC (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -400<VLSR<-350 km/s',position=[0.5,0.5,1.0,1.0],/noerase
arrow,354.539,52.308,353.992,53.310,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
xyouts,0.75,0.3,'Centered near:',align=0.5,charsize=1.5,co=0,/norm
xyouts,0.75,0.27,'RA=353.60, DEC=53.840, VLSR=-374.0 km/s',align=0.5,charsize=1.5,co=0,/norm
ps_close
ps2gif,'gbt_mstip_new.eps',/eps


stop

end
