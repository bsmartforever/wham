pro grid_HVCAGrid1_gal,survey

; Put all of the MS-tip survey observations
; into a 3D datacube (grid)
; No interpolation is needed since they are all
; on the same grid

forward_function func_cos, func_cospoly
resolve_routine,'remove_baseline_indiv',/compile_full_file

dir = '~/observing/gbt/GBT13B-068/data/'
tag = 'red2'


;fits_read,dir+'grid_mstip_mag2_rfifix3_bigcube.fits',bigcube,head
;goto,skiptohere

files = file_search(dir+'*'+tag+'.dat',count=nfiles)

base = file_basename(files)
;arr = strsplitter(base,'_',/extract)
;sesname = reform(arr[1,*])
;sesnum = long(strmid(sesname,3))
lo = strpos(base,'_ses')
dum = strarr(nfiles)
for i=0,nfiles-1 do dum[i] = strmid(base[i],lo[i]+1)
arr = strsplitter(dum,'_',/extract)
sesname = reform(arr[0,*])
sesnum = long(strmid(sesname,3))
;gd = where(sesnum le 9,ngd)
;files = files[gd]
;nfiles = ngd


;;vel = (findgen(800)+1-400)*0.322*5
;;cen_ra = 357.5d0
;;cen_dec = 56.5d0
;;23:10:00.0     70.5000
;;cen_ra = ten(23,10,0)*15.0
;;cen_dec = 70.5
;cen_ra = 357.5d0
;cen_dec = 62.0
;step = 4.0/60.0
;
;; dec=46 to 77
;
;;dec0 = cen_dec-7.5  ; DEC value of first row
;;dec0 = cen_dec-7.5  ; DEC value of first row
;dec0 = cen_dec-16.0  ; DEC value of first row
;
;glactc,cen_ra,cen_dec,2000.0,cen_glon,cen_glat,1,/deg
;gal2mag,cen_glon,cen_glat,cen_mlon,cen_mlat

;-------------------------------------------------------
; SETUP THE HEADER
;-------------------------------------------------------
; copied some of this code from /net/halo/dln5q/gbt/mag_head.pro

; Set up the WCS information for the LARGE image/cube
undefine,head
nchan = 6300
nx = 155 ;150
ny = 135 ;130

bigcube = fltarr(nx,ny,nchan)
;bigcube[*,*,*] = !values.f_nan   ; default all bad
MKHDR,head,bigcube
SXADDPAR,head,'NAXIS3',nchan

;xref = 90L ;850L ;790L  ; 875
;yref = 127L ;800L ;750L  ; 800
;ra_ref = cen_ra
;dec_ref = cen_dec
;xref = 280L ;150L
;yref = 100L
;mlon_ref = -145.0
;mlat_ref = 16.50
xref = nx/2 ;150L
yref = ny/2
;mlon_ref = -150.0
;mlat_ref = 17.0
glon_ref = 166.0d0 ;166.30000  ; HVCAGrid1
glat_ref = 39.00000d0

step = 3.5d0/60.0d0
;vstep = 1.610
;vmin = -642.390
; -458.92153, +555.37886d0
vstep = 0.16102562d0
vmin = -458.92153d0  

; in GBTIDL, load in one of our data files
; select km/s and VLSR in plotter
; x=getxarray()
; x2=rebin(x2[14300:18299],800)  ; MAKE SURE TO USE THE RIGHT VALUES HERE!!
; print,min(x2),x2[1]-x2[0]
;      -670.14361       1.6101484
; the last few decimal places will vary a bit by position

; Braun used SIN with CDELT
; Stanimirovic used NO projection with CDELT
; Bruens used ZEA with CDELT
SXADDPAR,head,'CDELT1',step
SXADDPAR,head,'CRPIX1',xref+1L
SXADDPAR,head,'CRVAL1',glon_ref
SXADDPAR,head,'CTYPE1','GLON-TAN'
SXADDPAR,head,'CDELT2',step
SXADDPAR,head,'CRPIX2',yref+1L
SXADDPAR,head,'CRVAL2',glat_ref
SXADDPAR,head,'CTYPE2','GLAT-TAN'
SXADDPAR,head,'CDELT3',vstep
SXADDPAR,head,'CRPIX3',1L
SXADDPAR,head,'CRVAL3',vmin
SXADDPAR,head,'CTYPE3','VELO-LSR'

sz = size(bigcube)
nx = sz[1]
ny = sz[2]

numobs = lonarr(nx,ny)
exptime = fltarr(nx,ny)

;stop

;if n_elements(survey) eq 0 then begin
;  print,'Restoring the data'
;  restore,dir+'grid_mstip_mag_survey.dat'
;endif
;goto,grid_survey

; Initialize the cube
;cube = fltarr(74,226,800)
;raarr = fltarr(74,226)
;decarr = fltarr(74,226)
maxy = -1

; CHECKING THESE
;files = dir+['MSgrid3_518_ses25_s13-28_all2.dat','MSgrid3_638_ses27_s8-18_all2.dat',$
;             'MSgrid4_588_ses38_s10-13_all2.dat','MSgrid4_597_ses38_s14-29_all2.dat',$
;             'MSgrid3_590_ses47_s25-40_all2.dat']
;nfiles = n_elements(files)

For i=0,nfiles-1 do begin
;For i=21,nfiles-1 do begin
;For i=106,nfiles-1 do begin

  print,'Adding ',strtrim(i+1,2),'/',strtrim(nfiles,2),' ',file_basename(files[i])
  restore,files[i]
  nfinal = n_elements(final)

  fbase = file_basename(files[i],'_'+tag+'.dat')
  lo = strpos(fbase,'_ses')
  dum = strsplit(strmid(fbase,lo+1),'_',/extract)
  session = dum[0]
  session_num = long(strmid(session,3))

  add_tag,final,'session',0,final
  final.session = session_num

  ui = uniq(final.scan,sort(final.scan))
  nscans = n_elements(ui)
  print,'NSCANS=',strtrim(nscans,2)

  ;if nscans lt 2 then begin
  ;  print,'Not enough scans. Skipping'
  ;  goto,skip
  ;endif

 ; ; Fix some coords
 ; ; session 10, scans 8,10,12,14,15,17
 ; ; session 11, scans 12, 14
 ; ; session 44
 ; if session_num eq 10 or session_num eq 11 or session_num eq 44 then begin
 ;   ;bd = where(survey1.session eq 10 or survey1.session eq 11,nbd)
 ;   ;ses_scan = strtrim(survey1[bd].session,2)+'-'+strtrim(survey1[bd].scan,2)
 ;   ;ui = uniq(ses_scan,sort(ses_scan))
 ;   ;uses_scan = ses_scan[ui]
 ;   ;nuses_scan = n_elements(uses_scan)
;
;    ui = uniq(final.scan,sort(final.scan))
;    scans = final[ui].scan
;    nscans = n_elements(scans)
;    for j=0,nscans-1 do begin
;      ind1 = where(final.scan eq scans[j],nind1)
;      x = indgen(nind1)
;      y = final[ind1].ra
;      lora = where(y gt 180,nlora)
;      if nlora gt 0 then y[lora]-=360  ; continuous values
;
;      ra_coef = robust_poly_fitq(x,y,1)
;      model = poly(x,ra_coef)
;      sig = mad(y-model)
;      bdpnt = where( abs(y-model)/sig gt 10,nbdpnt)  ; find outliers
; 
;      ; Make sure model RA is not negative
;      model2 = model
;      bdmodel = where(model2 lt 0,nbdmodel)
;      if nbdmodel gt 0 then model2[bdmodel]+=360
;
;      ; Fix bad values
;      if nbdpnt gt 0 then final[ind1[bdpnt]].ra = model2[bdpnt]
;
;    endfor
;  endif


; Bad data, redone in session4
if fbase eq 'HVCAGrid1_ses3_s10-36' then begin
  bd = where(final.procseqn ge 12 and final.procseqn le 24,nbd)
  remove,bd,final
endif

;  ; Kludge
;  ;  some bad integrations
;  if fbase eq 'MSgrid2_645_ses11_s16-32' then begin
;    final[[190,192]].comb_corr = !values.f_nan
;    final[[299,304,312]].comb_corr = !values.f_nan
;    final[[444,450,451,514,523,955,957,1016]].comb_corr = !values.f_nan
;    final[170:172].comb_corr = !values.f_nan
;    final[316:319].comb_corr = !values.f_nan
;    final[408:412].comb_corr = !values.f_nan
;    final[555:556].comb_corr = !values.f_nan
;    final[991].comb_corr = !values.f_nan
;    final[1229].comb_corr = !values.f_nan
;    final[1445].comb_corr = !values.f_nan
;    final[1648].comb_corr = !values.f_nan
;    final[1651].comb_corr = !values.f_nan
;    final[1673].comb_corr = !values.f_nan
;    final[1682].comb_corr = !values.f_nan
;    final[1706].comb_corr = !values.f_nan
;    final[1825:1828].comb_corr = !values.f_nan
;    final[1844:1845].comb_corr = !values.f_nan
;    final[1851:1852].comb_corr = !values.f_nan
;    final[1860].comb_corr = !values.f_nan
;    final[1867].comb_corr = !values.f_nan
;    final[1872].comb_corr = !values.f_nan
;    final[1878].comb_corr = !values.f_nan
;    ;lo = 25
;    ;hi = 32
;    ;ind = [indgen(20)+lo-22,indgen(20)+hi+2]
;    ;med = median(final.comb_corr[ind],dim=1)
;    ;for j=lo,hi do final.comb_corr[j]=med
;  endif
;
;  ;  some bad integrations
;  if fbase eq 'MSgrid2_703_ses18b_s23-33' then begin
;    final[490:493].comb_corr = !values.f_nan
;    ;lo = 25
;    ;hi = 29
;    ;ind = [indgen(20)+lo-22,indgen(20)+hi+2]
;    ;med = median(final.comb_corr[ind],dim=1)
;    ;for j=lo,hi do final.comb_corr[j]=med
;  endif


  ; REMOVE RESIDUAL baseline
  wset,0
;
;  ;REMOVE_RESIDUAL_POLYFIT,final,/pl
;  REMOVE_RESIDUAL_BSPLINE,final,/pl

  ; Set Bad pixels to ZERO
  bd = where(finite(final.comb_corr) eq 0,nbd)
  if nbd gt 0 then begin
    print,'Setting ',strtrim(nbd,2),' BAD PIXELS to ZERO'
    temp = final.comb_corr
    temp[bd] = 0.0
    final.comb_corr = temp
  endif

  ; Looking for bad integrations
  findbadints = 0
  if keyword_set(findbadints) then begin
    med = median(final.comb_corr[0:300],dim=1)
    g = where(abs(med) gt 0.0,ng)
    sig = mad(med[g])
    bd = where(med gt 5*sig,nbd)
    if nbd gt 0 then begin
      plot,med,xs=1
      oplot,bd,med[bd],ps=1,co=250
      oplot,[0,1e4],[0,0]+5*sig
      print,strtrim(nbd,2),' bad integrations'
      print,bd
      stop
      for j=0,nbd-1 do begin
        print,j+1,' ',bd[j]
        wset,0
        plot,final[bd[j]].comb_corr,yr=[-0.5,0.5],tit=bd[j]
        wset,1
        displayc,final.comb_corr[0:300],min=-0.2,max=0.2,yr=[bd[j]-10>0,bd[j]+10<(nfinal-1)]
        stop
      endfor
    endif
  endif

  wset,1
  
  displayc,smooth(final.comb_corr[0:300],[20,20<nscans],/edge_truncate,/nan),/z
  ;wait,1



  ;stop

  ; Blanking bad scans
  ; Fix messed up scans
  ; Y=9, X=0-40
  ; Y=10, X=0-10
  ; Y=19, X=40-73
  ; Y=32, X=0-35
  ; Y=41-42, X=0-25
  ; Y=54-55, X=45-73
  ;cube[0:40,9,*] = cube[0:40,8,*]
  ;cube[0:10,10,*] = cube[0:10,11,*]
  ;cube[40:73,19,*] = cube[40:73,20,*]
  ;cube[0:35,32,*] = cube[0:35,33,*]
  ;cube[0:25,41,*] = cube[0:25,40,*]
  ;cube[0:25,42,*] = cube[0:25,43,*]
  ;cube[45:73,54,*] = cube[45:73,53,*]
  ;cube[45:73,55,*] = cube[45:73,56,*]

  ; rows: 10, 11, 20, 33, 42, 43, 55, 56
;  badrows = [10, 11, 20, 33, 42, 43, 55, 56]
;  noise = 0.055
;  for j=0,n_elements(badrows)-1 do begin
;    bd = where(final.procseqn eq badrows[j],nbd)
;    if nbd gt 0 then final[bd].comb_corr = randomn(seed,nchan,nbd)*noise
;    ;if nbd gt 0 then final[bd].comb_corr = 0.0
;  end

  ; Add to final structure
  ;push,survey,final
  dum = replicate({object:'',file:'',glon:0.0d0,glat:0.0d0,duration:0.0,exposure:0.0,$
                   tsys:0.0,scan:0L,procseqn:0,int:0,session:0,comb_corr:fltarr(nchan)},n_elements(final))
  struct_assign,final,dum
  push,survey,dum


  skip:

  ;wait,0.5
  stop

Endfor


; Regrid on the final grid
;---------------------------

; Convert from MLON/MLAT to new X/Y
;ADXY,head,final.glon,final.glat,xnew,ynew
ADXY,head,survey.glon,survey.glat,xnew,ynew

; Triangulate
triangulate,xnew,ynew,tr,b

;xmin = ceil(min(xnew))
;xmax = floor(max(xnew))
xmin = round(min(xnew))
xmax = round(max(xnew))
nxout = xmax-xmin+1
xout = lindgen(nxout)+xmin
;ymin = ceil(min(ynew))
;ymax = floor(max(ynew))
ymin = round(min(ynew))
ymax = round(max(ynew))
nyout = ymax-ymin+1
yout = lindgen(nyout)+ymin

if nxout lt 2 or nyout lt 2 then begin
  print,'Not enough pixels in XOUT/YOUT'
  goto,skip
endif

;numobs = lonarr(nx,ny)

; Make a mask to make sure that TRIGRID doesn't extrapolate and
;  create garbage around the edges
cmask = lonarr(nx,ny)
wig = 0.2 ; 0.1  ; "convolving" wiggle
cmask[round(xnew),round(ynew)]++
cmask[round(xnew-wig),round(ynew-wig)]++
cmask[round(xnew-wig),round(ynew)]++
cmask[round(xnew-wig),round(ynew+wig)]++
cmask[round(xnew),round(ynew-wig)]++
cmask[round(xnew),round(ynew+wig)]++
cmask[round(xnew+wig),round(ynew-wig)]++
cmask[round(xnew+wig),round(ynew)]++
cmask[round(xnew+wig),round(ynew-wig)]++
;cmask = CONVOL(cmask,lonarr(3,3)+1,/center)
cmask = cmask/(cmask > 1)

exposure = median(survey.exposure)  ; use same exptime for all points

; Setting masked integrations to 999999
;  TRIGRID will mask these
bd = where(total(survey.comb_corr,1) eq 0.0,nbd)
if nbd gt 0 then final[bd].comb_corr=999999.
if nbd gt 0 then print,'Masking out ',strtrim(nbd,2),' bad integrations for TRIGRID'

For j=0,nchan-1 do begin

  ;if (j+1) mod 200 eq 0 then print,strtrim(j+1,2),'/',strtrim(nchan,2)

  grid = TRIGRID(xnew, ynew, survey.comb_corr[j], tr, XOUT = xout, YOUT = yout,$
                 max_value=1000, missing=!values.f_nan)
 
  if j eq 0 then begin
    obsmask = long(finite(grid) eq 1 and cmask[xmin:xmax,ymin:ymax] eq 1)
    numobs[xmin:xmax,ymin:ymax] += obsmask
    exptime[xmin:xmax,ymin:ymax] += obsmask*exposure
  endif
  bd = where(finite(grid) eq 0 or cmask[xmin:xmax,ymin:ymax] eq 0,nbd)
  if nbd gt 0 then grid[bd]=0.0

  ; WEIGHT BY EXPOSURE TIME!!!!!

  bigcube[xmin:xmax,ymin:ymax,j] += grid*exposure

  if j eq 150 then begin
    wset,0
    display,grid,maskv=0,maskc=0
    ;wait,1
    ;stop
  endif

Endfor

;  NEED TO REVERSE THE CHANNELS!!!

; Reverse in velocity
bigcube = reverse(temporary(bigcube),3)

; Divide by Exposure TIME !!!
sz = size(bigcube)
for i=0,sz[3]-1 do bigcube[*,*,i]/=(exptime>1)

; Save the combined cube
outfile = 'grid_HVCAGrid1_gal_bigcube.fits'
print,'Writing cube to ',outfile
fits_write,dir+outfile,bigcube,head


; quick plot
fits_arrays,head,glon,glat,vel
dv = abs(median(slope(vel)))
!p.font = 0
;file = 'grid_HVCAGrid1_072213'
;file = 'grid_HVCAGrid1_081213'
;file = 'grid_HVCAGrid1_091613'
file = 'grid_HVCAGrid1_091813'
ps_open,file,/color,thick=5,/encap
device,/inches,xsize=9.5,ysize=8.5
;tot3 = total(bigcube[*,*,2090:2690],3)
;tot3 = total(bigcube[*,*,2390:2690],3)
;displayc,tot3[*,0:20],glon,glat[0:20],/z,/xflip,xtit='GLON',ytit='GLAT',tit='-122 < V!dLSR!n < -26 km/s'
tot3 = total(bigcube[*,*,1500:2250],3)
col3 = tot3*dv*1.83e18/1e18
posim = [0.10,0.10,0.98,0.90]
poscol = [0.10,0.97,0.98,0.99]
displayc,col3,glon,glat,min=-6,max=50,maskv=0,maskc=0,/xflip,xtit='Galactic Longitude',$
         posim=posim,poscol=poscol,charsize=1.2,ytit='Galactic Latitude',$
         tit='Complex A HI Column Density (10!u18!n atoms/cm!u2!n, -217 < V!dLSR!n < -97 km/s)'

;; Overplot LAB data in contours
;rdhispecm,[161.5,170.5],[35.0,43.0],spec,v,l,b
;t2=total(spec[*,234:351,*],2)
;contour,t2,l,b,levels=indgen(5)*5+1,/over,color=0

xr = reverse(minmax(glon))
yr = minmax(glat)
plot,[0,0],/nodata,/noerase,xr=xr,yr=yr,xs=1,ys=1,charsize=charsize,position=posim,xtit=' ',ytit=' ',$
           co=255,xtickformat='(A1)',ytickformat='(A1)'

ps_close
ps2jpg,file+'.eps',/eps

;fits_write,dir+'grid_mstip_mag2_rfifix3_bigcube1.fits',bigcube,head

;stop


skiptohere:


stop

fits_arrays,head,ml,mb,vl
cube = bigcube
head2 = head

;; Cut out the main survey region
;xlo = 172
;xhi = 449
;ylo = 25
;yhi = 147
;zlo = 58
;zhi = 250
;
;; This is just the main survey area and the two attached pieces
;fits_arrays,head,mlon,mlat,vel
;;ml = mlon[185:*]
;;mb = mlat[25:143]
;;vl = vel[45:250]
;;cube = bigcube[185:*,25:143,45:250]
;ml = mlon[xlo:xhi]
;mb = mlat[ylo:yhi]
;vl = vel[zlo:zhi]
;cube = bigcube[xlo:xhi,ylo:yhi,zlo:zhi]
;sz = size(cube)
;
;tot1 = total(cube,1)
;tot2 = total(cube,2)
;tot3 = total(cube,3)
;
;; Make new header for this subcube
;head2 = head
;sxaddpar,head2,'CTYPE1','MLON-CAR'
;sxaddpar,head2,'CRPIX1',1
;sxaddpar,head2,'CRVAL1',ml[0]
;sxaddpar,head2,'CTYPE2','MLAT-CAR'
;sxaddpar,head2,'CRPIX2',1
;sxaddpar,head2,'CRVAL2',mb[0]
;sxaddpar,head2,'CTYPE3','VELO-LSR'
;sxaddpar,head2,'CRPIX3',1
;sxaddpar,head2,'CRVAL3',vl[0]


; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec[0:330])
  ;rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

FITS_WRITE,dir+'grid_mstip_mag2_rfifix3.fits',cube,head2
FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_rmsmap.fits',rmsmap,rmshead

;stop

; Copied from /net/halo/dln5q/gbt/grid_gbt_gal.pro
;goto,smoothcube_pos

;----------------------
; Smooth the datacube
;----------------------
SMOOTHCUBE:


; Smooth each integration with a Gaussian in velocity
gdints = where(reform(cube[*,*,0]) ne 0.0,ngdints)
print,'SMOOTHING GRID IN VELOCITY - ',strtrim(ngdints,2),' positions'
onemap = reform(cube[*,*,0])
chansm = 10L
for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),'/',strtrim(ngdints,2)

  ind2 = array_indices(onemap,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])

  ; Gaussian Smooth
  spec2 = GSMOOTH(spec,chansm,widfwhm=3)

  ; Put back in
  cube[xind,yind,*] = spec2

  ;stop

end
;head2 = head
velsm = abs(sxpar(head2,'CDELT3'))*chansm
strvelsm = strtrim(string(velsm,format='(F6.1)'),2)
SXADDHIST,'Velocity Gaussian smoothing - FWHM='+strtrim(chansm,2)+' channels = '+strvelsm+' km/s',head2

; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec[0:330])
  ;rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'


;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_vsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_vsm_rmsmap.fits',rmsmap,rmshead

stop

;-------------------------------------------
; Smooth with a spatial gaussian, FWHM=3
;-------------------------------------------
SMOOTHCUBE_POS:
nchan = n_elements(cube[0,0,*])
print,'SMOOTHING GRID IN POSITION - ',strtrim(nchan,2),' channels'

; Make the smoothing kernel
smpos = 3L ;2L ;3L
npix = 2 * fix( (3*smpos)/2 ) + 1    ;make # pixels odd.
psf = PSF_GAUSSIAN(np=npix,fwhm=smpos,/norm)

bd = where(reform(cube[*,*,0]) eq 0.0,nbd)

for i=0,nchan-1 do begin
  if (i+1) mod 100 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)

  map = reform(cube[*,*,i])

  ;; Set NANs to 0 so edges are okay
  ;bd = where(finite(map) eq 0,nbd)
  ;map[bd] = 0.0

  ;map2 = GSMOOTH(map,[3,3],widfwhm=3)
  map2 = CONVOL(map,psf,/center,/edge_truncate,/nan,/normalize)

  ; Set missing data back to NANs
  ;map2[bd] = !values.f_nan

  ; set back to 0.0
  map2[bd] = 0.0


  ; Put back in
  cube[*,*,i] = map2

end



; Add history statement to header
;head3 = head2
cdelt1 = abs(SXPAR(head2,'CDELT1'))
smposmin = smpos*cdelt1*60.0  ; in arcmin
strsmposmin = strtrim(string(smposmin,format='(F6.1)'),2)
strsmpos = strtrim(string(smpos,format='(F6.1)'),2)
SXADDHIST,'Spatial Gaussian smoothing - FWHM='+strsmpos+' pixels = '+strsmposmin+' arcmin',head2


; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec[0:330])
  ;rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_pvsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_pvsm_rmsmap.fits',rmsmap,rmshead

;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_psm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_psm_rmsmap.fits',rmsmap,rmshead



stop





;----------------------
;  OLD STUFF
;----------------------

; some problem ones:
; 518_ses25
; 638_ses26 (?)
; 588_ses37
; 617_ses
; ses39 and ses40

; Pass 1: session 1-40, 48
; Pass 2: session 41-


; A few issues:
; Pass 1:
; -MSgrid3_518, session 25
;   scans 8-10 are bad for session 25, need to not be used at
;   reduce_session_getfs level  FIXED
; -MSgrid3_638, session 27
;   scan 11 and 16 BAD, scan 14 and 17 are redos
;    mostly FIXED,  there are couple integrations that are still bad
; -MSgrid4_588, session 38. FINE, just some bright emission
; -MSgrid4_597, session 38. FINE, just some bright emission
;
; Pass 2: 
; -MSgrid3_590, session 47
;   mostly okay, a few bad integrations
;   This still shows up bad in the map, maybe due to bright
;   emission in the spectrum.

; MSgrid4_617_ses39, MSgrid4_617_ses40, MSgrid4_626_ses39
; MSgrid4_626_ses40
; the scatter in the binned spectrum above is a bit high, not sure why

;save,survey,file=dir+'grid_mstip_mag2_rfifix3_survey.dat'

stop

GRID_SURVEY:


; Now interpolate onto the final grid
;------------------------------------
;;ind1 = where(session lt 11,ind1)
;;ind2 = where(session ge 11,ind2)
;ind1 = where(survey.session lt 9 or (survey.session eq 9 and survey.scan le 21),nind1)
;ind2 = where( (survey.session gt 10 and survey.session lt 15) or $
;              (survey.session eq 10 and survey.scan ge 79) or $
;              (survey.session eq 15 and survey.scan lt 84),ningd2)
;ind1 = where(survey.session le 40 or survey.session eq 48,nind1)
;ind2 = where(survey.session gt 40 and survey.session ne 48,nind2)
;survey1 = survey[ind1]
;survey2 = survey[ind2]
;survey1 = survey




;  Scan           Source      Vel    Proc Seqn   RestF nIF nInt nFd     Az    El
;-------------------------------------------------------------------------------
; These are redo's for the first pass
; Session 9
;    22     MSTIP-survey      0.0 RALongM   10   1.420   1   74   1  413.9  43.3
;    23     MSTIP-survey      0.0 RALongM   11   1.420   1   74   1  415.3  50.3
;    24     MSTIP-survey      0.0 RALongM   20   1.420   1   74   1  413.9  47.5
;    25     MSTIP-survey      0.0 RALongM   33   1.420   1   74   1  413.1  54.5
;    26     MSTIP-survey      0.0 RALongM   42   1.420   1   74   1  412.1  51.6
;
; Session 10
;     8     MSTIP-survey      0.0 RALongM   43   1.420   1   74   1  -52.0  50.7
;     9     MSTIP-survey      0.0 RALongM   55   1.420   1   74   1  -50.6  48.7
;    10     MSTIP-survey      0.0 RALongM   56   1.420   1   74   1  -50.7  51.6
;
;   start of pass 2
;    79     MSTIP-survey      0.0 RALongM    1   1.420   1   74   1  -37.0  13.9
;    80     MSTIP-survey      0.0 RALongM    2   1.420   1   13   1  -39.2  16.3
;
; Session 11
;     8     MSTIP-survey      0.0 RALongM    2   1.420   1   74   1   51.8  68.5
;     9     MSTIP-survey      0.0 RALongM    3   1.420   1   74   1   41.8  74.2
;
; These are redo's for the second pass
; Session 15
;    85     MSTIP-survey      0.0 RALongM  124   1.420   1   74   1  320.9  31.0
;    86     MSTIP-survey      0.0 RALongM  125   1.420   1   74   1  324.3  25.5
;    87     MSTIP-survey      0.0 RALongM  138   1.420   1   74   1  323.2  29.0
;    88     MSTIP-survey      0.0 RALongM  139   1.420   1   74   1  326.9  23.7

;; Replace bad scans for pass 1
;;  procseqn=10
;;gd = where(survey.session eq 9 and survey.scan eq 22 and survey.procseqn eq 10,ngd)    ; redo
;bd = where(survey1.procseqn eq 10,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=11
;gd = where(survey.session eq 9 and survey.scan eq 23 and survey.procseqn eq 11,ngd)    ; redo
;bd = where(survey1.procseqn eq 11,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=20
;gd = where(survey.session eq 9 and survey.scan eq 24 and survey.procseqn eq 20,ngd)    ; redo
;bd = where(survey1.procseqn eq 20,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=33
;gd = where(survey.session eq 9 and survey.scan eq 25 and survey.procseqn eq 33,ngd)    ; redo
;bd = where(survey1.procseqn eq 33,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=42
;gd = where(survey.session eq 9 and survey.scan eq 26 and survey.procseqn eq 42,ngd)    ; redo
;bd = where(survey1.procseqn eq 42,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=43
;gd = where(survey.session eq 10 and survey.scan eq 8 and survey.procseqn eq 43,ngd)    ; redo
;bd = where(survey1.procseqn eq 43,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=55
;gd = where(survey.session eq 10 and survey.scan eq 9 and survey.procseqn eq 55,ngd)    ; redo
;bd = where(survey1.procseqn eq 55,nbd)                                                 ; bad
;survey1[bd] = survey[gd]
;;  procseqn=56
;gd = where(survey.session eq 10 and survey.scan eq 10 and survey.procseqn eq 56,ngd)    ; redo
;bd = where(survey1.procseqn eq 56,nbd)                                                  ; bad
;survey1[bd] = survey[gd]
;
;; Remove extra row 2 for pass 2
;bd = where(survey2.session eq 11 and survey2.scan eq 8,nbd)
;remove,bd,survey2
;
;; Replace bad scans for pass 2
;;  procseqn=124
;gd = where(survey.session eq 15 and survey.scan eq 85 and survey.procseqn eq 124,ngd)    ; redo
;bd = where(survey2.procseqn eq 124,nbd)                                                  ; bad
;survey2[bd] = survey[gd]
;;  procseqn=125
;gd = where(survey.session eq 15 and survey.scan eq 86 and survey.procseqn eq 125,ngd)    ; redo
;bd = where(survey2.procseqn eq 125,nbd)                                                  ; bad
;survey2[bd] = survey[gd]
;;  procseqn=138
;gd = where(survey.session eq 15 and survey.scan eq 87 and survey.procseqn eq 138,ngd)    ; redo
;bd = where(survey2.procseqn eq 138,nbd)                                                  ; bad
;survey2[bd] = survey[gd]
;;  procseqn=139
;gd = where(survey.session eq 15 and survey.scan eq 88 and survey.procseqn eq 139,ngd)    ; redo
;bd = where(survey2.procseqn eq 139,nbd)                                                  ; bad
;survey2[bd] = survey[gd]

;stop

; I think the last 5 scans in session 9 are redos for bad scans
; the first three scans in session 10 are also redos for bad scans
; The last two scans of session 10 are rows 1+2 of the second pass
; session 11 starts with row 2
; Regular scans for pass 1 end at with: session 9 scan 21
; Regular scans for pass 2 start with: session 10 scan 79


; MAKE MAP WITH VELOCITY CUT AND SIGMA FILTERING

stop

;; Normalize
;for j=0,nchan-1 do bigcube[*,*,j]/=(numobs>1)
;
;tot2 = total(bigcube[*,*,150:210],3)
;displayc,tot2,min=-0.8,max=0.8,/xflip

tot2 = total(bigcube2[*,*,150:210],3)
displayc,tot2,min=-0.8,max=0.8,/xflip
fits_write,'grid_mstip_mag2_rfifix3_pass2.fits',tot2,head

; Combine pass 1+2
tot = (tot1+tot2)/(numobs>1)
displayc,gsmooth(tot,3),min=-0.5,max=0.5,/xflip
fits_write,'grid_mstip_mag2_rfifix3_comb.fits',tot,head

; Save some plots
ps_open,'grid_mstip_mag2_rfifix3_pass1',/color,thick=4,/encap
displayc,gsmooth(tot1,3),min=-0.5,max=0.5,/xflip,tit='Pass 1'
ps_close
ps2jpg,'grid_mstip_mag2_rfifix3_pass1.eps',/eps

ps_open,'grid_mstip_mag2_rfifix3_pass2',/color,thick=4,/encap
displayc,gsmooth(tot2,3),min=-0.5,max=0.5,/xflip,tit='Pass 2'
ps_close
ps2jpg,'grid_mstip_mag2_rfifix3_pass2.eps',/eps

ps_open,'grid_mstip_mag2_rfifix3_comb',/color,thick=4,/encap
displayc,gsmooth(tot,3),min=-0.5,max=0.5,/xflip,tit='Combined'
ps_close
ps2jpg,'grid_mstip_mag2_rfifix3_comb.eps',/eps

; Positions for follow-up Mapping
; X=161, Y=188  RA=00:22:41.237  DEC=68:05:04.706
; IT SEEMS OFF A BIT
; 5.5495690       67.910817
; 00:22:11.896    67:54:38.952
; This seems better

; roughly 20x15 grid 10s integration
; but maybe larger
; could do it a couple of times.

; 51 127
; 352.26528       74.969817
; 23:29:03.668  74:58:11.341
; 25x15

; 133, 116
; 352.13687       69.671342
; 23:28:32.848  69:40:16.832

; Positions to get deep pointings for
; X=228, Y=93   RA=23:23:45.971  DEC=63:23:11.111  TOTmax=2.6   NHI=7.7E18  BRIGHT! don't need followup!
; X=161, Y=188  RA=00:22:41.237  DEC=68:05:04.706  TOTmax=0.80  Tmax=0.02K??
; X=198, Y=88   RA=23:17:09.360  DEC=65:13:07.590  TOTmax=0.7   Tmax=0.02K??
; X=141, Y=187  RA=00:22:54.131  DEC=69:20:01.667  TOTmax=0.52  Tmax=0.02K??
; X=79,  Y=84   RA=22:53:43.060  DEC=72:38:22.446  TOTmax=1.2   Tmax=0.03K
; X=83,  Y=108  RA=23:15:28.055  DEC=72:45:04.959  TOTmax=0.75  Tmax=0.02K??
; X=50,  Y=134  RA=23:36:08.863  DEC=75:04:50.649  TOTmax=0.35  Tmax=0.02K??
; X=15,  Y=103, RA=22:56:14.209  DEC=76:57:31.334  TOTmax=0.32  Tmax=0.03K??
; X=173, Y=111  RA=23:29:09.535  DEC=67:05:01.193  TOTmax=0.36  Tmax=0.02K??
; X=155, Y=237  RA=00:57:52.718  DEC=68:00:17.358  TOTmax=0.40  Tmax=0.03K??
; X=190, Y=214  RA=00:38:46.033  DEC=66:08:09.817  TOTmax=0.51  Tmax=0.05K??
; X=316, Y=170  RA=00:11:08.341  DEC=58:23:33.681  TOTmax=0.52  Tmax=0.03K??

xarr = [228,161,198,141,79,83,50,15,173,155,190,316]
yarr = [93,188,88,187,84,108,134,103,111,237,214,170]

stop


; Now divide by numobs
;for j=0,nchan-1 do bigcube[*,*,j] /= (numobs > 1)


tot2 = total(bigcube,2)
tot3 = total(bigcube[*,*,150:210],3)
tot3b = total(bigcube[*,*,50:210],3)
displayc,tot3,min=-0.8,max=0.8,/xflip

stop

; Cut out the main survey region
;xlo = 172
;xhi = 449
;ylo = 25
;yhi = 147
;zlo = 58
;zhi = 250
;xlo = 200 ;250
;xhi = 440 ;434
;ylo = 0
;yhi = 152
xlo = 200
xhi = 650
ylo = 0
yhi = 249
zlo = 58
zhi = 280 ;250

fits_arrays,head,mlon,mlat,vel
;ml = mlon[185:*]
;mb = mlat[25:143]
;vl = vel[45:250]
;cube = bigcube[185:*,25:143,45:250]
ml = mlon[xlo:xhi]
mb = mlat[ylo:yhi]
vl = vel[zlo:zhi]
cube = bigcube[xlo:xhi,ylo:yhi,zlo:zhi]
sz = size(cube)

tot1 = total(cube,1)
tot2 = total(cube,2)
tot3 = total(cube,3)

; Make new header for this subcube
head2 = head
sxaddpar,head2,'CTYPE1','MLON-CAR'
sxaddpar,head2,'CRPIX1',1
sxaddpar,head2,'CRVAL1',ml[0]
sxaddpar,head2,'CTYPE2','MLAT-CAR'
sxaddpar,head2,'CRPIX2',1
sxaddpar,head2,'CRVAL2',mb[0]
sxaddpar,head2,'CTYPE3','VELO-LSR'
sxaddpar,head2,'CRPIX3',1
sxaddpar,head2,'CRVAL3',vl[0]

;stop

; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

FITS_WRITE,dir+'grid_mstip_mag2_rfifix3.fits',cube,head2
FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_rmsmap.fits',rmsmap,rmshead


; Copied from /net/halo/dln5q/gbt/grid_gbt_gal.pro
;goto,smoothcube_pos

;----------------------
; Smooth the datacube
;----------------------
;SMOOTHCUBE:


; Smooth each integration with a Gaussian in velocity
gdints = where(reform(cube[*,*,0]) ne 0.0,ngdints)
print,'SMOOTHING GRID IN VELOCITY - ',strtrim(ngdints,2),' positions'
onemap = reform(cube[*,*,0])
chansm = 10L
for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),'/',strtrim(ngdints,2)

  ind2 = array_indices(onemap,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])

  ; Gaussian Smooth
  spec2 = GSMOOTH(spec,chansm,widfwhm=3)

  ; Put back in
  cube[xind,yind,*] = spec2

  ;stop

end
;head2 = head
velsm = abs(sxpar(head2,'CDELT3'))*chansm
strvelsm = strtrim(string(velsm,format='(F6.1)'),2)
SXADDHIST,'Velocity Gaussian smoothing - FWHM='+strtrim(chansm,2)+' channels = '+strvelsm+' km/s',head2

; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'


;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_vsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_vsm_rmsmap.fits',rmsmap,rmshead

stop

;-------------------------------------------
; Smooth with a spatial gaussian, FWHM=3
;-------------------------------------------
;SMOOTHCUBE_POS:
nchan = n_elements(cube[0,0,*])
print,'SMOOTHING GRID IN POSITION - ',strtrim(nchan,2),' channels'

; Make the smoothing kernel
smpos = 3L ;2L ;3L
npix = 2 * fix( (3*smpos)/2 ) + 1    ;make # pixels odd.
psf = PSF_GAUSSIAN(np=npix,fwhm=smpos,/norm)

bd = where(reform(cube[*,*,0]) eq 0.0,nbd)

for i=0,nchan-1 do begin
  if (i+1) mod 100 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)

  map = reform(cube[*,*,i])

  ;; Set NANs to 0 so edges are okay
  ;bd = where(finite(map) eq 0,nbd)
  ;map[bd] = 0.0

  ;map2 = GSMOOTH(map,[3,3],widfwhm=3)
  map2 = CONVOL(map,psf,/center,/edge_truncate,/nan,/normalize)

  ; Set missing data back to NANs
  ;map2[bd] = !values.f_nan

  ; set back to 0.0
  map2[bd] = 0.0


  ; Put back in
  cube[*,*,i] = map2

end



; Add history statement to header
;head3 = head2
cdelt1 = abs(SXPAR(head2,'CDELT1'))
smposmin = smpos*cdelt1*60.0  ; in arcmin
strsmposmin = strtrim(string(smposmin,format='(F6.1)'),2)
strsmpos = strtrim(string(smpos,format='(F6.1)'),2)
SXADDHIST,'Spatial Gaussian smoothing - FWHM='+strsmpos+' pixels = '+strsmposmin+' arcmin',head2


; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(cube)
tot3 = total(cube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(cube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_pvsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_pvsm_rmsmap.fits',rmsmap,rmshead

;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_psm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag2_rfifix3_psm_rmsmap.fits',rmsmap,rmshead


stop

; I don't see anything in the CHVC424 spot


; Bin spatially and in velocity
;bincube2 = REBIN(cube2[*,0:81,*],sz[1]/2,41,sz[3])
;btot1 = total(bincube2,1)
;btot2 = total(bincube2,2)
;btot3 = total(bincube2,3)

; spectral smmothing
;smcube2 = cube*0
;fwhm = 2.0 ; 5.0
;sz = size(cube)
;for i=0,sz[1]-1 do begin
;  for j=0,sz[2]-1 do begin
;    smspec = GSMOOTH(reform(cube[i,j,*]),fwhm)
;    smcube2[i,j,*] = smspec
;  end
;end

; spatial smoothing
smcube3 = cube*0
fwhm2 = 2.0 ;3
for i=0,sz[3]-1 do begin
  ;im = reform(smcube2[*,*,i])
  im = reform(cube[*,*,i])
  smim = GSMOOTH(im,fwhm2)
  smcube3[*,*,i] = smim
end

smtot1 = total(smcube3,1)
smtot2 = total(smcube3,2)
smtot3 = total(smcube3,3)

smtot3b = total(smcube3[*,*,110:176],3)
smtot3c = total(smcube3[*,*,110:*],3)
smtot3d = total(smcube3[*,*,90:180],3)
;smtot3c = total(smcube3[*,*,113:*],3)
displayc,smtot3


stop

col1 = tot3*1.61*1.83e18 / 1e18

;col3b = tot3b*1.61*1.83e18 / 1e18
smcol1 = smtot3*1.61*1.83e18 / 1e18
smcol2 = smtot3b*1.61*1.83e18 / 1e18
smcol3 = smtot3c*1.61*1.83e18 / 1e18
smcol4 = smtot3d*1.61*1.83e18 / 1e18
; the noise level in the column density image is 1.3E18 cm^-2

; PLOTS,  ALL emission
ps_open,'gbt_mstip_mag_all1',/color,thick=4,/encap
device,/inches,xsize=15,ysize=12
displayc,transpose(smtot1),vl,mb,min=-1.5,max=1.5,xtit='VLSR (km/s)',ytit='MLAT (deg)',$
         tit='MLAT-VLSR (all MLON)',position=[0.02,0.5,0.4,1.01],colcharsize=0.8
displayc,smtot2,ml,vl,min=-1.5,max=1.5,/xflip,xtit='MLON (deg)',ytit='VLSR (km/s)',$
         tit='MLON-VLSR (all MLAT)',position=[0.4,0.0,1.0,0.51],/noerase,colcharsize=0.8
displayc,smcol1,ml,mb,min=-3,max=4,xtit='MLON (deg)',ytit='MLAT (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -570<VLSR<-240 km/s',position=[0.4,0.5,1.0,1.01],/noerase
;xyouts,-139,17,'CHVC 402',align=0.5,charsize=1.5,co=255,/data
;xyouts,-143,13,'CHVC 391',align=0.0,charsize=1.5,co=255,/data
ps_close
ps2gif,'gbt_mstip_mag_all1.eps',/eps


; MS plots
ps_open,'gbt_mstip_mag_ms1',/color,thick=4,/encap
device,/inches,xsize=15,ysize=12
displayc,transpose(smooth(smtot1,[2,2],/edge_truncate)),vl,mb,min=-1.0,max=1.0,xtit='VLSR (km/s)',ytit='MLAT (deg)',$
         tit='MLAT-VLSR (all MLON)',position=[0.02,0.5,0.4,1.01],colcharsize=0.8
;oplot,[0,0]+vl[110],minmax(mb),linestyle=2,co=0,thick=4
;oplot,[0,0]+vl[176],minmax(mb),linestyle=2,co=0,thick=4
oplot,[0,0]+vl[70],minmax(mb),linestyle=2,co=0,thick=4
oplot,[0,0]+vl[180],minmax(mb),linestyle=2,co=0,thick=4
;oplot,[0,0]+vl[110],minmax(mb),linestyle=2,co=255
;oplot,[0,0]+vl[176],minmax(mb),linestyle=2,co=255
;arrow,50.62,-463.15,51.19,-424.31,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255

displayc,smooth(smtot2,[2,2],/edge_truncate),ml,vl,min=-0.7,max=0.7,/xflip,xtit='MLON (deg)',ytit='VLSR (km/s)',$
         tit='MLON-VLSR (all MLAT)',position=[0.4,0.0,1.0,0.51],/noerase,colcharsize=0.8
;oplot,minmax(ml),[0,0]+vl[110],linestyle=2,co=0
;oplot,minmax(ml),[0,0]+vl[176],linestyle=2,co=0
oplot,minmax(ml),[0,0]+vl[90],linestyle=2,co=0
oplot,minmax(ml),[0,0]+vl[180],linestyle=2,co=0

;arrow,359.22,-478.388,358.47,-441.08,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
;displayc,smcol2,ml,mb,min=-1.5,max=1.5,xtit='MLON (deg)',ytit='MLAT (deg)',/xflip,colcharsize=0.8,$
;         tit='Column Density (10!u18!n cm!u-2!n) for -393<VLSR<-287 km/s',position=[0.4,0.5,1.0,1.01],/noerase
displayc,smcol4,ml,mb,min=-1.5,max=1.5,xtit='MLON (deg)',ytit='MLAT (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -393<VLSR<-287 km/s',position=[0.4,0.5,1.0,1.01],/noerase
;xyouts,-135,13.9,'BT04',align=0.5,charsize=1.5,charthick=4,co=0,/data
;xyouts,-144.8,12.4,'MS detections',align=0.5,charsize=1.5,charthick=4,co=0,/data
;arrow,-144.8,12.8,-142.0,13.5,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
;arrow,-144.8,12.8,-145.0,14.2,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
;xyouts,-143.8,18.9,'MS???',align=0.5,charsize=1.5,charthick=4,co=0,/data
;arrow,-143.8,18.7,-143.5,16.5,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
;arrow,-143.8,18.7,-146.5,18.0,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
;;xyouts,-139,17,'CHVC 402',align=0.5,charsize=1.5,co=255,/data
;;xyouts,-143,13,'CHVC 391',align=0.0,charsize=1.5,co=255,/data
ps_close
ps2gif,'gbt_mstip_mag_ms1.eps',/eps

stop

; OLD STUFF
;--------------

; The new detection
; Vlsr vs. DEC for ALL RA
tot1c = total(smcube3[0:35,*,*],1)
tot2c = total(smcube3[*,65:*,*],2)
;tot3c = total(smcube3[*,*,150:178],3)
tot3c = total(smcube3[*,*,150:213],3)
col3c = tot3c*1.61*1.83e18 / 1e18
ps_open,'gbt_mstip_mag_new1',/color,thick=4,/encap
device,/inches,xsize=12.3,ysize=12
displayc,tot1c[*,45:250],dec1,vel[45:250],/z,xtit='DEC (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (352.89<RA<356.66)',position=[0.02,0,0.5,0.5],colcharsize=0.8
arrow,53.13,-447.26,53.525,-392.535,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
displayc,tot2c[*,45:250],ra1,vel[45:250],/z,/xflip,xtit='RA (deg)',ytit='VLSR (km/s)',$
         tit='Integrated Intensity (53.33<DEC<54.47)',position=[0.02,0.5,0.5,1.0],/noerase,colcharsize=0.8
arrow,354.42,-444.61,353.9,-400.48,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
displayc,col3c,ra1,dec1,/z,xtit='RA (deg)',ytit='DEC (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -400<VLSR<-300 km/s',position=[0.5,0.5,1.0,1.0],/noerase
arrow,354.539,52.308,353.992,53.310,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
xyouts,0.75,0.3,'Centered near:',align=0.5,charsize=1.5,co=0,/norm
xyouts,0.75,0.27,'RA=353.60, DEC=53.840, VLSR=-374.0 km/s',align=0.5,charsize=1.5,co=0,/norm
ps_close
ps2gif,'gbt_mstip_mag_new1.eps',/eps


; The new detection
; Vlsr vs. DEC for ALL RA
tot1c = total(smcube3[0:35,*,*],1)
tot2c = total(smcube3[*,65:*,*],2)
tot3c = total(smcube3[*,*,150:178],3)
;tot3c = total(smcube3[*,*,150:213],3)
col3c = tot3c*1.61*1.83e18 / 1e18
ps_open,'gbt_mstip_new3b',/color,thick=4,/encap
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
ps2gif,'gbt_mstip_new3b.eps',/eps

;; The new detection
;; Vlsr vs. DEC for ALL RA
;tot1c = total(smcube3[0:35,*,*],1)
;tot2c = total(smcube3[*,65:*,*],2)
;tot3c = total(smcube3[*,*,150:178],3)
;col3c = tot3c*1.61*1.83e18 / 1e18
;ps_open,'gbt_mstip_new3',/color,thick=4,/encap
;device,/inches,xsize=12.3,ysize=12
;displayc,tot1c[*,45:250],dec1,vel[45:250],/z,xtit='DEC (deg)',ytit='VLSR (km/s)',$
;         tit='Integrated Intensity (352.89<RA<356.66)',position=[0.02,0,0.5,0.5],colcharsize=0.8
;arrow,53.13,-447.26,53.525,-392.535,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
;displayc,tot2c[*,45:250],ra1,vel[45:250],/z,/xflip,xtit='RA (deg)',ytit='VLSR (km/s)',$
;         tit='Integrated Intensity (53.33<DEC<54.47)',position=[0.02,0.5,0.5,1.0],/noerase,colcharsize=0.8
;arrow,354.42,-444.61,353.9,-400.48,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
;displayc,col3c,ra1,dec1,/z,xtit='RA (deg)',ytit='DEC (deg)',/xflip,colcharsize=0.8,$
;         tit='Column Density (10!u18!n cm!u-2!n) for -400<VLSR<-350 km/s',position=[0.5,0.5,1.0,1.0],/noerase
;arrow,354.539,52.308,353.992,53.310,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=8,color=255
;xyouts,0.75,0.3,'Centered near:',align=0.5,charsize=1.5,co=0,/norm
;;xyouts,0.75,0.27,'RA=353.60, DEC=53.840, VLSR=-374.0 km/s',align=0.5,charsize=1.5,co=0,/norm
;ps_close
;ps2gif,'gbt_mstip_new3.eps',/eps

stop

end
