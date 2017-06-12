pro grid_gbt_gal,allfinal,bigcube,head

; Put the GBT data on a galactic cartesian grid

;dir = '/local/dln5q/research/observing/gbt/'
;dir = '/net/halo/dln5q/gbt/'
dir = '/net/halo/dln5q/gbt/data/'

;restore,dir+'programs/chanstr.dat'
;restore,dir+'data/vel.dat'
restore,dir+'vel.dat'

setdisp
psym8

;tag = 'red3'
tag = 'red4'
;tag = 'red1'

v2 = REBIN(v[600:9599],900)
if tag eq 'red1' then v2 = REBIN(v[4500:9499],500)


;files = file_search(dir+'data/MS*_'+tag+'.dat',count=nfiles)
;
;base = file_basename(files,'_'+tag+'.dat')
;allsources = strmid(base,0,12)
;ui = uniq(allsources,sort(allsources))
;sources = allsources[ui]
;nsources = n_elements(sources)
;
;print,strtrim(nfiles,2),' files'
;print,strtrim(nsources,2),' unique sources'


;if n_elements(allfinal) eq 0 then restore,dir+'combine_gbt_cubes_'+tag+'.dat'
;goto,MAKECUBE

if n_elements(bigcube) eq 0 then FITS_READ,dir+'grid_gbt_gal.fits',bigcube,head
;goto,SKIP1
goto,SMOOTHCUBE
;goto,SMOOTHCUBE_POS


undefine,allfinal

for i=0,nsources-1 do begin

  isource = sources[i]
  print,strtrim(i+1,2),'/',strtrim(nsources,2),' ',isource
  gd = where(allsources eq isource,ngd)
  print,strtrim(ngd,2),' file(s)'

  ; Restore and concatenate the files
  undefine,all,final
  nscan = 0
  for j=0,ngd-1 do begin
    ifile = files[gd[j]]
    restore,ifile
    
    ; Convert to new format
    if tag eq 'red1' then begin
      nstr1 = n_elements(str1)
      dum = {object:'',file:'',glon:0.0d0,glat:0.0d0,scan:0L,procseqn:0,int:0,comb_corr:fltarr(500)}
      final = replicate(dum,nstr1)
      final.object = strtrim(str1.object,2)
      fitsfile = file_dirname(ifile)+'/'+file_basename(ifile,'_'+tag+'.dat')+'.fits'
      final.file = fitsfile
      final.glon = str1.crval2
      final.glat = str1.crval3
      final.scan = str1.scan
      final.procseqn = str1.procseqn
      final.int = str1.int
      for k=0,nstr1-1 do final[k].comb_corr = REBIN(str1[k].diff,500)

      ; New V2 array, 4500-9499
      if i eq 0 then v2 = REBIN(v[4500:9499],500)

    endif

    ui = uniq(final.scan,sort(final.scan))
    nsc = n_elements(ui)
    nfinal = n_elements(final)
    print,file_basename(ifile),' Nscan=',strtrim(nsc,2),' Nints=',strtrim(nfinal,2)
    PUSH,all,final
    nscan = nscan + nsc
  end
  final = all

  nchan = n_elements(final[0].comb_corr)

  ;stop

  ;------------------
  ;---- KLUDGES -----
  ;------------------

  ; There is a 1 scan overlap
  if isource eq 'MS088.5-34.0' then begin
    bd = where(final.file eq '/local/dln5q/research/observing/gbt/data/MS088.5-34.0_ses23_s122-125.fits' and $
               final.scan eq 125,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; Observed twice and 2 scan overlap
  if isource eq 'MS090.6-36.0' then begin
    left = final
    gd1 = where(stregex(final.file,'MS090.6-36.0_ses21_s7-36.fits',/boolean) eq 1)
    final1 = final[gd1]
    gd2 = where(stregex(final.file,'MS090.6-36.0_ses22_s13-42.fits',/boolean) eq 1)
    final2 = final[gd2]
    REMOVE,[gd1,gd2],left

    final = final1
    final.comb_corr = (final1.comb_corr + final2.comb_corr)*0.5  ; combine the data

    final = [final,left]

    ; 2 scan overlap
    bd = where(stregex(final.file,'MS090.6-36.0_ses20_s99-107.fits',/boolean) eq 1 and final.procseqn ge 8,nbd)
    REMOVE,bd,final

    nscan = 37
  endif

  ; half-scan overlap
  if isource eq 'MS093.1-36.0' then begin
    bd = where(stregex(final.file,'MS093.1-36.0_ses24_s80-93.fits',/boolean) eq 1 and final.procseqn eq 14,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; missing a scan, take average of neighbors
  if isource eq 'MS094.3-46.0' then begin
    gd1 = where(stregex(final.file,'MS094.3-46.0_ses7_s68-77.fits',/boolean) eq 1)
    final1 = final[gd1]
    gd2 = where(stregex(final.file,'MS094.3-46.0_ses7_s93-118.fits',/boolean) eq 1)
    final2 = final[gd2]
    ind1 = where(final1.procseqn eq 10,nind1)
    ind2 = where(final2.procseqn eq 12,nind1)

    ; Make averaged scan
    new = final1[ind1]
    new.comb_corr = (final1[ind1].comb_corr+final2[ind2].comb_corr)*0.5
    new.glat = (final1[ind1].glat + final2[ind2].glat)*0.5
    new.procseqn = 11

    final = [final1,new,final2]

    undefine,final1,final2,new
    nscan = 37
  endif

  ; 1.5 scan overlap
  if isource eq 'MS094.9-42.0' then begin
    bd = where(stregex(final.file,'MS094.9-42.0_ses6_s95-117.fits',/boolean) eq 1 and final.procseqn ge 22,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 2 scan overlap
  if isource eq 'MS097.2-46.0' then begin
    bd = where(stregex(final.file,'MS097.2-46.0_ses5_s98-130.fits',/boolean) eq 1 and final.procseqn ge 32,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 int overlap
  if isource eq 'MS097.9-38.0' then begin
    bd = where(stregex(final.file,'MS097.9-38.0_ses22_s45-53.fits',/boolean) eq 1 and final.procseqn eq 9,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; Bad data
  if isource eq 'MS100.1-46.0' then begin
    bd = where(stregex(final.file,'MS100.1-46.0_ses10_s6-16.fits',/boolean) eq 1,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 2 scan overlap
  if isource eq 'MS100.2-44.0' then begin
    bd = where(stregex(final.file,'MS100.2-44.0_ses8_s264-279.fits',/boolean) eq 1 and final.procseqn ge 15,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 scan overlap
  if isource eq 'MS103.0-42.0' then begin
    bd = where(stregex(final.file,'MS103.0-42.0_ses15_s62-81.fits',/boolean) eq 1 and final.procseqn eq 20,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 scan overlap
  if isource eq 'MS103.0-48.0' then begin
    bd = where(stregex(final.file,'MS103.0-48.0_ses16_s102-137.fits',/boolean) eq 1 and final.procseqn ge 35,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 scan overlap
  if isource eq 'MS103.0-50.0' then begin
    bd = where(stregex(final.file,'MS103.0-50.0_ses17_s14-32.fits',/boolean) eq 1 and final.procseqn ge 19,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 scan overlap
  if isource eq 'MS105.7-44.0' then begin
    bd = where(stregex(final.file,'MS105.7-44.0_ses18_s28-43.fits',/boolean) eq 1 and final.procseqn eq 22,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; 1 scan overlap
  if isource eq 'MS105.8-46.0' then begin
    bd = where(stregex(final.file,'MS105.8-46.0_ses18_s45-72.fits',/boolean) eq 1 and final.procseqn eq 6,nbd)
    REMOVE,bd,final
    nscan = 37
  endif

  ; missing a scan, take average of neighbors
  if isource eq 'MS095.3-38.0' then begin
    gd1 = where(final.procseqn le 25,ngd1)
    final1 = final[gd1]
    gd2 = where(final.procseqn ge 27,ngd2)
    final2 = final[gd2]
    ind1 = where(final1.procseqn eq 25,nind1)
    ind2 = where(final2.procseqn eq 27,nind1)

    ; Make averaged scan
    new = final1[ind1]
    new.comb_corr = (final1[ind1].comb_corr+final2[ind2].comb_corr)*0.5
    new.glat = (final1[ind1].glat + final2[ind2].glat)*0.5
    new.procseqn = 26

    final = [final1,new,final2]

    undefine,final1,final2,new
    nscan = 37
  endif


  ; Add to final structure
  dum = {object:'',file:'',glon:0.0d0,glat:0.0d0,scan:0L,procseqn:0L,int:0L,data:fltarr(nchan)}
  nfinal = n_elements(final)
  temp = REPLICATE(dum,nfinal)
  STRUCT_ASSIGN,final,temp
  temp.data = final.comb_corr
  PUSH,allfinal,temp

  ;stop

end

; 237MB
;save,allfinal,file='../data/combine_gbt_cubes_'+tag+'.dat'

stop

MAKECUBE:

; Unique sources
allsources = strmid(allfinal.object,0,12)
ui = uniq(allsources,sort(allsources))
sources = allsources[ui]
nsources = n_elements(sources)

;stop

;-------------------------------------------------------
; SETUP THE HEADER
;-------------------------------------------------------

; Set up the WCS information for the LARGE image/cube
;step = 0.0583d
;undefine,head
;cube = fltarr(477,380,900)
;nchan = n_elements(allfinal[0].data)
nchan = 900 ;1
;bigcube = fltarr(371,384,nchan)
;nx = 1700  ; 1750
;ny = 1600
;bigcube = fltarr(nx,ny,10)
;bigcube[*,*,*] = !values.f_nan   ; default all bad
;MKHDR,head,bigcube
;SXADDPAR,head,'NAXIS3',nchan

; Just the GBT data
;gal2mag,allfinal.glon,allfinal.glat,mlon,mlat,wrap=0
;mag_head,head
;adxy,head,mlon,mlat,x,y

; 80.02 < GLON < 107.82
; -51.05 < GLAT < -28.95
step = 0.05830d0
nx = 480
ny = 383  ;382

; For the GBT data we only need:
; X: 666-1061
; Y: 272-735
;nx = 1061-666+1L
;ny = 735-272+1L
bigcube = fltarr(nx,ny,nchan)
undefine,head
MKHDR,head,bigcube

xref = nx/2
yref = ny/2
glon_ref = 93.920d0
glat_ref = -40.00d0

;stop

; Need to use celestial coordinates
;                                         spatial  vel step  
; GBT      334<RA<3.3, +8.8<DEC<+22.9,  3.5'  1.6 km/s   GAL LSR
; Arecibo  319<RA<360.2,   -2<DEC<+31     3.0'  1.4 km/s   CEL LSR
; Braun    333<RA<48.5,  +19<DEC<+60.0   15'   8.3 km/s   CEL-SIN  LGSR
; Bruens   337<RA<10.0,  -25<DEC<+20     10'   0.82 km/s  GAL-ZEA LSR 
; Use 3 arcmin spatial scale, and ~1.5 or 5 km/s velocity scale

; 319<RA<48.5
; -25<DEC<+60.0
;xref = 650L  ;nx/2
;yref = 760L  ;ny/2
;ra_ref = 354.0  ;4.0
;dec_ref = 18.5

;xoffset = 666L
;yoffset = 272L
;xref = 850L-xoffset
;yref = 800L-yoffset
;;xref = 850L ;790L  ; 875
;;yref = 800L ;750L  ; 800
;mlon_ref = 253.0d0 ;250.0d0  ;260.0d0
;mlat_ref = 15.0d0  ;12.50d0  ;10.0d0

; it looks like I need to go
; 50-1657 in Y
; -200 to 1556 in X

;step = 0.05  ; 3'
;vstep = 1.5  ; 1.5 km/s
;vmin = -548.50  ;-100.0
vstep = -1.61031384824d0
vmin = 521.667678579d0


; Braun used SIN with CDELT
; Stanimirovic used NO projection with CDELT
; Bruens used ZEA with CDELT
SXADDPAR,head,'CDELT1',step
SXADDPAR,head,'CRPIX1',xref+1L
SXADDPAR,head,'CRVAL1',glon_ref
SXADDPAR,head,'CTYPE1','GLON'
SXADDPAR,head,'CDELT2',step
SXADDPAR,head,'CRPIX2',yref+1L
SXADDPAR,head,'CRVAL2',glat_ref
SXADDPAR,head,'CTYPE2','GLAT'
SXADDPAR,head,'CDELT3',vstep
SXADDPAR,head,'CRPIX3',1L
SXADDPAR,head,'CRVAL3',vmin
SXADDPAR,head,'CTYPE3','VELO-LSR'

;adxy,head,allfinal.glon,allfinal.glat,x,y

sz = size(bigcube)
nx = sz[1]
ny = sz[2]
x = findgen(sz[1])#(fltarr(sz[2])+1.0)
y = (fltarr(sz[1])+1.0)#findgen(sz[2])
xyad,head,x,y,a,d

vfinal = findgen(nchan)*vstep+vmin

; Initialize RMS map
rmsmap = fltarr(nx,ny)
rmsmap[*,*] = 1d30  ; all bad to begin with


;-----------------------
;   GBT DATA
;-----------------------

; Loop through the unique sources
;for i=48,nsources-1 do begin
for i=0,nsources-1 do begin

  isource = sources[i]
  print,strtrim(i+1,2),'/',strtrim(nsources,2),' ',isource
  gd = where(allfinal.object eq isource,ngd)
  origfinal = allfinal[gd]

  ; Start new 
  dum = {object:'',file:'',glon:0.0d0,glat:0.0d0,ra:0.0d0,dec:0.0d0,mlon:0.0d0,mlat:0.0d0,$
         scan:0L,procseqn:0L,int:0L,data:fltarr(nchan)}
  final = REPLICATE(dum,ngd)
  STRUCT_ASSIGN,origfinal,final
  ;GLACTC,ra,dec,2000.0,origfinal.glon,origfinal.glat,2,/deg
  ;final.ra = ra
  ;final.dec = dec
  ;GAL2MAG,origfinal.glon,origfinal.glat,mlon,mlat,wrap=0
  ;final.mlon = mlon
  ;final.mlat = mlat

  ; Spline onto the final velocity scale
  ;for j=0,ngd-1 do final[j].data=CSPLINE(v2,origfinal[j].data,vfinal)

  ui = uniq(final.procseqn,sort(final.procseqn))
  nscan = n_elements(ui)

  ; Calculate RMS for the entire brick
  ;temp = final.data
  ;temp[250:420,*] = !values.f_nan
  ;rms = mad(temp)
  ;undefine,temp
  rms = MAD(final.data)

  ;restore,files[i]
  nfinal = n_elements(final)
  print,'Nints = ',strtrim(nfinal,2)
  ;file = file_basename(files[i],'_red3.dat')

  ;ui = uniq(final.scan,sort(final.scan))
  ;nscan = n_elements(ui)

  mngl = mean(final.glon)
  mngb = mean(final.glat)
  rotsphcen,final.glon,final.glat,mngl,mngb,xi,eta,/gnomic
  step = abs(median(slope(xi[0:36])))
  xx1 = xi/step
  yy1 = eta/step
  xx2 = round( xx1-min(xx1) )
  yy2 = round( yy1-min(yy1) )
  totim = fltarr(37,nscan)
  peakim = fltarr(37,nscan)

  ; Make the cube
  nchan = n_elements(final[0].data)
  cube = fltarr(37,nscan,nchan)
  nxx = n_elements(xx1)
  for j=0L,nxx-1 do cube[xx2[j],yy2[j],*]=final[j].data
  ; make it a structure, or add header

  glonarr = fltarr(37,nscan)
  glonarr[xx2,yy2] = final.glon
  glatarr = fltarr(37,nscan)
  glatarr[xx2,yy2] = final.glat
  glon = median(glonarr,dim=2)
  glat = median(glatarr,dim=1)

  glsteparr = (glonarr[1:36,*]-glonarr[0:35,*])*cos(glatarr[1:36,*]/!radeg)
  gbsteparr = glatarr[*,1:nscan-1]-glatarr[*,0:nscan-1]
  step = median( [ (glsteparr)[*], (gbsteparr)[*] ] )
  glstep = median(glsteparr)
  gbstep = median(gbsteparr)


  ; Extend by 1 pixel to deal with points outside the boundary
  ;map2 = fltarr(39,nscan+2)
  ;map2[1:37,1:nscan] = map             ; center map
  ;map2[0,1:nscan] = map[0,*]           ; left edge
  ;map2[38,1:nscan] = map[36,*]         ; right edge
  ;map2[1:37,0] = map[*,0]              ; bottom edge
  ;map2[1:37,nscan+1] = map[*,nscan-1]  ; top edge
  ;map2[0,0] =  map[0,0]                ; left-bottom corner
  ;map2[0,nscan+1] = map[0,nscan-1]     ; left-top corner
  ;map2[38,0] = map[36,0]               ; right-bottom corner
  ;map2[38,nscan+1] = map[36,nscan-1]   ; top-right corner

  glonarr2 = fltarr(39,nscan+2)
  glonarr2[1:37,1:nscan] = glonarr
  glonarr2[0,1:nscan] = glonarr[0,*]-glstep           ; left edge
  glonarr2[38,1:nscan] = glonarr[36,*]+glstep         ; right edge
  glonarr2[1:37,0] = glonarr[*,0]              ; bottom edge
  glonarr2[1:37,nscan+1] = glonarr[*,nscan-1]  ; top edge
  glonarr2[0,0] =  glonarr[0,0]-glstep                ; left-bottom corner
  glonarr2[0,nscan+1] = glonarr[0,nscan-1]-glstep     ; left-top corner
  glonarr2[38,0] = glonarr[36,0]+glstep               ; right-bottom corner
  glonarr2[38,nscan+1] = glonarr[36,nscan-1]+glstep   ; top-right corner

  glatarr2 = fltarr(39,nscan+2)
  glatarr2[1:37,1:nscan] = glatarr
  glatarr2[0,1:nscan] = glatarr[0,*]           ; left edge
  glatarr2[38,1:nscan] = glatarr[36,*]         ; right edge
  glatarr2[1:37,0] = glatarr[*,0]-gbstep              ; bottom edge
  glatarr2[1:37,nscan+1] = glatarr[*,nscan-1]+gbstep  ; top edge
  glatarr2[0,0] =  glatarr[0,0]-gbstep                ; left-bottom corner
  glatarr2[0,nscan+1] = glatarr[0,nscan-1]+gbstep     ; left-top corner
  glatarr2[38,0] = glatarr[36,0]-gbstep               ; right-bottom corner
  glatarr2[38,nscan+1] = glatarr[36,nscan-1]+gbstep   ; top-right corner

  ; Convert to celestial coordinates
  ;GLACTC,raarr,decarr,2000.0,glonarr,glatarr,2,/deg
  ;GLACTC,raarr2,decarr2,2000.0,glonarr2,glatarr2,2,/deg
 ; GAL2MAG,glonarr,glatarr,mlonarr,mlatarr,wrap=0
 ; GAL2MAG,glonarr2,glatarr2,mlonarr2,mlatarr2,wrap=0

  ; Convert from GLON/GLAT to new X/Y
  ADXY,head,glonarr,glatarr,xnew,ynew
  ADXY,head,glonarr2,glatarr2,xnew2,ynew2
  ;ADXY,head,raarr,decarr,xnew,ynew
  ;ADXY,head,raarr2,decarr2,xnew2,ynew2
 ;ADXY,head,mlonarr,mlatarr,xnew,ynew
 ;ADXY,head,mlonarr2,mlatarr2,xnew2,ynew2


  ; Triangulate
  triangulate,xnew2,ynew2,tr,b

  ;xmin = ceil(min(xnew))
  xmax = floor(max(xnew))
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

  ;xout = findgen(37)
  ;yout = findgen(nscan)
  ;;grid = TRIGRID(xnew, ynew, map2, tr, XOUT = xout, YOUT = yout, extrapolate=b)
  ;cube2 = fltarr(37,nscan,nchan)
  ;print,'REGRIDDING THE CUBE'

  ; Now loop through all of the channels and regrid
  for j=0,nchan-1 do begin
  ;for j=0,0 do begin

    if (j+1) mod 100 eq 0 then print,strtrim(j+1,2),'/',strtrim(nchan,2)

    map = reform(cube[*,*,j])

    ; Extend by 1 pixel to deal with points outside the boundary
    map2 = fltarr(39,nscan+2)
    map2[1:37,1:nscan] = map             ; center map
    map2[0,1:nscan] = map[0,*]           ; left edge
    map2[38,1:nscan] = map[36,*]         ; right edge
    map2[1:37,0] = map[*,0]              ; bottom edge
    map2[1:37,nscan+1] = map[*,nscan-1]  ; top edge
    map2[0,0] =  map[0,0]                ; left-bottom corner
    map2[0,nscan+1] = map[0,nscan-1]     ; left-top corner
    map2[38,0] = map[36,0]               ; right-bottom corner
    map2[38,nscan+1] = map[36,nscan-1]   ; top-right corner

    ;grid = TRIGRID(xnew2, ynew2, map2, tr, XOUT = xout, YOUT = yout, extrapolate=b, missing=!values.f_nan)
    grid = TRIGRID(xnew2, ynew2, map2, tr, XOUT = xout, YOUT = yout, missing=!values.f_nan)
    ;cube2[*,*,j] = grid

    ;if i eq 48 then stop

    ;stop

    temprms = rmsmap[xmin:xmax,ymin:ymax]
    gdgrid = where(temprms ge rms and finite(grid) eq 1,ngdgrid)
    if ngdgrid eq 0 then stop,'NO NEW POINTS TO ADD'
    temprms[gdgrid] = rms 
    rmsmap[xmin:xmax,ymin:ymax] = temprms

    tempmap = reform(bigcube[xmin:xmax,ymin:ymax,j])
    tempmap[gdgrid] = grid[gdgrid]
    bigcube[xmin:xmax,ymin:ymax,j] = tempmap

    ;stop

  end

  ;temp = tempmap
  ;bd = where(finite(temp) eq 0,nbd)
  ;if nbd gt 0 then temp[bd] = -100
  ;displayc,temp,min=min(map),max=max(map),tit=strtrim(i,2)+' '+isource
  ;wait,1

  ;stop

end

;SKIP1:

; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(bigcube)
tot3 = total(bigcube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(bigcube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'


;FITS_WRITE,dir+'grid_gbt_gal.fits',bigcube,head
;FITS_WRITE,dir+'grid_gbt_gal_rmsmap.fits',rmsmap,rmshead

stop




;----------------------
; Smooth the datacube
;----------------------
SMOOTHCUBE:


; Smooth each integration with a Gaussian in velocity
;gdints = where(finite(reform(bigcube[*,*,0]) ) eq 1,ngdints)
gdints = where(reform(bigcube[*,*,0]) ne 0.0,ngdints)
print,'SMOOTHING GRID IN VELOCITY - ',strtrim(ngdints,2),' positions'
onemap = reform(bigcube[*,*,0])
chansm = 10L
for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),'/',strtrim(ngdints,2)

  ind2 = array_indices(onemap,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(bigcube[xind,yind,*])

  ; Gaussian Smooth
  spec2 = GSMOOTH(spec,chansm,widfwhm=3)

  ; Put back in
  bigcube[xind,yind,*] = spec2

  ;stop

end
;head2 = head
velsm = abs(sxpar(head,'CDELT3'))*chansm
strvelsm = strtrim(string(velsm,format='(F6.1)'),2)
SXADDHIST,'Velocity Gaussian smoothing - FWHM='+strtrim(chansm,2)+' channels = '+strvelsm+' km/s',head

; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(bigcube)
tot3 = total(bigcube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(bigcube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'


;FITS_WRITE,dir+'grid_gbt_gal_vsm.fits',bigcube,head
;FITS_WRITE,dir+'grid_gbt_gal_vsm_rmsmap.fits',rmsmap,rmshead

stop

;-------------------------------------------
; Smooth with a spatial gaussian, FWHM=3
;-------------------------------------------
SMOOTHCUBE_POS:
nchan = n_elements(bigcube[0,0,*])
print,'SMOOTHING GRID IN POSITION - ',strtrim(nchan,2),' channels'

; Make the smoothing kernel
smpos = 3L ;2L ;3L
npix = 2 * fix( (3*smpos)/2 ) + 1    ;make # pixels odd.
psf = PSF_GAUSSIAN(np=npix,fwhm=smpos,/norm)

bd = where(reform(bigcube[*,*,0]) eq 0.0,nbd)

for i=0,nchan-1 do begin
  if (i+1) mod 100 eq 0 then print,strtrim(i+1,2),'/',strtrim(nchan,2)

  map = reform(bigcube[*,*,i])

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
  bigcube[*,*,i] = map2

end



; Add history statement to header
;head3 = head2
cdelt1 = abs(SXPAR(head,'CDELT1'))
smposmin = smpos*cdelt1*60.0  ; in arcmin
strsmposmin = strtrim(string(smposmin,format='(F6.1)'),2)
strsmpos = strtrim(string(smpos,format='(F6.1)'),2)
SXADDHIST,'Spatial Gaussian smoothing - FWHM='+strsmpos+' pixels = '+strsmposmin+' arcmin',head



; Make RMS map
;----------------------
print,'Making RMS map'
sz = size(bigcube)
tot3 = total(bigcube,3)
gdints = where(tot3 ne 0.0,ngdints)
rmsmap = fltarr(sz[1],sz[2])+1d30

for i=0L,ngdints-1 do begin
  if (i+1) mod 5000 eq 0 then print,strtrim(i+1,2),' ',strtrim(ngdints,2)
  ind2 = array_indices(tot3,gdints[i])
  xind = ind2[0]
  yind = ind2[1]
  spec = reform(bigcube[xind,yind,*])
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

;FITS_WRITE,dir+'grid_gbt_gal_pvsm.fits',bigcube,head
;FITS_WRITE,dir+'grid_gbt_gal_pvsm_rmsmap.fits',rmsmap,rmshead

;FITS_WRITE,dir+'grid_gbt_gal_psm.fits',bigcube,head
;FITS_WRITE,dir+'grid_gbt_gal_psm_rmsmap.fits',rmsmap,rmshead



stop


end
