pro grid_mstip_mag

; Put all of the MS-tip survey observations
; into a 3D datacube (grid)
; No interpolation is needed since they are all
; on the same grid

forward_function func_cos, func_cospoly
resolve_routine,'remove_baseline_indiv',/compile_full_file

dir = '/net/halo/dln5q/doradus/research/observing/gbt/GBT10B-035/data/'
;tag = 'red1'
tag = 'red2'

;files = file_search(dir+'MSTIP*'+tag+'.dat',count=nfiles)
;files = file_search(dir+'*'+tag+'.dat',count=nfiles)

; Just pass 1 for now
files = file_search(dir+'MSTIP*'+tag+'.dat',count=nfiles)
base = file_basename(files)
arr = strsplitter(base,'_',/extract)
sesname = reform(arr[1,*])
sesnum = long(strmid(sesname,3))
;gd = where(sesnum le 9,ngd)
;files = files[gd]
;nfiles = ngd

vel = (findgen(800)+1-400)*0.322*5

cen_ra = 357.5d0
cen_dec = 56.5d0
step = 4.0/60.0

dec0 = cen_dec-7.5  ; DEC value of first row

glactc,cen_ra,cen_dec,2000.0,cen_glon,cen_glat,1,/deg
gal2mag,cen_glon,cen_glat,cen_mlon,cen_mlat

;-------------------------------------------------------
; SETUP THE HEADER
;-------------------------------------------------------
; copied some of this code from /net/halo/dln5q/gbt/mag_head.pro

; Set up the WCS information for the LARGE image/cube
undefine,head
nchan = 800
;nx = 160 ;1700  ; 1750
;ny = 250 ;1600
nx = 450 ;1700  ; 1750
ny = 200 ;1600
bigcube = fltarr(nx,ny,nchan)
;bigcube[*,*,*] = !values.f_nan   ; default all bad
MKHDR,head,bigcube
SXADDPAR,head,'NAXIS3',nchan

;xref = 90L ;850L ;790L  ; 875
;yref = 127L ;800L ;750L  ; 800
;ra_ref = cen_ra
;dec_ref = cen_dec
xref = 280L ;150L
yref = 100L
mlon_ref = -145.0
mlat_ref = 16.50

step = 4.0d0/60.0d0
vstep = 1.610
vmin = -642.390

; Braun used SIN with CDELT
; Stanimirovic used NO projection with CDELT
; Bruens used ZEA with CDELT
SXADDPAR,head,'CDELT1',step
SXADDPAR,head,'CRPIX1',xref+1L
SXADDPAR,head,'CRVAL1',mlon_ref
SXADDPAR,head,'CTYPE1','MLON'
SXADDPAR,head,'CDELT2',step
SXADDPAR,head,'CRPIX2',yref+1L
SXADDPAR,head,'CRVAL2',mlat_ref
SXADDPAR,head,'CTYPE2','MLAT'
SXADDPAR,head,'CDELT3',vstep
SXADDPAR,head,'CRPIX3',1L
SXADDPAR,head,'CRVAL3',vmin
SXADDPAR,head,'CTYPE3','VELO-LSR'

sz = size(bigcube)
nx = sz[1]
ny = sz[2]


; Initialize the cube
;cube = fltarr(74,226,800)
;raarr = fltarr(74,226)
;decarr = fltarr(74,226)
maxy = -1

For i=0,nfiles-1 do begin

  print,'Adding ',file_basename(files[i])
  restore,files[i]
  

  fbase = file_basename(files[i],'_'+tag+'.dat')
  dum = strsplit(fbase,'_',/extract)
  session = dum[1]
  session_num = long(strmid(session,3))

  add_tag,final,'session',0,final
  final.session = session_num

  if file_basename(files[i],'_red2.dat') eq 'MSTIP-survey_ses5_s10-38' then begin
    bd = where(final.scan eq 38 ,nbd)
    if nbd gt 0 then remove,bd,final
  endif

  ; Remove a polynomial baseline from this session
  nfinal = n_elements(final)
  tot = TOTAL(final.comb_corr,2)/nfinal
  ;subcube = cube[*,min(yindarr):max(yindarr),*]
  ;sz = size(subcube)
  ;tot1 = total(subcube,1)/sz[1]
  ;sig1 = mad(tot1)
  ;bd1 = where(tot1 gt 4*sig1,nbd1)
  ;if nbd1 gt 0 then tot1[bd1] = !values.f_nan
  ;tot = total(tot1,1)/sz[2]
  ;xx1 = findgen(sz[3])
  xx1 = findgen(nchan)
  yy1 = tot
  sig = mad(tot)
  bd = where(xx1 gt 275 or (xx1 ge 23 and xx1 le 37) or finite(tot) eq 0,nbd)
  ;bd = where(xx gt 275 or (xx ge 37 and xx le 53),nbd)
  remove,bd,xx1,yy1
  ; also remove MS region
  ;bd2 = where(xx1 ge 130 and xx1 le 180,nbd2)
  ;remove,bd2,xx1,yy1

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

  ; Polynomial fit
  xx = findgen(sz[3])
  plcoef = robust_poly_fit(xx1,yy1,5)
  plbase = poly(xx,plcoef)

  ; Plotting
  plot,xx1,yy1,ps=1
  swave = func_cospoly(xx,fpar)
  oplot,xx,swave,co=250
  oplot,xx,poly(xx,plcoef),co=150

  ; Final baseline
  base = plbase
  if session eq 'ses1' then base=swave


  ; Now remove it
  ;base2 = replicate(1.0,sz[1]*sz[2])#base
  ;base3 = fltarr(sz[1],sz[2],sz[3])
  ;base3[*] = base2
  ;subcube -= base3
  base2 = base#replicate(1.0,nfinal)
  final.comb_corr -= base2  ; correct comb_corr


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
  badrows = [10, 11, 20, 33, 42, 43, 55, 56]
  noise = 0.055
  for j=0,n_elements(badrows)-1 do begin
    bd = where(final.procseqn eq badrows[j],nbd)
    if nbd gt 0 then final[bd].comb_corr = randomn(seed,nchan,nbd)*noise
    ;if nbd gt 0 then final[bd].comb_corr = 0.0
  end

  ; Add to final structure
  push,survey,final

  skip:

  wait,0.5
  ;stop

End



; Now interpolate onto the final grid
;------------------------------------
;ind1 = where(session lt 11,ind1)
;ind2 = where(session ge 11,ind2)
ind1 = where(survey.session lt 9 or (survey.session eq 9 and survey.scan le 21),nind1)
ind2 = where( (survey.session gt 10 and survey.session lt 15) or $
              (survey.session eq 10 and survey.scan ge 79) or $
              (survey.session eq 15 and survey.scan lt 84),ningd2)
survey1 = survey[ind1]
survey2 = survey[ind2]

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

; Replace bad scans for pass 1
;  procseqn=10
gd = where(survey.session eq 9 and survey.scan eq 22 and survey.procseqn eq 10,ngd)    ; redo
bd = where(survey1.procseqn eq 10,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=11
gd = where(survey.session eq 9 and survey.scan eq 23 and survey.procseqn eq 11,ngd)    ; redo
bd = where(survey1.procseqn eq 11,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=20
gd = where(survey.session eq 9 and survey.scan eq 24 and survey.procseqn eq 20,ngd)    ; redo
bd = where(survey1.procseqn eq 20,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=33
gd = where(survey.session eq 9 and survey.scan eq 25 and survey.procseqn eq 33,ngd)    ; redo
bd = where(survey1.procseqn eq 33,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=42
gd = where(survey.session eq 9 and survey.scan eq 26 and survey.procseqn eq 42,ngd)    ; redo
bd = where(survey1.procseqn eq 42,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=43
gd = where(survey.session eq 10 and survey.scan eq 8 and survey.procseqn eq 43,ngd)    ; redo
bd = where(survey1.procseqn eq 43,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=55
gd = where(survey.session eq 10 and survey.scan eq 9 and survey.procseqn eq 55,ngd)    ; redo
bd = where(survey1.procseqn eq 55,nbd)                                                 ; bad
survey1[bd] = survey[gd]
;  procseqn=56
gd = where(survey.session eq 10 and survey.scan eq 10 and survey.procseqn eq 56,ngd)    ; redo
bd = where(survey1.procseqn eq 56,nbd)                                                  ; bad
survey1[bd] = survey[gd]

; Remove extra row 2 for pass 2
bd = where(survey2.session eq 11 and survey2.scan eq 8,nbd)
remove,bd,survey2

; Replace bad scans for pass 2
;  procseqn=124
gd = where(survey.session eq 15 and survey.scan eq 85 and survey.procseqn eq 124,ngd)    ; redo
bd = where(survey2.procseqn eq 124,nbd)                                                  ; bad
survey2[bd] = survey[gd]
;  procseqn=125
gd = where(survey.session eq 15 and survey.scan eq 86 and survey.procseqn eq 125,ngd)    ; redo
bd = where(survey2.procseqn eq 125,nbd)                                                  ; bad
survey2[bd] = survey[gd]
;  procseqn=138
gd = where(survey.session eq 15 and survey.scan eq 87 and survey.procseqn eq 138,ngd)    ; redo
bd = where(survey2.procseqn eq 138,nbd)                                                  ; bad
survey2[bd] = survey[gd]
;  procseqn=139
gd = where(survey.session eq 15 and survey.scan eq 88 and survey.procseqn eq 139,ngd)    ; redo
bd = where(survey2.procseqn eq 139,nbd)                                                  ; bad
survey2[bd] = survey[gd]

;stop

; I think the last 5 scans in session 9 are redos for bad scans
; the first three scans in session 10 are also redos for bad scans
; The last two scans of session 10 are rows 1+2 of the second pass
; session 11 starts with row 2
; Regular scans for pass 1 end at with: session 9 scan 21
; Regular scans for pass 2 start with: session 10 scan 79

;============
; PASS 1
;============

; Convert RA/DEC to MLON/MLAT
glactc,survey1.ra,survey1.dec,2000.,glon,glat,1,/deg
gal2mag,glon,glat,mlon,mlat

; Convert from MLON/MLAT to new X/Y
ADXY,head,mlon,mlat,xnew,ynew

; Triangulate
triangulate,xnew,ynew,tr,b

xmin = ceil(min(xnew))
xmax = floor(max(xnew))
nxout = xmax-xmin+1
xout = lindgen(nxout)+xmin
ymin = ceil(min(ynew))
ymax = floor(max(ynew))
nyout = ymax-ymin+1
yout = lindgen(nyout)+ymin

numobs = lonarr(nx,ny)

; Make a mask to make sure that TRIGRID doesn't extrapolate and
;  create garbage around the edges
cmask = lonarr(nx,ny)
wig = 0.1  ; "convolving" wiggle
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

For j=0,nchan-1 do begin

  if (j+1) mod 200 eq 0 then print,strtrim(j+1,2),'/',strtrim(nchan,2)

  grid = TRIGRID(xnew, ynew, survey1.comb_corr[j], tr, XOUT = xout, YOUT = yout, missing=!values.f_nan)
 
  if j eq 0 then begin
    obsmask = long(finite(grid) eq 1 and cmask[xmin:xmax,ymin:ymax] eq 1)
    numobs[xmin:xmax,ymin:ymax] += obsmask
  endif
  bd = where(finite(grid) eq 0 or cmask[xmin:xmax,ymin:ymax] eq 0,nbd)
  if nbd gt 0 then grid[bd]=0.0

  bigcube[xmin:xmax,ymin:ymax,j] += grid

End

tot1 = total(bigcube[*,*,150:210],3)
displayc,tot1,min=-0.8,max=0.8,/xflip

stop

;============
; PASS 2
;============

; Convert RA/DEC to MLON/MLAT
glactc,survey2.ra,survey2.dec,2000.,glon,glat,1,/deg
gal2mag,glon,glat,mlon,mlat

; Convert from MLON/MLAT to new X/Y
ADXY,head,mlon,mlat,xnew,ynew

; Triangulate
triangulate,xnew,ynew,tr,b

;xmin = ceil(min(xnew))
;xmax = floor(max(xnew))
;nxout = xmax-xmin+1
;xout = lindgen(nxout)+xmin
;ymin = ceil(min(ynew))
;ymax = floor(max(ynew))
;nyout = ymax-ymin+1
;yout = lindgen(nyout)+ymin

; Make a mask to make sure that TRIGRID doesn't extrapolate and
;  create garbage around the edges
cmask = lonarr(nx,ny)
wig = 0.1  ; "convolving" wiggle
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

;bigcube = bigcube*0

For j=0,nchan-1 do begin

  if (j+1) mod 200 eq 0 then print,strtrim(j+1,2),'/',strtrim(nchan,2)

  grid = TRIGRID(xnew, ynew, survey2.comb_corr[j], tr, XOUT = xout, YOUT = yout, missing=!values.f_nan)

  if j eq 0 then begin
    obsmask = long(finite(grid) eq 1 and cmask[xmin:xmax,ymin:ymax] eq 1)
    numobs[xmin:xmax,ymin:ymax] += obsmask
  endif

  bd = where(finite(grid) eq 0 or cmask[xmin:xmax,ymin:ymax] eq 0,nbd)
  if nbd gt 0 then grid[bd]=0.0

  bigcube[xmin:xmax,ymin:ymax,j] += grid

  ;if j eq 170 then stop

End

tot2 = total(bigcube[*,*,150:210],3)
displayc,tot2,min=-0.8,max=0.8,/xflip



stop


; Now add the CHVC
files2a = file_search(dir+'CHVC*'+tag+'.dat',count=nfiles2a)
files2b = file_search(dir+'MSWedge*'+tag+'.dat',count=nfiles2b)
files2 = [files2a,files2b]
nfiles2 = n_elements(files2)


For i=0,nfiles2-1 do begin

  print,'Adding ',file_basename(files2[i])
  restore,files2[i]

  fbase = file_basename(files2[i],'_'+tag+'.dat')
  dum = strsplit(fbase,'_',/extract)
  session = dum[1]
  session_num = long(strmid(session,3))

  add_tag,final,'session',0,final
  final.session = session_num

  ;; Smoothed image
  ;im = final.comb_corr
  ;smim = smooth(im,[10,10],/edge)

  ; Remove a polynomial baseline from this session
  nfinal = n_elements(final)
  tot = TOTAL(final.comb_corr,2)/nfinal
  med = MEDIAN(final.comb_corr,dim=2)
  ;subcube = cube[*,min(yindarr):max(yindarr),*]
  ;sz = size(subcube)
  ;tot1 = total(subcube,1)/sz[1]
  ;sig1 = mad(tot1)
  ;bd1 = where(tot1 gt 4*sig1,nbd1)
  ;if nbd1 gt 0 then tot1[bd1] = !values.f_nan
  ;tot = total(tot1,1)/sz[2]
  ;xx1 = findgen(sz[3])
  xx1 = findgen(nchan)
  yy1 = tot  ;med
  sig = mad(yy1)
  bd = where(xx1 gt 275 or (xx1 ge 23 and xx1 le 37) or finite(tot) eq 0,nbd)
  ;bd = where(xx gt 275 or (xx ge 37 and xx le 53),nbd)
  remove,bd,xx1,yy1
  ; also remove MS region
  ;bd2 = where(xx1 ge 130 and xx1 le 180,nbd2)
  ;remove,bd2,xx1,yy1

  if fbase eq 'CHVC48_ses4_s53-88' then begin
    bd = where(xx1 ge 172 and xx1 le 207,nbd)
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

  ; Polynomial fit
  xx = findgen(sz[3])
  plcoef = robust_poly_fit(xx1,yy1,5)
  plbase = poly(xx,plcoef)
  ;sig2 = mad(yy1-poly(xx1,plcoef))
  ;gd = where(abs(yy1-poly(xx1,plcoef)) lt 3*sig2,ngd)
  ;plcoef2 = robust_poly_fit(xx1[gd],yy1[gd],5)
  ; should mask points next those rejected to remove wings


  ; Plotting
  plot,xx1,yy1,ps=1
  swave = func_cospoly(xx,fpar)
  oplot,xx,swave,co=250
  oplot,xx,poly(xx,plcoef),co=150

  ; Final baseline
  base = swave
  if session eq 'ses2' then base=plbase
  if session eq 'ses3' then base=plbase
  if session eq 'ses4' then base=plbase
  if session eq 'ses5' then base=plbase
  if session eq 'ses6' then base=plbase
  if session eq 'ses7' then base=plbase

  ; Now remove it
  ;base2 = replicate(1.0,sz[1]*sz[2])#base
  ;base3 = fltarr(sz[1],sz[2],sz[3])
  ;base3[*] = base2
  ;subcube -= base3
  base2 = base#replicate(1.0,nfinal)
  final.comb_corr -= base2  ; correct comb_corr

  ; Kludge
  ;  some bad integrations for 
  if fbase eq 'MSWedge2_ses10_s44-77' then begin
    final[388:394].comb_corr = 0
    final[415:425].comb_corr = 0
    final[445:454].comb_corr = 0
  endif

  ; Now interpolate onto the final grid
  ;------------------------------------

  ; Convert RA/DEC to MLON/MLAT
  glactc,final.ra,final.dec,2000.,glon,glat,1,/deg
  gal2mag,glon,glat,mlon,mlat

  ; Convert from MLON/MLAT to new X/Y
  ADXY,head,mlon,mlat,xnew,ynew

  ; Triangulate
  triangulate,xnew,ynew,tr,b

  xmin = ceil(min(xnew))
  xmax = floor(max(xnew))
  nxout = xmax-xmin+1
  xout = lindgen(nxout)+xmin
  ymin = ceil(min(ynew))
  ymax = floor(max(ynew))
  nyout = ymax-ymin+1
  yout = lindgen(nyout)+ymin


  For j=0,nchan-1 do begin

    if (j+1) mod 200 eq 0 then print,strtrim(j+1,2),'/',strtrim(nchan,2)

    grid = TRIGRID(xnew, ynew, final.comb_corr[j], tr, XOUT = xout, YOUT = yout, missing=!values.f_nan)

    gd = where(finite(grid) eq 1,ngd)
    bd = where(finite(grid) eq 0,nbd)

    if j eq 0 then begin
      mask = long(finite(grid) eq 1)
      numobs[xmin:xmax,ymin:ymax] += mask
    endif
    if nbd gt 0 then grid[bd]=0.0



    ;temp = reform(bigcube[xmin:xmax,ymin:ymax,j])
    ;gd = where(finite(grid) eq 1,ngd)
    ;temp[gd] = grid[gd]

    bigcube[xmin:xmax,ymin:ymax,j] += grid
    ;bigcube[xmin:xmax,ymin:ymax,j] = temp
    ;;bigcube[xmin:xmax,ymin:ymax,j] = grid  ; replace anything that's there
    ;;bigcube[xmin:xmax,ymin:ymax,j] += grid

    ;if j eq 170 then stop
    ;stop

  End

  ;stop

End

; Now divide by numobs
for j=0,nchan-1 do bigcube[*,*,j] /= (numobs > 1)


tot2 = total(bigcube,2)
tot3 = total(bigcube[*,*,150:210],3)
tot3b = total(bigcube[*,*,50:210],3)
displayc,tot3,min=-0.8,max=0.8,/xflip

stop

; Cut out the main survey region
xlo = 172
xhi = 449
ylo = 25
yhi = 147
zlo = 58
zhi = 250

; This is just the main survey area and the two attached pieces
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

FITS_WRITE,dir+'grid_mstip_mag.fits',cube,head2
FITS_WRITE,dir+'grid_mstip_mag_rmsmap.fits',rmsmap,rmshead


; Copied from /net/halo/dln5q/gbt/grid_gbt_gal.pro
goto,smoothcube_pos

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
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'


;FITS_WRITE,dir+'grid_mstip_mag_vsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag_vsm_rmsmap.fits',rmsmap,rmshead

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
  rmsmap[xind,yind] = MAD(spec)
end
rmshead = head2
sxdelpar,rmshead,'CDELT3'
sxdelpar,rmshead,'CRPIX3'
sxdelpar,rmshead,'CRVAL3'
sxdelpar,rmshead,'CTYPE3'

;FITS_WRITE,dir+'grid_mstip_mag_pvsm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag_pvsm_rmsmap.fits',rmsmap,rmshead

;FITS_WRITE,dir+'grid_mstip_mag_psm.fits',cube,head2
;FITS_WRITE,dir+'grid_mstip_mag_psm_rmsmap.fits',rmsmap,rmshead


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
;smtot3c = total(smcube3[*,*,113:*],3)
displayc,smtot3


stop

col1 = tot3*1.61*1.83e18 / 1e18

;col3b = tot3b*1.61*1.83e18 / 1e18
smcol1 = smtot3*1.61*1.83e18 / 1e18
smcol2 = smtot3b*1.61*1.83e18 / 1e18
smcol3 = smtot3c*1.61*1.83e18 / 1e18
; the noise level in the column density image is 1.3E18 cm^-2

; PLOTS,  ALL emission
ps_open,'gbt_mstip_mag_all1',/color,thick=4,/encap
device,/inches,xsize=15,ysize=12
displayc,transpose(smtot1),vl,mb,min=-1.5,max=10,xtit='VLSR (km/s)',ytit='MLAT (deg)',$
         tit='MLAT-VLSR (all MLON)',position=[0.02,0.5,0.4,1.01],colcharsize=0.8
displayc,smtot2,ml,vl,min=-1.5,max=10,/xflip,xtit='MLON (deg)',ytit='VLSR (km/s)',$
         tit='MLON-VLSR (all MLAT)',position=[0.4,0.0,1.0,0.51],/noerase,colcharsize=0.8
displayc,smcol1,ml,mb,min=-3,max=30,xtit='MLON (deg)',ytit='MLAT (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -570<VLSR<-240 km/s',position=[0.4,0.5,1.0,1.01],/noerase
xyouts,-139,17,'CHVC 402',align=0.5,charsize=1.5,co=255,/data
xyouts,-143,13,'CHVC 391',align=0.0,charsize=1.5,co=255,/data
ps_close
ps2gif,'gbt_mstip_mag_all1.eps',/eps


; MS plots
ps_open,'gbt_mstip_mag_ms1',/color,thick=4,/encap
device,/inches,xsize=15,ysize=12
displayc,transpose(smooth(smtot1,[2,2],/edge)),vl,mb,min=-1.5,max=1.5,xtit='VLSR (km/s)',ytit='MLAT (deg)',$
         tit='MLAT-VLSR (all MLON)',position=[0.02,0.5,0.4,1.01],colcharsize=0.8
oplot,[0,0]+vl[110],minmax(mb),linestyle=2,co=0,thick=4
oplot,[0,0]+vl[176],minmax(mb),linestyle=2,co=0,thick=4
;oplot,[0,0]+vl[110],minmax(mb),linestyle=2,co=255
;oplot,[0,0]+vl[176],minmax(mb),linestyle=2,co=255
;arrow,50.62,-463.15,51.19,-424.31,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
displayc,smooth(smtot2,[2,2],/edge),ml,vl,min=-1.0,max=1.0,/xflip,xtit='MLON (deg)',ytit='VLSR (km/s)',$
         tit='MLON-VLSR (all MLAT)',position=[0.4,0.0,1.0,0.51],/noerase,colcharsize=0.8
oplot,minmax(ml),[0,0]+vl[110],linestyle=2,co=0
oplot,minmax(ml),[0,0]+vl[176],linestyle=2,co=0
;arrow,359.22,-478.388,358.47,-441.08,hsize=(!D.X_SIZE / 64.),/solid,/data,thick=5,color=255
displayc,smcol2,ml,mb,min=-1.5,max=1.5,xtit='MLON (deg)',ytit='MLAT (deg)',/xflip,colcharsize=0.8,$
         tit='Column Density (10!u18!n cm!u-2!n) for -393<VLSR<-287 km/s',position=[0.4,0.5,1.0,1.01],/noerase
xyouts,-135,13.9,'BT04',align=0.5,charsize=1.5,charthick=4,co=0,/data
xyouts,-144.8,12.4,'MS detections',align=0.5,charsize=1.5,charthick=4,co=0,/data
arrow,-144.8,12.8,-142.0,13.5,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
arrow,-144.8,12.8,-145.0,14.2,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
xyouts,-143.8,18.9,'MS???',align=0.5,charsize=1.5,charthick=4,co=0,/data
arrow,-143.8,18.7,-143.5,16.5,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
arrow,-143.8,18.7,-146.5,18.0,hsize=(!D.X_SIZE / 64.)*0.7,/solid,/data,thick=5,color=0
;xyouts,-139,17,'CHVC 402',align=0.5,charsize=1.5,co=255,/data
;xyouts,-143,13,'CHVC 391',align=0.0,charsize=1.5,co=255,/data
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
