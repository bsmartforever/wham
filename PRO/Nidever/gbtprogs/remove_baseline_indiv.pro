function func_cos,x,par
;return,par[0]*cos((par[1]*x+par[2])/!radeg)
return,par[0]*cos((x-par[2])*2.0*!dpi/par[1])
end

;function func_2cos,x,par
;return,par[0]*cos((par[1]*x+par[2])/!radeg) + par[3]*cos((2.0*par[1]*x+par[4])/!radeg)
;end

function func_cospoly,x,par
;return,par[0]*cos((par[1]*x+par[2])/!radeg) + poly(x,par[3:*])
return,par[0]*cos((x-par[2])*2.0*!dpi/par[1]) + poly(x,par[3:*])
end


pro remove_baseline_indiv,file,save=save,tag=tag,stp=stp

; Try to remove the baseline for each spectrum separately

if n_elements(file) eq 0 then begin
  print,'remove_baseline_indiv,file,save=save,tag=tag,stp=stp'
  return
endif

forward_function func_cos, func_2cos, func_cospoly

test = file_test(file)
if test eq 0 then begin
  print,'File ',file,' NOT FOUND'
  return
endif

cspeed = 2.99792458d5  ; speed of light in km/s

;if n_elements(tag) eq 0 then tag='_red1'
if n_elements(tag) eq 0 then tag='_red2'

;restore,'/local/dln5q/research/observing/gbt/data/vel.dat'
;v2 = v[4500:9499]

setdisp

print,'###################################################'
print,'USING REMOVE_BASELINE_INDIV2.PRO FOR THIS PROJECT!!!'
print,'###################################################'
stop

; --- Restore the file ---
str = mrdfits(file,1)
ind0 = where(str.plnum eq 0,nind0)
ind1 = where(str.plnum eq 1,nind1)

; Make the velocity array
n = n_elements(str[0].data)
f = (dindgen(n)+1-str[0].crpix1)*str[0].cdelt1 + str[0].crval1
v = (str[0].restfreq-f)/str[0].restfreq*cspeed
; THIS IS ONLY APPROXIMATELY CORRECT!!!!

; FIX MISSING INTEGRATIONS
bd = where(finite(str[ind1].data[16000]) eq 0,nbd)
print,'Fixing ',strtrim(nbd,2),' missing plnum=1 integrations'
for i=0,nbd-1 do begin
  ; Replace it with the integration PRECEDING IT
  repind = bd[i]-1
  if bd[i] eq 0 then repind=bd[i]+1
  str[ind1[bd[i]]].data = str[ind1[repind]].data
end


; --- KLUDGE ---
; Flag bad data

; HVCAGrid1_ses1_s14-17.fits
if file_basename(file,'.fits') eq 'HVCAGrid1_ses1_s14-17' then begin
  str0 = str[ind0]
  str0.data[6934:6984]=!values.f_nan
  str0.data[11325:11750]=!values.f_nan
  str1 = str[ind1]
  str1.data[6934:6984]=!values.f_nan
  str1.data[11325:11750]=!values.f_nan
  str[ind0].data = str0.data  ; copy back in
  str[ind1].data = str1.data
  undefine,str0,str1
endif

; Bigger RFI problems:
; MS083.7-34.0_ses25_s70-106
if file_basename(file,'.fits') eq 'MS083.7-34.0_ses25_s70-106' then begin
  ; channels 8500-9800, ints 2050-2300
  str0 = str[ind0]
  str0[1040:1094].data[9355:9840]=!values.f_nan
  str0[1054:1086].data[8794:9355]=!values.f_nan
  str1 = str[ind1]
  str1[1033:1083].data[9355:9840]=!values.f_nan
  str1[1054:1082].data[8765:9355]=!values.f_nan
  str1[1084:1115].data[9565:9840]=!values.f_nan
  str[ind0].data = str0.data  ; copy back in
  str[ind1].data = str1.data
  str.data[9354:9373]=!values.f_nan
  undefine,str0,str1
endif


; ---- RFI ----
; Maybe do the two polarizations separately??
tot = total(str.data,2,/nan)
nchanel = n_elements(str[0].data)
xx = findgen(nchanel)
;gd = where( (xx gt 400 and xx lt 3600) or (xx gt 4000 and xx lt 9700),ngd)
gd = where( (xx gt 500 and xx lt 3600) or (xx gt 4400 and xx lt 9700) and finite(tot) eq 1,ngd)
;coef = robust_poly_fit(xx[gd],tot[gd],5)
;med1 = poly(xx,coef)
med = median(tot,20)
slp = slope(tot-med)
slpabs = slope(abs(tot-med))
rt_slpabs = slope(abs(tot-med))
lft_slpabs = shift(rt_slpabs,1)
std = mad( (tot-med)[500:9700] )
BINDATA,xx,tot-med,xbin,stdbin,bin=200,/mad
noise = CSPLINE(xbin,stdbin,xx)
noise = noise > std
;noise = std > sqrt( (tot-med1)>0)
bd = where( (( lft_slpabs gt 4.0*noise or rt_slpabs lt -4.0*noise) or (abs(tot-med) gt 4.0*noise) ) $
             and ((xx gt 500 and xx lt 3600) or (xx gt 4400 and xx lt 9700) ),nbd)
;            ((xx gt 400 and xx lt 3600) or (xx gt 4000 and xx lt 9700) ),nbd)
; grow by 2 pixel
nbd2 = 0
if nbd gt 0 then begin
  mask = tot*0.0
  mask[bd] = 1.0
  mask = CONVOL(mask,fltarr(5)+1.0,/center)
  ;mask = mask + shift(mask,-1) + shift(mask,1)
  mask = mask/(mask>1)
  bd2 = where(mask eq 1 and abs(tot-med) gt 2.0*noise,nbd2)
  ;bd2 = where(mask eq 1 and abs(tot-med) gt 4.0*noise,nbd2)
endif

; KLUDGE for complex A
if file_basename(file,'.fits') eq 'complexa4_ses26_s10-46' then begin
  bbb = where(bd2 gt 4500 and bd2 lt 6000,nbbb)
  REMOVE,bbb,bd2
  nbd2 = n_elements(bd2)
endif

window,2
;wset,2
;plot,tot-med,yr=[-1300,1300],tit='RFI - '+strtrim(nbd2,2)+' channels'
plot,tot,xr=[0,9800],yr=[-1300,1300],xs=1,ys=1,tit='RFI - '+strtrim(nbd2,2)+' channels'
if nbd gt 0 then oplot,bd2,(tot)(bd2),ps=1,co=250,sym=1.5
; Using median to replace bad ones
if nbd2 gt 0 then begin
  print,'Replacing RFI pixels with median of neighbors'
  for i=0L,nbd2-1 do str.data[bd2[i]]=!values.f_nan  ; set to NaN
  nstr = n_elements(str)
  xx = findgen(n_elements(str[0].data))
  gd = where(finite(str[0].data) eq 1 and xx gt 300 and xx lt 9800,ngd)

  for i=0,nbd2-1 do $
    str.data[bd2[i]] = median(str.data[bd2[i]-10:bd2[i]+10],dim=1)

  ;for i=0,nstr-1 do begin
  ;  ;med2 = median(str[i].data[0:9800],20)
  ;  ;str[i].data[bd2] = med2[bd2]
  ;  ;;str[i].data[bd2]=cspline(xx[gd],str[i].data[gd],bd2)
  ;  for j=0,nbd2-1 do str[i].data[bd2[j]] = median(str[i].data[bd2[j]-10:bd2[j]+10])
  ;end
endif
tot2 = total(str.data,2,/nan)
oplot,tot2,co=150

;stop

;wait,3

source = file_basename(file,'.fits')
if keyword_set(save) then begin
  fil = '../plots/'+source+'_rfi'+tag
  ps_open,fil,/color,thick=4
  ;device,/inches,xsize=25,ysize=8
  plot,tot,xr=[0,9800],yr=[-1300,1300],xs=1,ys=1,xtit='Channels',tit=source+' RFI - '+strtrim(nbd2,2)+' channels'
  if nbd gt 0 then oplot,bd2,(tot)(bd2),ps=1,co=250,sym=1.5
  oplot,tot2,co=150
  ps_close
  ps2gif,fil+'.ps',rot=-90
  FILE_DELETE,fil+'.ps',/allow,/quiet
endif

;stop
;return

; Velocity
; -1319.1204 - +1318.9624
; step 0.16102562


npix = 6300L ;4000L ;900L ;9000 ;5000
npoly =  5 ;4; 3 ;5 ;4 ;5 ;4

dum = {object:'',file:'',ra:0.0d0,dec:0.0d0,scan:0L,procseqn:0,plnum:0,int:0,data:fltarr(npix),model:fltarr(npix),$
       diff:fltarr(npix),coef:fltarr(npoly+1),coldens:0.0,smcoldens:0.0,meanvel:0.0,smmeanvel:0.0,detection:0L,lochan:0L,hichan:0L}
str1 = replicate(dum,nind1)
STRUCT_ASSIGN,str[ind1],str1
;for i=0L,nind1-1 do str1[i].data = REBIN(str[ind1[i]].data[600:9599],900)   ; bin 10x, this takes the mean
;for i=0L,nind1-1 do str1[i].data = str[ind1[i]].data[14300:18299]
;for i=0L,nind1-1 do str1[i].data = REBIN(str[ind1[i]].data[14300:18299],800)
;str1.data = str[ind1].data[600:9599]
;str1.data = str[ind1].data[4500:9499]
str1.data = str[ind1].data[4742:11041]  ; -456.82820 to +571.48142
str1.object = strtrim(str1.object,2)
str1.file = file
str1.ra = str[ind1].crval2
str1.dec = str[ind1].crval3

dum = {object:'',file:'',ra:0.0d0,dec:0.0d0,scan:0L,procseqn:0,plnum:0,int:0,data:fltarr(npix),model:fltarr(npix),fmodel:fltarr(npix),$
       pars:fltarr(3+npoly+1),diff:fltarr(npix),coef:fltarr(npoly+1),swave:fltarr(npix),swavepar:fltarr(3),swave2:fltarr(npix),coldens:0.0,$
       smcoldens:0.0,meanvel:0.0,smmeanvel:0.0,detection:0L,lochan:0L,hichan:0L}
str0 = replicate(dum,nind0)
STRUCT_ASSIGN,str[ind0],str0
;for i=0L,nind1-1 do str0[i].data = REBIN(str[ind0[i]].data[600:9599],900)   ; bin 10x, this takes the mean
;for i=0L,nind1-1 do str0[i].data = str[ind0[i]].data[14300:18299]
;for i=0L,nind1-1 do str0[i].data = REBIN(str[ind0[i]].data[14300:18299],800)
;str0.data = str[ind0].data[600:9599]
;str0.data = str[ind0].data[4500:9499]
str0.data = str[ind0].data[4742:11041]  ; -456.82820 to +571.48142
str0.object = strtrim(str0.object,2)
str0.file = file
str0.ra = str[ind0].crval2
str0.dec = str[ind0].crval3

;dum = {red:fltarr(npix)}
;comb = replicate(dum,nind0)

; Final structure
dum0 = {data:fltarr(npix),pars:fltarr(3+npoly+1),model:fltarr(npix),red:fltarr(npix)}
dum1 = {data:fltarr(npix),pars:fltarr(npoly+1),model:fltarr(npix),red:fltarr(npix)}
dum = {object:'',file:'',ra:0.0d0,dec:0.0d0,scan:0L,procseqn:0,int:0,p0:dum0,p1:dum1,comb:fltarr(npix),comb_corr:fltarr(npix)}
final = replicate(dum,nind0)
STRUCT_ASSIGN,str0,final
final.p0.pars[1] = 1.0   ; just a precaution so we don't get NANs

ui = uniq(str0.scan,sort(str0.scan))
nscan = n_elements(ui)
scans = str0[ui].scan
;nscan = n_elements(str0)/37


;;nstd = 3.0  ;4.0
nbin = 90 ;50 ;500  ;250, 100
nchanelsm = 20 ;100 ;200 ; 100
;nbin = 900 ;50 ;500  ;250, 100
;nchanelsm = 200 ;100 ;200 ; 100
for i=0,nscan-1 do begin

  if (i+1) mod 5 eq 0 then print,strtrim(i+1,2),'/',strtrim(nscan,2)

  iscan = scans[i]
  ;iscan = min(str0.scan)+i
  ind = where(str0.scan eq iscan,nind)
  if nind eq 0 then goto,BOMB


  ;im = str0[ind].data[4500:9499]

  for j=0,nind-1 do begin
    t0 = systime(1)
    i_int = str0[ind[j]].int
    ind1 = where(str1.scan eq iscan and str1.int eq i_int,nind1)

    line0 = str0[ind[j]].data
    line0_0 = line0
    line0 = rebin(line0,npix/10)  ; REBIN
    nline = n_elements(line0)
    std0 = mad(line0)
    err0 = fltarr(nline)+std0

    line1 = str1[ind1].data
    line1_0 = line1
    line1 = rebin(line1,npix/10)  ; REBIN
    std1 = mad(line1)
    err1 = fltarr(nline)+std1

    xx = findgen(nline)
    ;line = reform(im[*,j])
    gd0 = where(finite(line0) eq 1,ngd0)
    if ngd0 eq 0 then goto,BOMB2   ; no good points
    bd0 = where(finite(line0) eq 0,nbd0)
    if nbd0 gt 0 then begin
      if nbd0 lt 15 then begin
        gd0 = where(finite(line0) eq 1,ngd0)
        line0[bd0] = spline(xx[gd0],line0[gd0],xx[bd0])
      endif else begin
        line0[bd0] = 0.0
      endelse
      err0[bd0] = 1e30
    endif
    gd1 = where(finite(line1) eq 1,ngd1)
    if ngd1 eq 0 then goto,BOMB2   ; no good points
    bd1 = where(finite(line1) eq 0,nbd1)
    if nbd1 gt 0 then begin
      if nbd1 lt 15 then begin
        gd1 = where(finite(line1) eq 1,ngd1)
        line1[bd1] = spline(xx[gd1],line1[gd1],xx[bd1])
      endif else begin
        line1[bd1] = 0.0
      endelse
      err1[bd1] = 1e30
    endif


    ; Zero-velocity region to mask
    lozero = 319  ;3188  ;280 ;349  ;1748  ;260 ; 250 ; 280
    hizero = 411  ;4114  ;469 ;447  ;2236  ;410 ; 420 ;400


    ;------------------------
    ; POLZARIZATION=1 FIRST
    ;------------------------
    std1a = mad(line1[where(xx lt lozero or xx gt hizero and (err1 lt 100))])
    line1_sm = SAVGOLSM(line1,[nchanelsm,nchanelsm,4])

    ; --- First poly fit ---
    gd1a = where( (line1 lt median(line1)+4.0*std1a) and (xx lt lozero or xx gt hizero) and (err1 lt 100),ngd1a)
    ;gd1a = where( (line1 lt median(line1)+4.0*std1a),ngd1a)
    if ngd1a eq 0 then goto,bomb2
    ;coef1a = robust_poly_fit(xx[gd1a],line1[gd1a], npoly)
    coef1a = poly_fit(xx[gd1a],line1[gd1a], npoly, measure_errors=err1[gd1a])
    model1a = poly(xx,coef1a)
    diff1a = line1-model1a
    ;diff1a_sm = smooth(diff1a,nchanelsm,/edge_truncate)
    ;diff1a_sm = SAVGOLSM(diff1a,[nchanelsm,nchanelsm,4])
    diff1a_sm = GSMOOTH(diff1a,nchanelsm,widfwhm=3)
  
    ; --- Second poly fit ---
    std1b = mad(diff1a_sm[where(xx lt lozero or xx gt hizero and (err1 lt 100))])
    gd1b = where( (diff1a_sm lt 3.5*std1b) and (xx lt lozero or xx gt hizero) and (err1 lt 100),ngd1b)
    ;gd1b = where( (diff1a_sm lt 3.5*std1b),ngd1b)
    if ngd1b eq 0 then goto,bomb2
    ;coef1b = robust_poly_fit(xx[gd1b],line1[gd1b], npoly)
    coef1b = poly_fit(xx[gd1b],line1[gd1b], npoly, measure_errors=err1[gd1b])
    model1b = poly(xx,coef1b)
    diff1b = line1-model1b
    ;diff1b_sm = smooth(diff1b,nchanelsm,/edge_truncate)
    ;diff1b_sm = SAVGOLSM(diff1b,[nchanelsm,nchanelsm,4])
    diff1b_sm = GSMOOTH(diff1b,nchanelsm,widfwhm=3)

    ; --- Third poly fit ---
    std1c = mad(diff1b_sm[where(xx lt lozero or xx gt hizero and (err1 lt 100))])
    ;gd1c = where( (diff1b_sm lt 3.5*std1c) and (xx lt lozero or xx gt hizero),ngd1c)
    ;gd1c = where( (diff1b_sm lt 3.5*std1c),ngd1c)
    ;if ngd1c eq 0 then goto,bomb2
    bd1c = where( diff1b_sm gt 2.5*std1c and xx gt nchanelsm and xx lt (nline-nchanelsm) and $
                  (xx lt lozero or xx gt hizero) and (err1 lt 100),nbd1c)
    ;line1_corr = line1_sm
    line1_corr = line1
    ;if nbd1c gt 0 then begin
    ;  line1_corr[bd1c] = line1_corr[bd1c] - diff1b_sm[bd1c]
    ;endif
    ; Mask out the emission
    ;mask1c = float( diff1b_sm gt 3.0*std1c and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err1 lt 100)) 
    mask1c = float( diff1b_sm gt 2.0*std1c and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err1 lt 100)) 
    ngrow = 20 ; 10 ;20
    mask1c = CONVOL(mask1c,fltarr(ngrow)+1.0,/center)
    mask1c = mask1c/(mask1c>1)
    ; grow the bad regions by ~20 pixels, but must be above 1sigma at least
    mask1c = float(mask1c eq 1 and diff1b_sm gt 1.0*std1c)
    gd1c = where(mask1c eq 0 and (xx lt lozero or xx gt hizero) and (err1 lt 100),ngd1c)

    ;coef1c = poly_fit(xx[gd1c],line1[gd1c], npoly)
    ;coef1c = robust_poly_fit(xx[gd1c],line1_corr[gd1c], npoly)
    coef1c = poly_fit(xx[gd1c],line1_corr[gd1c], npoly, measure_errors=err1[gd1c])
    model1c = poly(xx,coef1c)
    diff1c = line1-model1c
    ;diff1c_sm = smooth(diff1c,nchanelsm,/edge_truncate)
    ;diff1c_sm = SAVGOLSM(diff1c,[nchanelsm,nchanelsm,4])
    diff1c_sm = GSMOOTH(diff1c,nchanelsm,widfwhm=3)

    ;if nbd0 gt 10 or nbd1 gt 10 then stop
    ;stop

    str1[ind[j]].coef = reform(coef1c)
    str1[ind[j]].model = model1c
    str1[ind[j]].diff = line1-model1c

    t1 = systime(1)
    ;print,t1-t0

    pl=1
    if pl eq 1 then begin
      plot,xx,line1_corr,yr=[-1,1],tit='Plnum=1 Scan='+strtrim(iscan,2)+' Int='+strtrim(i_int,2)
      oplot,xx,gsmooth(line1_corr,nchanelsm,widfwhm=3),co=200,thick=2
      oplot,xx[gd1c],line1_corr[gd1c],ps=1,co=150
      bd = where(mask1c eq 1 and (xx lt lozero or xx gt hizero),nbd)
      if nbd gt 0 then oplot,xx[bd],line1_corr[bd],ps=1,co=250
      oplot,xx,model1c,co=250
      wait,1
      stop
    endif

    ;stop

    ;------------------------
    ; POLARIZATION=0 SECOND
    ;------------------------
    std0a = mad(line0[where(xx lt lozero or xx gt hizero and (err0 lt 100))])
    line0_sm = SAVGOLSM(line0,[nchanelsm,nchanelsm,4])

    ; Correct vel=0 region using plnum=1
    line0_corr = line0
    line0_corr[lozero:hizero] = line0_corr[lozero:hizero] - diff1c[lozero:hizero]
    err0_corr = err0
    err0_corr[lozero:hizero] = err0_corr[lozero:hizero]*sqrt(2.0)

    ; --- First poly fit ---
    ;gd0a = where( (line0_corr lt median(line0_corr)+4.0*std0a),ngd0a)
    gd0a = where( (line0_corr lt median(line0_corr)+4.0*std0a) and (xx lt lozero or xx gt hizero) and (err0 lt 100),ngd0a)
    ;gd0a = where( (line0 lt median(line0)+4.0*std0a) and (xx lt lozero or xx gt hizero),ngd0a)
    if ngd0a eq 0 then goto,bomb2
    ;coef0a = robust_poly_fit(xx[gd0a],line0[gd0a], npoly)
    coef0a = poly_fit(xx[gd0a],line0_corr[gd0a], npoly, measure_errors=err0_corr[gd0a])
    model0a = poly(xx,coef0a)
    diff0a = line0_corr-model0a
    ;diff0a_sm = SAVGOLSM(diff0a,[nchanelsm,nchanelsm,4])
    diff0a_sm = GSMOOTH(diff0a,nchanelsm,widfwhm=3)

    ; --- Second poly fit ---
    std0b = mad(diff0a_sm[where(xx lt lozero or xx gt hizero and (err0 lt 100))])
    gd0b = where( (mask1c eq 0) and (diff0a_sm lt 3.5*std0b) and (xx lt lozero or xx gt hizero) and (err0 lt 100),ngd0b)
    ;gd0b = where( (diff0a_sm lt 3.5*std0b),ngd0b)
    if ngd0b eq 0 then goto,bomb2
    ;coef0b = robust_poly_fit(xx[gd0b],line0[gd0b], npoly)
    coef0b = poly_fit(xx[gd0b],line0_corr[gd0b], npoly, measure_errors=err0_corr[gd0b])
    model0b = poly(xx,coef0b)
    diff0b = line0_corr-model0b
    ;diff0b_sm = SAVGOLSM(diff0b,[nchanelsm,nchanelsm,4])
    diff0b_sm = GSMOOTH(diff0b,nchanelsm,widfwhm=3)

    ; --- Third poly fit ---
    std0c = mad(diff0b_sm[where(xx lt lozero or xx gt hizero and (err0 lt 100))])
    gd0c = where( (mask1c eq 0) and (diff0b_sm lt 3.5*std0c) and (xx lt lozero or xx gt hizero) and (err0 lt 100),ngd0c)
    ;gd0c = where( (diff0b_sm lt 3.5*std0c),ngd0c)
    if ngd0c eq 0 then goto,bomb2
    bd0c = where( diff0b_sm gt 2.5*std0c and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err0 lt 100),nbd0c)
    ;line0_corr = line0_sm
    line0_corr2 = line0_corr
    if nbd0c gt 0 then begin
      line0_corr2[bd0c] = line0_corr2[bd0c] - diff0b_sm[bd0c]
      ;line0_corr[bd0c] = line0_sm[bd0c] - diff0b_sm[bd0c]
    endif
    ;coef0c = robust_poly_fit(xx[gd0c],line0_corr[gd0c], npoly)
    coef0c = poly_fit(xx[gd0c],line0_corr2[gd0c], npoly, measure_errors=err0_corr[gd0c])
    model0c = poly(xx,coef0c)
    diff0c = line0_corr-model0c
    ;diff0c_sm = SAVGOLSM(diff0c,[nchanelsm,nchanelsm,4])
    diff0c_sm = GSMOOTH(diff0c,nchanelsm,widfwhm=3)

    str0[ind[j]].coef = reform(coef0c)
    str0[ind[j]].model = model0c
    str0[ind[j]].diff = line0-model0c

    t2 = systime(1)
    ;print,t2-t1

    ; From Toney's email:
    ;  There is a known standing wave that has a period of 1.5 MHz.
    ; It arises from a total pathlenght of 200 meters and it is known
    ; to arise from a double reflection (+/- lambda/8 focus shifts
    ; change the phase by 180 degrees).  This staning wave is highly
    ; linearly polarized and should only be in one polarization - the
    ; Y polarization.  The origin of this standing wave (i.e. where it
    ; is reflecting) is unkown.
    ;  If this is the only standing wave present, then it should
    ; behave as A*cos(2*pi*4*d/lambda) with A being the amplitude,
    ; lambda being the wavelength of a given channel and d is the
    ; distance between the reflecting surfaces (in this case it would
    ; be 25 meters due to the double reflection).
    ; THIS WORKS, but you need 8*25 instead of 4*25
    ; xfreq = dindgen(16384)*762.93945+1417473538.8302d0
    ; wave = 299792458d0/xfreq
    ; xchan = dindgen(16384)
    ; plot,cos(2.0*!dpi*8.0*25.0/wave)
    ; oplot,cos(2.0*!dpi*(xchan-710.)/1965.),co=250

    ; --- First Cosine fit ---
    ; Fix the amplitude and wavelength!
    phase = 120.0  ;100.0
    ;if i gt 0 or j gt 0 then phase = fpar2[2]  ; get phase from last integration
    initpars = [0.020, 191.0, phase]
    ;initpars = [0.020, 955.0, phase]
    parinfo = replicate({value:0.0,fixed:0,limited:[1,1],limits:[0.0,0.0]},3)
    parinfo[0].limits = [0.00,0.04]
    ;;parinfo[0].fixed=1 & parinfo[0].value=0.01
    parinfo[0].fixed=1 & parinfo[0].value=0.02
    parinfo[1].limits = [173.,207.0]
    parinfo[1].fixed=1 & parinfo[1].value=191.0
    ;parinfo[1].limits = [50.,1035.0]
    ;parinfo[1].fixed=1 & parinfo[1].value=955
    ;parinfo[1].fixed=1 & parinfo[1].value=200.0
    parinfo[2].limits = [0.0,250.0]
    ;parinfo[2].limits = [0.0,1250.0]

    ;diff01 = diff0c_sm - diff1c_sm
    ;gd0d = where(xx gt nchanelsm and xx lt (nline-nchanelsm),ngd0d)
    ;gd0d = where(xx gt nchanelsm and xx lt (nline-nchanelsm) and (xx lt lozero or xx gt hizero),ngd0d)
    gd0d = where( (mask1c eq 0) and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err0 lt 100),ngd0d)
    ;fpar = MPFITFUN('func_cos',xx[gd0d],diff01[gd0d],err0[gd0d],initpars,/quiet,parinfo=parinfo)
    fpar = MPFITFUN('func_cos',xx[gd0d],diff0c_sm[gd0d],err0_corr[gd0d],initpars,/quiet,parinfo=parinfo,$
                    status=status1,niter=niter1)
    if status1 lt 1 then stop,'STATUS1<1'
    swave = func_cos(xx,fpar)
    diff0d = line0_corr-model0c-swave
    ;diff0d_sm = SAVGOLSM(diff0d,[nchanelsm,nchanelsm,4])
    ;diff0d_sm = SAVGOLSM(diff0d,[nchanelsm,nchanelsm,4])
    diff0d_sm = GSMOOTH(diff0d,nchanelsm,widfwhm=3)
    std0d = MAD(diff0d_sm[where(xx lt lozero or xx gt hizero and (err0 lt 100))])


    t3 = systime(1)
    ;print,t3-t2

    ; --- Poly+Cosine Fit ---
    initpars2 = [fpar,reform(coef0c)]
    parinfo2 = replicate({value:0.0,fixed:0,limited:[1,1],limits:[0.0,0.0]},n_elements(initpars2))
    parinfo2[0].limited=1 & parinfo2[0].limits = [0.00,0.04]
    parinfo2[1].limited=1 & parinfo2[1].limits = [173.0,207.0]
    parinfo2[2].limited=1 & parinfo2[2].limits = [0.0,250.0]
    ;parinfo2[1].limited=1 & parinfo2[1].limits = [50.0,1035.0]
    ;parinfo2[2].limited=1 & parinfo2[2].limits = [0.0,1250.0]

    for k=3,n_elements(initpars2)-1 do parinfo2[k].limits=[-10.0,10.0]*abs(initpars2[k])
    ; Remove emission using plnum=1
    ;bd = where(diff1c_sm gt 2.0*std0c and xx gt nchanelsm and xx lt (nline-nchanelsm),nbd)
    bd0e = where(diff1c_sm gt 2.0*std0c and xx gt nchanelsm and xx lt (nline-nchanelsm) and (xx lt lozero or xx gt hizero),nbd0e)
    line0_corr3 = line0_corr
    if nbd0e gt 0 then begin
      line0_corr3[bd0e] = line0_corr3[bd0e] - diff1c_sm[bd0e]
    endif
    ; Bin and smooth
    ;line0b = FREBIN(line0_corr3,nbin)
    ;;line0b = SAVGOLSM(line0b,[10,10,4])
    ;xxb = FREBIN(xx,nbin)
    ;err0b = FREBIN(err0_corr,nbin)
    ;line0b = SAVGOLSM(line0_corr3,[40,40,4])
    line0b = line0_corr3
    xxb = xx
    err0b = err0_corr
    t4 = systime(1)
    ; Mask out the emission
    ;mask0e = float( diff0d_sm gt 3.0*std0d and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err0 lt 100) ) 
    mask0e = float( (diff0d_sm gt 2.0*std0d or diff1c_sm gt 2.0*std1c) and xx gt nchanelsm and xx lt (nline-nchanelsm) and (err0 lt 100) ) 
    ngrow = 20 ; 10 ;20
    mask0e = CONVOL(mask0e,fltarr(ngrow)+1.0,/center)
    mask0e = mask0e/(mask0e>1)
    ; grow the bad regions by ~20 pixels, but must be above 1sigma at least
    ;mask0e = float(mask0e eq 1 and diff0d_sm gt 1.0*std0d)
    gd0e = where(mask0e eq 0 and (xxb lt lozero or xxb gt hizero) and (err0 lt 100),ngd0e)
    ;fpar2 = MPFITFUN('func_cospoly',xxb[gd0e],line0b[gd0e],err0b[gd0e],initpars2,parinfo=parinfo2,/quiet,$
    t5 = systime(1)
    fpar2 = MPFITFUN('func_cospoly',xxb[gd0e],line0b[gd0e],err0b[gd0e],initpars2,parinfo=parinfo2,/quiet,$
                     bestnorm=bestnorm,status=status,niter=niter,dof=dof)
    if status lt 1 then stop,'STATUS<1'
    t6 = systime(1)
    chisq = sqrt(bestnorm/dof)
    fsurf = func_cospoly(xx,fpar2)
    swave2 = func_cos(xx,fpar2[0:2])


    str0[ind[j]].swave = swave
    str0[ind[j]].swavepar = fpar
    str0[ind[j]].swave2 = swave2
    str0[ind[j]].fmodel = fsurf
    str0[ind[j]].pars = fpar2

    ; Need to remove emission
    ; Maybe do everything a second time and use the distribution of
    ;   the standing wave parameters to constrain it more

    ; Combine the two polarizations
    line_combine = ( (line1-model1c) + (line0-fsurf) )*0.50
    ;comb[ind[j]].red = line_combine

    ;t6 = systime(1)
    ;print,t5-t4
    ;print,t6-t3

    ; Final structure
    final[ind[j]].p0.data = line0
    final[ind[j]].p0.pars = fpar2
    final[ind[j]].p0.model = fsurf
    final[ind[j]].p0.red = line0-fsurf
    bd0 = where(err0 gt 100,nbd0)
    if nbd0 gt 0 then final[ind[j]].p0.red[bd0]=0.0   ; mask bad data

    final[ind[j]].p1.data = line1
    final[ind[j]].p1.pars = reform(coef1c)
    final[ind[j]].p1.model = model1c
    final[ind[j]].p1.red = line1-model1c
    bd1 = where(err1 gt 100,nbd1)
    if nbd1 gt 0 then final[ind[j]].p1.red[bd1]=0.0   ; mask bad data

    final[ind[j]].comb = line_combine
    ;final[ind[j]].comb_bin = FREBIN(line_combine,500)

    ;if n_elements(mask0e)-total(mask0e)-120 gt 10 then stop
    pl=1
    if pl eq 1 then begin
      yr = [-1,1]*5*mad(line0b)
      plot,xxb,line0b,tit='Plnum=0 Scan='+strtrim(iscan,2)+' Int='+strtrim(i_int,2),yr=yr,ys=1
      oplot,xx,gsmooth(line0_corr,nchanelsm,widfwhm=3),co=200,thick=2
      oplot,xxb[gd0e],line0b[gd0e],ps=1,co=150
      bd = where(mask0e eq 1 and (xx lt lozero or xx gt hizero),nbd)
      if nbd gt 0 then oplot,xxb[bd],line0b[bd],ps=1,co=250
      oplot,xx,fsurf,co=250
      wait,1
      stop
    endif
    ;;plot,xx,line0
    ;;oplot,xxb,line0b,co=250
    ;;oplot,xx,fsurf,co=150
    ;
    ;wait,1
    ;wait,0.3
    stop

    BOMB2:

  end

  BOMB:
  ;stop
end

print,'Finished'

; Remove baseline from plnum=0
diff0 = final.p0.data
nint = n_elements(final)
for i=0,nint-1 do diff0[*,i]=final[i].p0.data-poly(xx,final[i].p0.pars[3:*])
diff0b = smooth(diff0,[20,20],/edge)
swave = final.p0.red*0.0
for i=0,nint-1 do swave[*,i]=func_cos(xx,final[i].p0.pars[0:2])
swaveb = smooth(swave,[20,20],/edge)
diff01 = diff0-final.p1.red
diff01b = smooth(diff01,[20,20],/edge)

;diff0 = str0.diff
;diff0b = smooth(diff0,[20,10],/edge)
;dum = str0.data-str0.fmodel
;dum2 = smooth(dum,[20,10],/edge)
;diff1 = str1.diff
;diff1b = smooth(diff1,[20,10],/edge)
;displayc,diff0b,/zscale,tit=file
red0 = smooth(final.p0.red,[20,10],/edge)
red1 = smooth(final.p1.red,[20,10],/edge)
m0 = median(red0)
m1 = median(red1)

; Plots
;window,0,xsize=1200,ysize=500
;wset,0
window,0
displayc,red0-m0,min=-0.06,max=0.06,position=[0,0,0.5,1],tit='Plnum=0'
displayc,red1-m1,min=-0.06,max=0.06,position=[0.5,0,1.0,1.0],/noerase,tit='Plnum=1'
xyouts,0.5,0.97,file_basename(file,'.fits'),charsize=1.5,/normal,align=0.5

wait,1
;stop

displayc,diff01b,min=-0.06,max=0.06,position=[0,0,0.5,1]
displayc,swaveb,min=-0.06,max=0.06,position=[0.5,0,1,1],/noerase

wait,1
;stop


; Subtract median from both polarizations
nint = n_elements(final)
sm0 = SMOOTH(final.p0.red,[20,20],/edge)
sm1 = SMOOTH(final.p1.red,[20,20],/edge)
sm0[lozero:hizero,*] = !values.f_nan
sm1[lozero:hizero,*] = !values.f_nan
med0 = median(sm0[20:npix-21,20:nint-20-1])  ; don't include edges
med1 = median(sm1[20:npix-21,20:nint-20-1])

;sm0 = SMOOTH(final.p0.red,[200,20],/edge)
;sm1 = SMOOTH(final.p1.red,[200,20],/edge)
;med0 = median(sm0[200:4799,20:nint-20-1])  ; don't include edges
;med1 = median(sm1[200:4799,20:nint-20-1])

final.comb_corr = ( (final.p0.red-med0) + (final.p1.red-med1) )*0.50


if n_elements(tag) eq 0 then tag='_all'

source = file_basename(file,'.fits')
if keyword_set(save) then begin
  fil = '../plots/'+source+'_intchan'+tag
  ps_open,fil,/color,thick=4
  device,/inches,xsize=25,ysize=8
  displayc,diff01b,min=-0.05,max=0.05,xtit='Channel',ytit='Integration Number',tit='Plnum=0 Raw-Baseline-P1',$
           position=[0.0, 0.0, 0.2, 1.0]
  displayc,swaveb,min=-0.05,max=0.05,xtit='Channel',ytit='Integration Number',tit='Plnum=0 Fitted Standing Wave',$
           position=[0.2, 0.0, 0.4, 1.0],/noerase
  displayc,smooth(final.p0.red,[20,20],/edge)-med0,min=-0.05,max=0.05,xtit='Channel',ytit='Integration Number',tit='Plnum=0',$
           position=[0.40, 0.0, 0.60, 1.0],/noerase
  displayc,smooth(final.p1.red,[20,20],/edge)-med1,min=-0.05,max=0.05,xtit='Channel',ytit='Integration Number',tit='Plnum=1',$
           position=[0.60, 0.0 ,0.80, 1.0],/noerase
  displayc,smooth(final.comb_corr,[20,20],/edge),min=-0.05,max=0.05,xtit='Channel',ytit='Integration Number',tit='Combined',$
           position=[0.80, 0.0, 1.0, 1.0],/noerase
  xyouts,0.5,0.97,source,charsize=1.2,align=0.5,/normal
  ;oplot,[chan1,chan1],[-1000,3000],linestyle=2
  ;oplot,[chan2,chan2],[-1000,3000],linestyle=2
  ps_close
  ps2gif,fil+'.ps',rot=-90
  FILE_DELETE,fil+'.ps',/allow,/quiet
endif

;stop

save,final,file='../data/'+source+tag+'.dat'
;save,final,file='../data/'+source+'_red4.dat'
;save,final,file='../data/'+source+'_red3.dat'
;save,final,file='../data/'+source+'_red2.dat'

stop

if keyword_set(stp) then stop

end
