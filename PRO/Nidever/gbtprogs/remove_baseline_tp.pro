pro remove_baseline_tp,file,save=save,tag=tag,stp=stp

;file = '../data/MSTIP-survey_ses1_s9-37_tp.fits'

if n_elements(file) eq 0 then begin
  print,'remove_baseline_tp,file,save=save,tag=tag,stp=stp'
  return
endif

forward_function func_cos, func_2cos, func_cospoly

test = file_test(file)
if test eq 0 then begin
  print,'File ',file,' NOT FOUND'
  return
endif

cspeed = 2.99792458d5  ; speed of light in km/s

if n_elements(tag) eq 0 then tag='_red1'
;if n_elements(tag) eq 0 then tag='_red2'

dir = file_dirname(file)+'/'

setdisp,/silent

; --- Restore the file ---
print,'Loading ',file
str = mrdfits(file,1)
ind0 = where(str.plnum eq 0,nind0)
ind1 = where(str.plnum eq 1,nind1)
nstr = n_elements(str)

; Make the velocity array
n = n_elements(str[0].data)
f = (dindgen(n)+1-str[0].crpix1)*str[0].cdelt1 + str[0].crval1
v = (f-str[0].restfreq)/str[0].restfreq*cspeed
; THIS IS ONLY APPROXIMATELY CORRECT!!!!
zerovel = first_el(minloc(abs(v)))

goto,skip2

; Find RFI by looking at peaks at constant FREQUENCY!!!!!
shift = (str[0].crval1-str.crval1)/abs(str[0].cdelt1)
shift = shift-min(shift)   ; make 0 the minimum
data = fltarr(n+ceil(max(shift)),nstr)+!values.f_nan
for i=0,nstr-1 do begin
  if i mod 500 eq 0 then print,i
  sh = round(shift[i])
  cont = median(str[i].data,10,/even)
  data[sh:sh+n-1,i] = str[i].data-cont ; remove the continuum
  ;data[sh:sh+n-1,i] = str[i].data/cont-1  ; remove the continuum
end

tot = total(data,2,/nan)
medall = median(data,dim=2)
;tot = tot-median(tot)
;num = total(finite(data),2)
;tot2 = tot/(num>1)
;med = median(data,dim=2)
med = median(tot,8,/even)  ; remove smooth component
;  if you use an even binsize and /even then it will always
;  take a mean and med will rarely have an actual value of tot
;  which screws up the sigma calculation
diff = tot-med
sig0 = mad(diff,/zero)
h = histogram(diff,binsize=sig0/10,min=-sig0*10,max=sig0*10,locations=x)
yfit = mpfitpeak(x,h,pars,nterms=3,/gaussian,/positive,estimates=[max(h),0.0,sig0])
sig = pars[2]
bd = where(diff GT 10*sig,nbd)
plot,diff,yr=[-1,1]*sig*20
if nbd gt 0 then oplot,bd,diff[bd],ps=1,co=250

;stop

skip:

;sz = size(data)
;bin = 20
;n2 = sz[2]/bin
;data2 = rebin(data[*,0:n2*bin-1],sz[1],n2)

; RFI at:
;display,smooth(data[6200:6700,*],[1,20],/edge),min=-0.2,max=0.2
; 3547
; 3623
; 3642
; 5753
; 10184-10203 three lines
; 10373
; 13466, 13479
; 16655
; 16750  wide one
; 17897
; 20671
; 21655 not sure?
; 23208
; 23298
; 29850 crazy stuff, very wide
; 32160
pixarr = [3547, 3623, 3642, 5753, 10196,$
          10374, 13466, 13479, 16655, 16749, 17897, 20672,$
          21655, 23209, 23300, 29851, 32161]
widarr = [13, 3, 15, 3, 26, 10, 3, 10, 3, 50, 5, 15, 3, 10,$
          23, 270, 11]

; Make the frequency array
ind0 = first_el(minloc(shift))
ndata = n_elements(data[*,0])
f = (dindgen(ndata)+1-str[ind0].crpix1)*str[ind0].cdelt1 + str[ind0].crval1
freqarr = f[pixarr]
rfi = replicate({pix:0L,wid:0L,freq:0.0d0},n_elements(pixarr))
rfi.pix = pixarr
rfi.wid = widarr
rfi.freq = freqarr
nrfi = n_elements(rfi)

; There are some diagonal lines near 7000 that look like real sources

; FIX THE RFI
; Loop through the data and fix the RFI
for i=0,nstr-1 do begin
  if i mod 100 eq 0 then print,'Scan ',strtrim(i,2)
  ff = (dindgen(n)+1-str[i].crpix1)*str[i].cdelt1 + str[i].crval1
  df = abs(str[0].cdelt1)
  ; Loop through RfI
  for j=0,nrfi-1 do begin
    bd = where(abs(ff-rfi[j].freq) lt 1.5*df*rfi[j].wid,nbd)
    if nbd gt 0 then begin
      index = median(bd)
      lo = round(index-5*rfi[j].wid) ; region to use for median
      hi = round(index+5*rfi[j].wid)
      bin = round(3*rfi[j].wid)
      med = median(str[i].data[lo:hi],bin) ; use median
      bd2 = bd-lo     ; indices for "med"
      str[i].data[bd] = med[bd2]
    endif
  end
end

;stop

skip2:

; Calculate the frequency shift in pixels
freqpix = abs(str[0].cdelt1)
restfreq = str[0].crval1
nfshift = round(3.5d6/freqpix)


; Initialize the final structure
nbin = 5
;npix = (32768+nfshift)/nbin
;npix = 16384L/nbin
npoly =  5 ;4; 3 ;5 ;4 ;5 ;4
;lochan = 0L ;3000L
;hichan = npix*nbin-1
lochan = 1600L
hichan = 15249L

;; shift the spectra
;specarr3 = fltarr(nspec/2,nchan+nshift)
;nspecarr = lonarr(nchan+nshift)
;for i=0,nspec/2-1 do begin
;  lo = (((i+1) mod 2) * nshift)
;  ;specarr2[i,lo] = specarr[i,*]
;  specarr3[i,lo:lo+nchan-1] = specarr2[i,*]
;  nspecarr[lo:lo+nchan-1]++
;end


ind0sig = where(str.plnum eq 0 and str.sig eq 'T',nint)   ;unique integrations
str0 = str[ind0sig]

; Final structure
;nfpix = (16384+nfshift)/5
nfpix = (hichan-lochan+1)/5
;dum0 = {red:fltarr(npix)}
;dum1 = {red:fltarr(npix)}
dum = {object:'',file:'',ra:0.0d0,dec:0.0d0,scan:0L,procseqn:0,int:0,p0:fltarr(nfpix),p1:fltarr(nfpix),spec:fltarr(nfpix)}
final = replicate(dum,nint)
STRUCT_ASSIGN,str0,final
final.ra = str0.crval2
final.dec = str0.crval3

ui = uniq(str0.scan,sort(str0.scan))
nscan = n_elements(ui)
scans = str0[ui].scan
;nscan = n_elements(str0)/37

zvreg = where(abs(v) lt 70,nzvreg)

; Loop through the scans
for i=0,nscan-1 do begin

  if (i+1) mod 5 eq 0 then print,strtrim(i+1,2),'/',strtrim(nscan,2)

  iscan = scans[i]
  ;iscan = min(str0.scan)+i
  ind = where(str0.scan eq iscan,nind)
  if nind eq 0 then goto,BOMB


  ;im = str0[ind].data[4500:9499]

  for j=0,nind-1 do begin
    i_int = str0[ind[j]].int

    for p=0,1 do begin

      ind_sig = where(str.scan eq iscan and str.int eq i_int and str.sig eq 'T' and str.plnum eq p,nindsig)
      ;ind_ref = where(str.scan eq iscan and str.int eq i_int and str.sig eq 'F' and str.plnum eq p,nindref)
      sig = str[ind_sig].data
      ;ref = str[ind_ref].data

      ; Remove the SIG baseline/continuum
      nel = n_elements(sig)

      med1 = median(sig,100)
      x = findgen(nel)
      std = MAD(sig-med1,/zero)
      ;; Use a wider median for zero velocity
      ;med2 = median(sig[14000L:19000L],1000)
      ;; Continuum, paste together
      ;sigcont = med1
      ;sigcont[15000L:18000L] = med2[1000L:4000L]
      ;; Calibrate
      ;calsig = sig/sigcont-1

      ; B-spline fit
      invvar = fltarr(nel)+1.0/std^2
      ;if nbd gt 0 then invvar[bd] = 0
      nord = 3           ;4
      bkspace = 500 ;50
      ;invvar[0:59]=0
      ;invvar[280:*]=0
      dum = bspline_iterfit(x,sig,invvar=invvar,nord=nord,bkspace=bkspace,yfit=model1)
      tinvvar = invvar
      ;bd = where(sig-model1 gt 3*std,nbd)
      ;if nbd gt 0 then tinvvar[bd]=0.0
      tinvvar[zvreg] = 0.0  ; mask zero-velocity region
      dum = bspline_iterfit(x,sig,invvar=tinvvar,nord=nord,bkspace=bkspace,yfit=model)
      ; remove continuum, instrumental profile
      spec = sig/model-1

      ; Now we bin and stuff it in the final structure
      ;binspec = REBIN(spec[0:nfpix*nbin-1],nfpix)
      ;findex = where(final.scan eq iscan and final.int eq i_int,nind)
      ;if p eq 0 then final[findex].p0=binspec else final[findex].p1=binspec
      binspec = REBIN(spec[lochan:lochan+nfpix*nbin-1],nfpix)
      findex = where(final.scan eq iscan and final.int eq i_int,nind)
      if p eq 0 then final[findex].p0=binspec else final[findex].p1=binspec

      BOMB:

      ;stop

    endfor ; polarization loop

    ; Now combine the two polarizations
    final[findex].spec = (final[findex].p0+final[findex].p1)*0.5

    ;stop
    
  endfor ; int loop
endfor ; scan loop

source = file_basename(file,'.fits')
;save,final,file='../data/'+source+tag+'.dat'
outfile = dir+source+tag+'.fits'
print,'Writing to ',outfile
MWRFITS,final,outfile,/create

;stop

end
