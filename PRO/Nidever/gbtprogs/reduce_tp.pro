pro reduce_tp,str,out

if n_elements(i) eq 0 then i=0

if n_elements(str) eq 0 then $
  str = mrdfits('GBT10B_037_01-test.fits',1)

x = findgen(32768)
y = str[i].data[3000:30000]
x2 = x[3000:30000]
lincoef = poly_fit(x2,y,1)

y2 = y-poly(x2,lincoef)
x3 = scale_vector(x2,-2,2)
;coef = robust_poly_fit(x2,y2,15)
coef = robust_poly_fit(x2,y2,10)
rms = mad(y2-poly(x2,coef))
gd = where(abs(y2-poly(x2,coef)) lt 3.0*rms,ngd)

initpar = [double(coef),dblarr(5)]
par = mpfitfun('poly',x2[gd],y2[gd],x2[gd]*0+1,initpar,/quiet)

;base = poly(x2,lincoef)+poly(x2,coef)
base = poly(x2,lincoef)+poly(x2,par)

;plot,y
;oplot,base,co=250
;wait,0.5
;stop

spec = y/base-1
smspec = smooth(spec,20)

;func = 'P[0]*sin((X/P[1]-P[2])/!radeg)'
;initpar2 = [0.01,16.0,50.0]
;;gd2 = where(x2 lt 13877 or x2 gt 15862,ngd)
;rms2 = mad(smspec)
;gd2 = where(abs(smspec) lt 3*rms2,ngd2)
;spar = mpfitexpr(func,x2[gd2],smspec[gd2],x2[gd2]*0+1,initpar2,/quiet)
;yfit = mpevalexpr(func,x2,spar)
;
;plot,x2,smspec,yr=[-0.05,0.05],xs=1,tit=strtrim(i,2)
;oplot,x2,yfit,co=250
;oplot,0.01*sin(x2/20./!radeg),co=250
;wait,0.5

;charsize = 1.0
;file = 'tp_polysub_9MHzwiggle'
;ps_open,file,thick=3,/color
;device,/inches,xsize=13,ysize=7
;charsize = 1.4
;
;plot,x2,smspec,yr=[-0.05,0.05],xs=1,xtit='Channel',charsize=charsize,$
;     tit='9 MHz wiggle (Raw Spectrum - 15th order polynomial) with 20 channel boxcar smoothing'
;
;ps_close
;ps2gif,file+'.ps',rot=-90

;stop

smy2 = smooth(y2,20)

; Loop over regions and fit a "local" sinusoid
n = 15
nx = n_elements(x2)
step = ceil(nx/float(n))
size = ceil(step*1.5)
basesum = fltarr(nx)
narr = fltarr(nx)
nclip = ceil(0.05*size) ; clip ends
for j=0,n-1 do begin
  lo = j*step
  hi = (lo+size) < (nx-1)
  ix = x2[lo:hi]
  ;ismspec = smspec[lo:hi]
  ismspec = smy2[lo:hi]

  ;ind = where(abs(ismspec) lt 3.0*rms2,nind)

  ;spar = mpfitexpr(func,ix[ind],ismspec[ind],ix[ind]*0+1,initpar2,/quiet)
  ;yfit = mpevalexpr(func,ix,spar)
  ;spar = robust_poly_fit(ix[ind],ismspec[ind],6)
  ;yfit = poly(ix,spar)
  spar = robust_poly_fit(ix,ismspec,3)
  yfit = poly(ix,spar)

  diff = ismspec-yfit
  drms = mad(diff)
  ind2 = where(abs(diff) lt 3.0*drms,nind2)
  ;spar2 = mpfitexpr(func,ix[ind2],ismspec[ind2],ix[ind2]*0+1,spar,/quiet)
  ;yfit2 = mpevalexpr(func,ix,spar2)
  spar2 = robust_poly_fit(ix[ind2],ismspec[ind2],3)
  yfit2 = poly(ix,spar2)

  ;yr = [-0.05,0.05]
  ;yr = minmax(ismspec)
  ;plot,ix,ismspec,yr=yr,xs=1,tit=strtrim(i,2)
  ;oplot,ix,yfit,co=250
  ;oplot,ix,yfit2,co=150

  ; clip ends a little bit
  ;  makes the fits much smoother
  if j gt 0 then loclip=nclip else loclip=0
  if j lt (n-1) then hiclip=nclip else hiclip=0
  nyfit = n_elements(yfit)

  basesum[lo+loclip:hi-hiclip] += yfit2[loclip:nyfit-1-hiclip]
  narr[lo+loclip:hi-hiclip]++

  ;stop

end

base2 = basesum/(narr>1)

fbase = poly(x2,lincoef)+base2
fspec = y/fbase-1


;file = 'tp_basefit'
;ps_open,file,thick=3,/color
;device,/inches,xsize=13,ysize=7
;charsize = 1.4

plot,x2,smy2,yr=[-0.5,0.5],xs=1,xtit='Channel',charsize=charsize,$
     tit='Raw Spectrum (with 20 channel boxcar smoothing) and Baseline fit'
oplot,x2,base2,co=250

legend,['Data','Baseline fit'],textcolor=[0,250],/top,/left,charsize=1.2,thick=4

;ps_close
;ps2gif,file+'.ps',rot=-90

wait,1
;stop

;file = 'tp_basesub'
;ps_open,file,thick=3,/color
;device,/inches,xsize=13,ysize=7
;charsize = 1.4

;plot,x2,smooth(fspec,20),yr=[-0.05,0.05],xs=1,tit=strtrim(i,2)
plot,x2,smooth(fspec,20),yr=[-0.05,0.05],xs=1,charsize=charsize,xtit='Channel',$
     tit='Baseline subtracted Total Power Spectrum (with 20 channel boxcar smoothing)'

;plot,x2,smspec,yr=[-0.05,0.05],xs=1,tit=strtrim(i,2)
;oplot,x2,base2,co=250

;ps_close
;ps2gif,file+'.ps',rot=-90

wait,1

;stop

end
