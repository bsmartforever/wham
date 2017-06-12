function mass,bv=bv,sptype=sptype,mjup=mjup,plot=plot

; This function returns the mass based on either the B-V
; color or the spectral type.  Only for Main Sequence stars.

;From Gray's book.

sp = ['O8','B0','B2','B5','B8','A0','A2','A5','A6','A7','A8','A9',$
      'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','G0','G1',$
      'G2','G3','G4','G5','G6','G7','G8','G9','K0','K1','K2','K3',$
      'K4','K5','K7','M0','M1','M2']

bvarr = [-0.305d,-0.295d,-0.245d,-0.163d,-0.100d,0.000d,0.055d,0.143d,$
       0.170d,0.198d,0.228d,0.265d,0.300d,0.329d,0.354d,0.380d,0.404d,$
       0.431d,0.464d,0.496d,0.530d,0.561d,0.583d,0.608d,0.625d,0.642d,$
       0.657d,0.672d,0.690d,0.713d,0.740d,0.776d,0.819d,0.866d,0.912d,$
       0.966d,1.030d,1.150d,1.310d,1.420d,1.480d,1.510d]

m = [23d,13.2d,8.7d,4.57d,2.88d,2.40d,2.19d,1.86d,1.80d,1.74d,1.66d,$
     1.62d,1.55d,1.51d,1.45d,1.41d,1.38d,1.32d,1.29d,1.26d,1.20d,$
     1.17d,1.12d,1.10d,1.06d,1.04d,1.00d,0.98d,0.95d,0.92d,0.89d,$
     0.85d,0.82d,0.79d,0.76d,0.74d,0.71d,0.68d,0.60d,0.52d,0.48d,$
     0.42d]

;Getting B-V or Spectral Type from sptype.pro
sptype,bv=bv,sptype=sptype,/noprint

; From Allen's Astrophysical Quantities

sp2 = ['O3','O5','O6','O8','B0','B3','B5','B8','A0','A5','F0','F5',$
       'G0','G5','K0','K5','M0','M2','M5','M8']

m2 = [120d,60d,37d,23d,17.5d,7.6d,5.9d,3.8d,2.9d,2.0d,1.6d,1.4d,1.05d,$
      0.92d,0.79d,0.67d,0.51d,0.40d,0.21d,0.06d]

;getting bvarr2
bvarr2 = dblarr(n_elements(sp2))
for i=0,n_elements(sp2)-1 do begin
  ind = where(sp eq sp2(i),nind)
  if nind eq 0 then bvarr2(i) = -1
  if nind gt 0 then bvarr2(i) = bvarr(ind(0))
  ;these are from Carroll & Ostlie
  if sp2(i) eq 'O6' then bvarr2(i) = -0.33d
  if sp2(i) eq 'B3' then bvarr2(i) = -0.20d
  if sp2(i) eq 'M5' then bvarr2(i) =  1.64d
  if sp2(i) eq 'M8' then bvarr2(i) =  1.93d
endfor

g = where(bvarr2 ne -1,ng)
sp2 = sp2(g)
m2 = m2(g)
bvarr2 = bvarr2(g)

;stop

;using Allen's masses
old_bvarr = bvarr       ;saving
old_m = m               ;saving
bvarr = bvarr2
m = m2


if bv lt min(bvarr) then begin
  print,'B-V TOO LOW'
  return,-1
endif
if bv gt max(bvarr) then begin
  print,'B-V TOO LARGE'
  return,-1
endif

logm = alog10(m)			; log mass
nbv = dindgen(1000)/999.*(max(bvarr)-min(bvarr))+min(bvarr)
nm = spline(bvarr,logm,nbv)

lo = first_el(where(nbv le bv),/last)
x = nbv(lo:lo+1)
y = nm(lo:lo+1)
slope = (y(1)-y(0))/(x(1)-x(0))
yint = y(0)-slope*x(0)
logmass = slope*bv+yint
mass = double(10.)^double(logmass)

if keyword_set(plot) then begin
  psym8
  plot,bvarr,logm,ps=8,tit='Log Mass vs. B-V',xtit='B-V',ytit='Log Mass (in Solar Masses)'
  oplot,nbv,nm
  oplot,[bv],[logmass],ps=4
  arrow,bv+0.02,logmass+0.5,bv,logmass,/data,thick=2
  xyouts,bv+0.021,logmass+0.68,'B-V = '+strmid(strtrim(bv,2),0,5),charsize=0.9
  xyouts,bv+0.021,logmass+0.55,'Mass = '+strmid(strtrim(mass,2),0,5)+' Msun',charsize=0.9
endif

;convert to jupiter masses
if keyword_set(mjup) then begin
  msun = 1.9891d30  ;in kg
  mjup = 1.8987d27  ;in kg
  fac = msun/mjup
  mass = mass * fac
endif

;stop

return,mass

; THIS IS THE OLD STUFF
;if bv lt -0.1 then begin
;  less = where(bvarr lt bv,nless)
;  lo = max(less)
;  high = where(bvarr ge bv,nhigh)
;  hi = min(high)
;
;  slope = (m(hi)-m(lo))/(bvarr(hi)-bvarr(lo))
;  diff = bv-bvarr(lo)
;  mass = m(lo) + diff * slope
;
;  gd = where(bvarr lt 0.0,ngd)
;
;  if keyword_set(plot) then begin
;    psym8
;    plot,bvarr(gd),m(gd),ps=-8,tit='Mass vs. B-V',xtit='B-V',ytit='Mass (in Solar Masses)'
;    oplot,[bv],[mass],ps=4
;    arrow,bv+0.02,mass+2,bv,mass,/data,thick=2
;    xyouts,bv+0.021,mass+2.1,'B-V = '+strmid(strtrim(bv,2),0,5),charsize=1.3
;  endif
;
;
;  ;stop
;
;endif
;
;if bv ge -0.1 then begin
;  gd = where(bvarr ge 0.1,ngd)
;  minbv = min(bvarr(gd))
;  maxbv = max(bvarr(gd))
;  bv2 = dindgen(1001)/1000.*(maxbv-minbv) + minbv
;  nm = spline(bvarr(gd),m(gd),bv2)
;
;  diff = abs(bv2-bv)
;  ind = first_el(minloc(diff))
;  mass = nm(ind(0))
;
;  if keyword_set(plot) then begin
;    psym8
;    plot,bvarr(gd),m(gd),ps=8,tit='Mass vs. B-V',xtit='B-V',ytit='Mass (in Solar Masses)'
;    oplot,bv2,nm
;    oplot,[bv2(ind(0))],[mass],ps=4
;    arrow,bv2(ind(0))+0.1,mass+0.1,bv2(ind(0))+0.03,mass+0.03,/data,thick=2
;    xyouts,bv2(ind(0))+0.13,mass+0.13,'B-V = '+strmid(strtrim(bv,2),0,5),charsize=1.3
;  endif
;
;endif
;
;;stop
;
;;convert to jupiter masses
;if keyword_set(mjup) then begin
;  msun = 1.9891d30  ;in kg
;  mjup = 1.8987d27  ;in kg
;  fac = msun/mjup
;  mass = mass * fac
;endif
;
;stop
;
;return,mass

end
