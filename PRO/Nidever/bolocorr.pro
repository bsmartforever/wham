function bolocorr,bv=bv,sptype=sptype,plot=plot,noprint=noprint

; This function returns the bolometric
; correction given B-V or Spectral Type.
; ONLY for main-sequence stars for now.

if not keyword_set(bv) then begin
  sptype,bv=bv,sptype=sptype,/noprint
endif

; This is from "Cambridge University Press Handbook of Space
; Astronomy and Astrophysics" Second Edition by Martin V. Zombeck
; (http://ads.harvard.edu/books/hsaa/idx.html), pg.68-70
; M_bol = M_vis + BC

; From Popper, ARAA, 18, 115, 1980 
; K5   B-V = 1.15   BC = -0.66
; K7   B-V = 1.30   BC = -0.93

bvarr = [-0.30,-0.28,-0.26,-0.24,-0.20,-0.16,-0.14,-0.12,-0.09,$
         -0.06,0.00,0.06,0.14,0.19,0.31,0.36,0.43,0.54,0.59,0.63,$
         0.66,0.74,0.82,0.92,1.15,1.30,1.41,1.48,1.52,1.55,1.56,1.61,2.00]
bcarr = [-3.17,-2.80,-2.50,-2.23,-1.77,-1.39,-1.21,-1.04,-0.85,$
         -0.66,-0.40,-0.25,-0.15,-0.12,-0.08,-0.06,-0.04,-0.05,$
         -0.06,-0.07,-0.10,-0.15,-0.19,-0.25,-0.66,-0.93,-1.20,-1.48,$
         -1.76,-2.03,-2.31,-2.62,-4.20]

;Setting limits/range
if bv lt -0.30 or bv gt 2.00 then begin
  print,'B-V must be in the range -0.30 - +2.00'
  return,1
endif

;Two different splines
bv1 = bvarr(0:29)
bc1 = bcarr(0:29)
bv2 = bvarr(30:32)
bc2 = bcarr(30:32)

nbv1 = dindgen(100)/99.*(max(bv1)-min(bv1))+min(bv1)
nbc1 = fspline(bv1,bc1,nbv1)
nbv2 = dindgen(100)/99.*(max(bv2)-min(bv2))+min(bv2)
nbc2 = fspline(bv2,bc2,nbv2)
nbv = [nbv1,nbv2]
nbc = [nbc1,nbc2]
lo = first_el(where(nbv le bv),/last)
x = nbv(lo:lo+1)
y = nbc(lo:lo+1)
slope = (y(1)-y(0))/(x(1)-x(0))
yint = y(1)-slope*x(1)
bc = slope*bv + yint
;f = fit(bcarr([lo,lo+1]),1,bvarr([lo,lo+1]),coef,sigma)
;bc = poly(bv,coef)

;stop

;Printing Results
if not keyword_set(noprint) then begin
  print,'B-V = ',strtrim(bv,2)
  print,'BC  = ',strtrim(bc,2)
endif

;Plotting
if keyword_set(plot) then begin
  psym8
  plot,bvarr,bcarr,ps=8,xtit='B-V',ytit='BC',$
       tit='Bolometric Correction vs. B-V'
  oplot,nbv,nbc
  oplot,[bv],[bc],ps=4
  oplot,[bv,bv],[-10,10],linestyle=2
  oplot,[-10.,10.],[bc,bc],linestyle=2
endif

;stop

return,bc

end