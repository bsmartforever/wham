pro absmag,bv=bv,m=m,reverse=reverse,noprint=noprint,$
           plot=plot

; This program returns absolute (visual) magnitude
; for a main-sequence star given the B-V.  This is
; from empirical fit Hipparcos data ( see program
; specplx_get.pro).  This program can also be run in
; reverse and return the B-V given the absolute (visual)
; magnitude

; I also have an extension from Henry's paper
; APJ, 512, 864, 1999; which extends it beyond
; 13.0 to 18.5.  Also see specplx_get2.pro where
; I found the inverse of his function.

; The Hipparcos main-sequence was fitted in my
; specplx_get.pro program.
;
; These are the nine coefficients of the best-fitting curve
;     1.0100991       6.8939422      -22.123639       85.979889      -159.37543
;       180.19800      -131.47445       55.257688      -9.7480315
;
;  fit3 = fit(m(ggg),8,bv(ggg),coef,sigma)
;The nine coefficients are =        1.0100991       6.8939422      -22.123639    85.979889
;      -159.37543       180.19800      -131.47445       55.257688      -9.7480315
;RMS Scatter about Fit =       0.37219982 mag
;
; good for -0.157 < B-V < 1.64
; good for -1.145 < M < 12.0 mag
; good for 614 < L < 0.011 Lsun


;These are from Carroll & Ostile Appendix E.  These should allow
;me to extend it to brighter stars. -0.33 < B-V < -0.15
; and -5.6 < Mv < 0.9
bvarr = [-0.33,-0.32,-0.31,-0.30,-0.24,-0.20,-0.17,-0.15]
mvarr = [-5.6,-5.05,-4.5,-4.0,-2.4,-1.6,-1.2,-0.9]
fit = fit(mvarr,2,bvarr,co_coef,sigma)
fit2 = fit(bvarr,2,mvarr,co_coef2,sigma2)


if not keyword_set(bv) and not keyword_set(m) then begin
  print,'B-V or Mv Must Be Input'
  return
endif
if not keyword_set(bv) then reverse=1

If not keyword_set(reverse) then begin

  ;Setting the limits/range
  if (bv lt -0.33 or bv gt 2.24) then begin
    print,'B-V Must Be In The Range Of -0.33 to 2.24'
    return
  endif

  ;Using the Carroll & Ostlie data for -0.33 < B-V < -0.157
  If (bv ge -0.33 and bv lt -0.157) then begin
    M = poly(bv,co_coef)
  Endif


  ;Using my values for -0.157 < B-V < 1.64; -1.145 < Mv < 12.0
  If (bv gt -0.157 and bv le 1.64) then begin
    coef = [ 1.0100991  ,   6.8939422  ,  -22.123639  ,  85.979889  ,  -159.37543,$
           180.19800  ,   -131.47445   ,   55.257688  ,   -9.7480315 ]
    coef = double(coef)

    M = poly(bv,coef)
  endif

  ;Using my fitted values of Henry's eqn. for 1.64 < B-V < 2.24
  if (bv gt 1.64 and bv lt 2.24) then begin

    ;From the Henry paper APJ, 512, 864, 1999  and my specplx_get2.pro program
    ;coef = [ -21.585380  ,  185.24090 ,   -558.02771 ,    902.42623 ,   -830.60231,$
    ;    436.08571  ,   -120.78003  ,  13.635664 ]
    ; std.dev. of resid = 0.0326
    coef = [ -21.585380  ,  185.24090 ,   -558.02771 ,    902.42623 ,   -830.60231,$
        436.08571  ,   -120.78003  ,  13.635664 ]
    M = poly(bv,coef)
  endif

Endif

; The REVERSE: Getting B-V from absolute (visual) magnitude

;The nine coefficients are =     -0.082659172     0.043613713     0.060769419     0.020296294
;    -0.019587141    0.0049763719  -0.00058007588   3.2214721e-05  -6.9064595e-07
;RMS Scatter about Fit =      0.058322646 dex
; good for -1.145 < M < 12.0 mag

if keyword_set(reverse) then begin

  ;Setting the limits/range
  if (m lt -5.6 or m gt 18.5) then begin
    print,'Mv Must Be In The Range Of -5.6 to 18.5'
    return
  endif

  ;Using the Carroll & Ostlie data for -5.6 < Mv < -1.145
  If (m ge -5.6 and m lt -0.145) then begin
    bv = poly(m,co_coef2)
  Endif

  ;Using my fitted Hipparcos values
  If (m ge -1.145 and m le 12.0) then begin
    coef =  [   -0.082659172  ,   0.043613713  ,   0.060769419  ,   0.020296294,$
      -0.019587141 ,   0.0049763719 , -0.00058007588 ,  3.2214721e-05 , -6.9064595e-07 ]
    coef = double(coef)

    bv = poly(m,coef)
  endif

  ;Using Henry's equation for 13.0 < Mv < 18.5
  if (m gt 12.0 and m le 18.5) then begin

    ;From the Henry paper APJ, 512, 864, 1999 we also have
    ; B-V = +0.000992 Mv^3 - 0.038184 Mv^2 + 0.555204 Mv - 1.242359
    bv = (+0.000992d)*M^3. - (0.038184d)*M^2. + (0.555204d)*M - 1.242359d

  endif

endif

;Printing Results
if not keyword_set(noprint) then begin
  print,'B-V = ',strtrim(bv,2)
  print,'Mv  = ',strtrim(m,2)
endif

;Plotting
if keyword_set(plot) then begin
  bv3 = dindgen(1000)/999.*(0.33-0.157) - 0.33
  mv3 = poly(bv3,co_coef)

  restore,'specplx_get.dat'
  g = where(bvarr le 1.651)
  fit3 = poly(bvarr(g),coef3)

  bv2 =  dindgen(1000)/999.*(2.24-1.64)+1.64
  coef = [ -21.585380  ,  185.24090 ,   -558.02771 ,    902.42623 ,   -830.60231,$
           436.08571  ,   -120.78003  ,  13.635664 ]
  m2 = poly(bv2,coef)

  ;yn = ['-4','-2','0','2','4','6','8','10','12','14']
  ;yv = [4.,2.,0.,-2.,-4.,-6.,-8.,-10.,-12.,-14.]
  yn = ['-10','-5','0','5','10','15','20']
  yv = [10.,5.,0.,-5.,-10.,-15.,-20.]
  plot,bvarr,-marr,ps=3,tit='Absolute Visual Magnitude vs. B-V',$
       xtit='B-V',ytit='M',yr=[-20,6],xr=[-0.2,2.3],$
       ytickn=yn,ytickv=yv,yminor=5,yticks=6
  oplot,bvarr(g),-fit3,thick=5
  oplot,bv2,-m2,thick=5
  oplot,bv3,-mv3,thick=5
  oplot,[bv],[-m],ps=4
  oplot,[-20,20],[-m,-m],linestyle=2
  oplot,[bv,bv],[-20,20],linestyle=2
  xyouts,1.2,5.,'B-V = '+strmid(strtrim(bv,2),0,7),charsize=1
  xyouts,1.2,4.,'Mv = '+strmid(strtrim(m,2),0,7),charsize=1
endif

;stop

end


