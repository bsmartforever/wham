pro rgbiso,jks,feh,mks,h96=h96,cg97=cg97,f99=f99,slope=slope,zp=zp

;  This program plots the 2MASS RGB isochrones, or the "Fan"-like diagram
;  From Ivanov & Borissova (2002)
;
; INPUT
;  feh   The metallicity, [Fe/H]
;  jks   The intrinsic (J-Ks)o color
;
; OUTPUT
;  mks   The absolute Ks magnitude
;

;feh = -1.0
;jks = 0.8
;jks = scale_vector(findgen(100),0.4,1.2)

if not keyword_set(cg97) and not keyword_set(f99) then h96=1

; H96
if keyword_set(h96) then begin
  a0s = -0.157  ;+/-0.009
  a1s = -0.051  ;+/-0.006
  a0z =  0.277  ;+/-0.152
  a1z = -0.070  ;+/-0.104
endif

; CG97
if keyword_set(cg97) then begin
  a0s = -0.158  ;+/-0.010
  a1s = -0.058  ;+/-0.007
  a0z =  0.272  ;+/-0.155
  a1z = -0.082  ;+/-0.120
endif

; F99
if keyword_set(f99) then begin
  a0s = -0.149  ;+/-0.009
  a1s = -0.060  ;+/-0.008
  a0z =  0.285  ;+/-0.137
  a1z = -0.084  ;+/-0.122
endif

slop = a0s + a1s*feh
zp = a0z + a1z*feh
;mks = zp + slop*jks
mks = (jks-zp)/slop    ; They have the equation reversed in the paper!!!

; The real slope and zero-point
slope = 1./slop
zp = -zp/slop

;plot,jks,mks,tit='Fan Diagram',xtit='(J-K!dS!n)!d0!n',ytit='M!dKs!n',$
;     xr=[0.4,1.3],yr=[-0.5,-8.5],xs=1,ys=1,charsize=1.3

;stop

end
