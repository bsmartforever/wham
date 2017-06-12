pro reddening,R_v,w,A_w,plot=plot,x=x

;  This will compute the reddening for a given
;  wavelength (can be array).  
;  Wavelength must be given in Angstroms. 
;
;  If the wavelength is out of the range
;  1000 < w < 33,333 Ang then it will 
;  return zero.

if keyword_set(w) then x = (1d4)/w

; x is in 1/micron
; A_lambda/A_v = a(x) + b(x)/R_v

if not keyword_set(w) then begin
  n = 1d4
  xmin = 0.3
  xmax = 10.
  x = dindgen(n)/(n-1)*(xmax-xmin)+xmin
  w = (1d4)/x
endif

indIR = where(x ge 0.3 and x le 1.1,nindIR)
indOPT = where(x ge 1.1 and x le 3.3,nindOPT)
indUV = where(x ge 3.3 and x le 8.0,nindUV)
indFUV = where(x ge 8.0 and x le 10.0,nindFUV)
indFa = where(x gt 5.9 and x le 8.0,nindFa)

a = x*0.
b = x*0.
Fa = x*0.
Fb = x*0.

; Infrared, 0.3 micron-1 <= x <= 1.1 micron-1
if nindIR gt 0 then begin
  a(indIR) = 0.574*x(indIR)^1.61
  b(indIR) = -0.527*x(indIR)^1.61
endif

; Optical/NIR, 1.1 micron-1 <= x <= 3.3 micron-1 and y=(x-1.82)
if nindOPT gt 0 then begin
  y = x-1.82
  a(indOPT) = 1.d + 0.17699*y(indOPT) - 0.50447*y(indOPT)^2. - 0.02427*y(indOPT)^3. $
      + 0.72085*y(indOPT)^4. + 0.01979*y(indOPT)^5. - 0.77530*y(indOPT)^6. $
      + 0.32999*y(indOPT)^7.
  b(indOPT) = 1.41338*y(indOPT) + 2.28305*y(indOPT)^2. + 1.07233*y(indOPT)^3. $
      - 5.38434*y(indOPT)^4. - 0.62251*y(indOPT)^5. + 5.30260*y(indOPT)^6. $
      - 2.09002*y(indOPT)^7.
endif

; Ultraviolet: 3.3 micron-1 <= x <= 8 micron-1
if nindUV gt 0 then begin
  if nindFa gt 0 then begin
    Fa(indFa) = -0.04473*(x(indFa)-5.9)^2. - 0.009779*(x(indFa)-5.9)^3.    ; 8 >= x >= 5.9
    Fb(indFa) = 0.2130*(x(indFa)-5.9)^2. + 0.1207*(x(indFa)-5.9)^3.        ; 8 >= x >= 5.9
  endif
  ;Fa = 0                ; x<5.9
  ;Fb = 0                ; x<5.9

  a(indUV) = 1.752 - 0.316*x(indUV) - 0.104/( (x(indUV)-4.67)^2. + 0.341 ) + Fa(indUV)
  b(indUV) = -3.090 + 1.825*x(indUV) + 1.206/( (x(indUV)-4.62)^2. + 0.263 ) + Fb(indUV)

endif

; Far-UV: 8 micron-1 <= x <= 10 micron-1
if nindFUV gt 0 then begin
  a(indFUV) = -1.073 - 0.628*(x(indFUV)-8.) + 0.137*(x(indFUV)-8.)^2. - 0.070*(x(indFUV)-8.)^3.
  b(indFUV) = 13.670 + 4.257*(x(indFUV)-8.) - 0.420*(x(indFUV)-8.)^2. + 0.374*(x(indFUV)-8.)^3.
endif

A_w = a+b/R_v

; Plotting
if keyword_set(plot) then begin
  tit='!6Extinction & Reddening'
  xtit='!61/!4k!6 (!4l!6m!u-1!n)'
  ytit='!6A!d!4k!n!6/A!dV!n'
  plot,x,a_w,xtit=xtit,ytit=ytit,tit=tit,charsize=1.2

endif

;stop

end
