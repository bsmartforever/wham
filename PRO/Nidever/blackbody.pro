pro blackbody,T,w,I,minw=minw,maxw=maxw,npts=npts,plot=plot,$
    l=l,cgs=cgs,noprint=noprint,wmax=wmax

; This function returns the blackbody radiation (Planck function)
; for any given temperature

; This is differential brightness ( energy/(time area wavelength solid angle) )
; ( W/(m^2 Ang str) )
; Unless you set the /cgs keyword and then it will be in erg/s/cm^2/Ang/str

; B_lambda(T) = 2*h*c^2/lambda^5 * 1/(exp(hc/lambda*k*T) - 1)
; B_lambda(T) = (b1/lambda^5)/(exp(b2/lambda*T)-1)

if not keyword_set(T) then T = 5800
if not keyword_set(noprint) then print,' Temperature = ',strtrim(T,2),' K'

h = 6.626076d-34	; planck's constant (J s)
c = 2.99792458d8	; speed of light (m/s)
k = 1.38066d-23		; boltzmann constant (J/K)

b1 = 2.*h*(c^2.) * 1d40     ; Watts/m^2/Ang/sr
b2 = h*c/k * 1d10           ; Ang/K

;b1 = 1.1904397d24	; B_lambda = W/(m^2 * rad^2 * ang)
;b2 = 1.438769d8   	; B_lambda = W/(m^2 * rad^2 * ang)

; using cgs units
if keyword_set(cgs) then begin
  h = 6.626076d-27         ; planck's constant (erg s)
  c = 2.99792458d10        ; speed of light (cm/s)
  k = 1.38066d-16          ; boltzmann constant (erg/K)

  b1 = 2.*h*(c^2.) * 1d32     ; ergs/s/cm^2/Ang/sr
  b2 = h*c/k * 1d8            ; Ang/K
endif

; Calculating wavelength of maximum from Wien's Law.  (in Ang)
wmax = 2.897d7/T
;wmax = 2.8978d7/T*1d-10
;if keyword_set(cgs) then wmax=wmax*100.

; Wavelength range in Ang
if not keyword_set(npts) then npts = 1000.
if not keyword_set(minw) then minw = wmax*0.1
if not keyword_set(maxw) then maxw = wmax*50.
;if not keyword_set(minw) then minw = 1.
;if not keyword_set(maxw) then maxw = 1.d5
;if not keyword_set(minw) then minw = 1.d-10 
;if not keyword_set(maxw) then maxw = 1.d-5

; Creating wavelength array, in Ang
w = dindgen(npts)/(npts-1.d)*(maxw-minw)+minw
dw = w(1)-w(0)
;w = dindgen(npts)*1000.*wmax+wmax/500.

; Calculating the blackbody function
I = (b1/w^5)/(exp(b2/(w*T)) - 1.d)
;I = (2.*h*c^2)/(w^5)*1./(exp((h*c)/(w*k*T)) - 1.d)

; Calculating Luminosity
;fmax = c/wmax
;f = c/w
;f2 = dindgen(npts)/(npts-1.d)*(1d16-1d12) + 1d12
;I2 = (2.*h*f2^3)/(c^2)*1./(exp((h*f2)/(k*T)) - 1.d)
;df = f2(1)-f2(0)
l = total(I)*dw

;Plotting
if keyword_set(plot) then begin
  ytit = 'Radiation (W/m^2 rad^2 Ang)'
  if keyword_set(cgs) then ytit = 'Radiation (erg/sec cm^2 Ang sr)' 
  plot,w,I,tit='Blackbody Radiation at T='+strtrim(T,2)+' K',$
       xtit='Wavelength (Ang)',ytit=ytit
endif

;print,l*4*(!dpi^2)*(6.96d10)^2

;stop

end
