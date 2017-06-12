function ha2lc, ha, T=T, flux=flux, mR=mR,log=log 

;+
; Purpose - To determine the incident Lyman Continuum radiation 
;    needed to produce an H-alpha intensity. Will determine the 
;    inverse if flux is passed.
;
;    Assumptions:  
;       1) The radiated cloud is optically thick, such that the HI 
;          column density exceeds 10^18 cm^-2.
;       2) The rate of recombination is directly proportional to 
;          the rate of ionization. This means that photoionizaiton is 
;          the dominant source of the ionization AND that the gas is 
;          in local thermostatic equalibrium.  
;          Uses case B recombination coeficient to calculate:
;          aB(T)=2.584e-13*(T/10.^4)^(-0.806) cm^3 s^-1 (Martin 1988)
;       3) The election temperature of the cloud is uniform and constant 
;          throughout. 
;
; ha   - H-alpha intensity in Rayleighs
; T    - Electron temperature in Kelvin
;        Values less than 10 are assumed to be logrithmic.  
;        Default: T=10e4
; flux - Incident ionizing radiation in photons/(cm^2 s)
;        If passed, calculate the H-alpha produced by a cloud 
;        radiated by flux. 
;        Values less than 10 are assumed to be logrithmic. 
; mR   - Sets the passed H-alpha intensity to mR or returns the 
;        H-alpha intensity in mR if a flux is passed.
;       
; Created by Dr. Kat Barger
;-	

num=max([n_elements(ha),n_elements(T),n_elements(flux)])

if keyword_set(ha) then ha=fltarr(num)+ha $
else if keyword_set(flux) then flux=fltarr(num)+flux

if (NOT keyword_set(T)) then T=fltarr(num)+10.^4. 
if T[0] lt 10. then T = fltarr(num)+10.^T
if keyword_set(mR) AND (NOT keyword_set(flux)) then ha=ha*1.0e-3

if keyword_set(flux) then begin
   if flux[0] lt 10. then flux = fltarr(num)+10.^flux
   if keyword_set(mR) then $
      return,(flux/2.1e5)*(.1)*(T/10.^4)^(-0.118)*1.e3
   return,(flux/2.1e5)*(.1)*(T/10.^4)^(-0.118)
endif

if keyword_set(log) then return,alog10(2.1e5*(ha/0.1)*(T/10.0^4.0)^0.118) else $
return,2.1e5*(ha/0.1)*(T/10.0^4.0)^0.118;

end