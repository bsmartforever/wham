function planckf,T,f,jansky=jansky

;  This program calculates the Planck function B_f(T)
;  For a given temperature and frequency.

; Planck function:
; B_f(T) = (2*h*f^3/c^2)/(e^((h*f)/(k*T))-1)

h = 6.626076d-27         ; planck's constant (erg s)
c = 2.99792458d10        ; speed of light (cm/s)
k = 1.38066d-16          ; boltzmann constant (erg/K)

b1 = 2.*h/(c^2.)         ;
b2 = h/k                 ;

Bf = (b1*f^3.)/(exp(b2*f/T)-1.d)

if keyword_set(jansky) then Bf=Bf*1d23
; 1 Jansky = 1e-23 erg/s/cm^2/Hz

;stop

return,Bf

end
