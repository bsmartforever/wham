pro	l2nu,w,fl,nu,fnu

;
;	Converting flambda (erg cm-2 s-1 A-1) to fnu  (erg cm-2 s-1 Hz-1)
;	and w (angstroms) to nu (Hz)
;

if N_params() LT 1 then begin
      print,'%l2nu: Syntax - nu2l,w,fl,nu,fnu'
      return
endif

c=299792458d2 	; cm
x=w*1d-8 	; cm
nu=c/x
fnu=x^2/c*fl*1d8

end