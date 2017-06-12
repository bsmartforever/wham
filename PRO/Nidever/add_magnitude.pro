pro add_magnitude,m1,m2,m,average=average

; This adds magnitudes properly
; m1-m2 = 2.5*log(f1/f2)
; m1 = -2.5*log(f1)+C
; m2 = -2.5*log(f2)+C
; f1 = 10^((C-m1)/2.5)
; f2 = 10^((C-m2)/2.5)
; f = f1+f2 = 10^((C-m1)/2.5) + 10^((C-m2)/2.5)
; m = -2.5*log(f) + C = -2.5*log( 10^((C-m1)/2.5) + 10^((C-m2)/2.5) ) + C
;   = -2.5*log( 10^(-m1/2.5) + 10^(-m2/2.5) )
; For an average you want 0.5*(f1+f2), so that turns out to add -2.5*log(0.5) to m.

nm1 = n_elements(m1)
nm2 = n_elements(m2)

if nm1 eq 0 or nm2 eq 0 then begin
  print,'Syntax - add_magnitude,m1,m2,m
  return
endif

m = -2.5*alog10( 10.^(-m1/2.5) + 10.^(-m2/2.5) )
if keyword_set(average) then m = m -2.5*alog10(0.5)

;stop

end