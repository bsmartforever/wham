pro stat,array,info,noprint=noprint

if n_elements(array) eq 0 then begin
  print,'syntax - stat,array,info'
  return
end

;  This is what stat returns:
;  info(0): Number of Elements
;  info(1): Minimum
;  info(2): Maximum
;  info(3): Range
;  info(4): Mean
;  info(5): Median
;  info(6): Standard Deviation
;  info(7): Standard Error
;  info(8): Root Mean Square (R.M.S.)
;  info(9): MAD estimate of St.Dev.

info=dblarr(10)
info(0)=n_elements(array)
info(1)=min(array)
info(2)=max(array)
info(3)=max(array)-min(array)
info(4)=total(array)/info(0)
if info(0) gt 1 then begin
  info(5)=median(array)
  info(6)=SQRT(total((array-info(4))^2/(info(0)-1)))
  info(7)=info(6)/SQRT(n_elements(array))
  info(8)=SQRT( total(array^2.)/info(0) )  ; RMS
  info(9)=1.4826*median(abs(array-median(array)))
endif
if info(0) eq 1 then info(5)=info(4)

if not keyword_set(noprint) then begin
  print,'----------------------'
  print,'elements = ',strtrim(info(0),2)
  print,'minimum  = ',strtrim(info(1),2)
  print,'maximum  = ',strtrim(info(2),2)
  print,'range    = ',strtrim(info(3),2)
  print,'mean     = ',strtrim(info(4),2)
  print,'median   = ',strtrim(info(5),2)
  print,'st. dev. = ',strtrim(info(6),2)
  print,'st. err. = ',strtrim(info(7),2)
  print,'r.m.s.   = ',strtrim(info(8),2)
  print,'mad s.d. = ',strtrim(info(9),2)
  print,'----------------------'
endif

end
