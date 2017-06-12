FUNCTION reduceto180, a
; Reduce an angle to the range of +- 180.
  
  b = a
  r = (b+180.)/360.
  t = where(r LT 0.0, count)
  IF count GT 0 THEN b(t) = b(t) + ceil(-r(t))*360
  t = where(r GT 1.0, count)
  IF count GT 0 THEN b(t) = b(t) - floor(r(t))*360
  return, b
END 

