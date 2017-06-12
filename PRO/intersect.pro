FUNCTION intersect, a, b, nmatch

;  t = systime(1)
  
  sza = n_elements(a)
  szb = n_elements(b)

  IF sza LT szb THEN BEGIN
      outer = a(sort(a))
      szo = sza
      inner = b(sort(b))
      szi = szb
      result = a*0
  ENDIF ELSE BEGIN
      outer = b(sort(b))
      szo = szb
      inner = a(sort(a))
      szi = sza
      result = b*0
  ENDELSE

  i = 0
  nmatch = 0
  WHILE (i LT szo) DO BEGIN
      wh = where(outer(i) EQ inner, count)
      IF count NE 0 THEN BEGIN
          result(nmatch) = outer(i)
          nmatch = nmatch + 1
      ENDIF 
      i = i + 1
  ENDWHILE

;  print, systime(1)-t
  
  IF nmatch EQ 0 THEN BEGIN
      return, -1
  ENDIF ELSE BEGIN 
      nresult = result(0:nmatch-1)
      return, nresult(uniq(nresult, sort(nresult)))
  ENDELSE 
END 
