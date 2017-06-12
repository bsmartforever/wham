PRO hdtoaa, hain, decin, olatin, az, alt, degrees = degrees

  IF keyword_set(degrees) THEN BEGIN
      ha = double(hain*!dtor)
      dec = double(decin*!dtor)
      olat = double(olatin*!dtor)
  ENDIF ELSE BEGIN
      ha = double(hain)
      dec = double(decin)
      olat = double(olatin)
  ENDELSE

  c1 = cos(dec)*sin(ha)
  c2 = cos(dec)*cos(ha)*sin(olat) - sin(dec)*cos(olat)
  c3 = sin(dec)*sin(olat) + cos(dec)*cos(ha)*cos(olat)

  alt = asin(c3)
  saz = asin(c1/cos(alt))
  caz = acos(c2/cos(alt))

  IF saz LT 0 THEN $
    az = 2*!pi - caz $
  ELSE $
    az = caz
  
END 
