FUNCTION color, incolor

  outcolor = 0L
  
  IF !d.n_colors GT 256 THEN BEGIN
      outcolor = (1 + 256L + 256L * 256L) * incolor
  ENDIF ELSE BEGIN
      outcolor = incolor
  ENDELSE

  return, outcolor

END 
