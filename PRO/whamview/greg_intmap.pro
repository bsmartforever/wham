function intmap, map, vmin = vmin, vmax = vmax, $
                 badpoints = badpoints, orig = orig, hi = hi
  
  ii = fltarr(n_elements(map))
  
  IF n_elements(vmin) eq 0 THEN vmin = -100
  IF n_elements(vmax) eq 0 THEN vmax = +100
  
  IF KEYWORD_SET(hi) THEN BEGIN
     IF vmin EQ vmax THEN BEGIN
        vmin = MIN(Map.Vel) &  vmax = MAX(Map.Vel)
     ENDIF
  ENDIF ELSE IF vmin EQ vmax THEN BEGIN
     vmin = -100
     vmax = 100
  ENDIF
  
  for i = 0L, n_elements(ii)-1 do $
    ii[i] = intspect(map[i], vmin, vmax, hi = hi)

  IF n_elements(badpoints) NE 0 THEN BEGIN
    orig = ii
    fixbaddies, map, ii, badpoints
  ENDIF 
  
  return, ii
end
