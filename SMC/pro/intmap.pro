function intmap, map, vmin = vmin, vmax = vmax, hi = hi, apclip = apclip, $
                 badpoints = badpoints, orig = orig, avg = avg, var = var, moment = moment
  
  ii = fltarr(n_elements(map))

  IF n_elements(vmin) eq 0 THEN vmin = -100
  IF n_elements(vmax) eq 0 THEN vmax = +100

  IF KEYWORD_SET(avg) THEN BEGIN 
    for i = 0L, n_elements(ii)-1 do $
      ii[i] = avgintspect(map[i], vmin, vmax, hi = hi, apclip = apclip)
  ENDIF ELSE BEGIN 
    for i = 0L, n_elements(ii)-1 do $
      ii[i] = intspect(map[i], vmin, vmax, hi = hi, apclip = apclip, moment = moment, var = var)
  ENDELSE 

  IF n_elements(badpoints) NE 0 THEN BEGIN
    orig = ii
    fixbaddies, map, ii, badpoints
  ENDIF 
  
  return, ii
end
