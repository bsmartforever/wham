function intmap_fast, map, vmin = vmin, vmax = vmax, $
                      badpoints = badpoints, orig = orig
  
  ii = fltarr(n_elements(map))

  IF n_elements(vmin) eq 0 THEN vmin = -100
  IF n_elements(vmax) eq 0 THEN vmax = +100

  PRINT, "Started intspect at: ", SYSTIME()
  for i = 0L, n_elements(ii)-1 do $
      ii[i] = intspect(map[i], vmin, vmax)
  IF n_elements(badpoints) NE 0 THEN BEGIN 
  PRINT, "Started FixBaddies at: ", SYSTIME()
    orig = ii
    fixbaddies_fast, map, ii, badpoints
  PRINT, "Ended FixBaddies at: ", SYSTIME()
  ENDIF 
  return, ii
end
