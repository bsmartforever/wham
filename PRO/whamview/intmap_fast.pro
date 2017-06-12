function intmap_fast, map, vmin = vmin, vmax = vmax, $
                      badpoints = badpoints, orig = orig
  
  ii = fltarr(n_elements(map))

  IF N_ELEMENTS(vmin) EQ 0 THEN vmin = -100
  IF N_ELEMENTS(vmax) EQ 0 THEN vmax = +100
  
  PRINT, "Started intspect at: ", SYSTIME()
  for i = 0L, n_elements(ii)-1 do $
      ii[i] = intspect(map[i], vmin, vmax)
  PRINT, "Started FixBaddies at: ", SYSTIME()
  IF n_elements(badpoints) NE 0 THEN BEGIN 
    orig = ii
    fixbaddies_fast, map, ii, badpoints
  ENDIF 
  PRINT, "Ended FixBaddies at: ", SYSTIME()
  return, ii
end
