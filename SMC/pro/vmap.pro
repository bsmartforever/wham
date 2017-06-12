function vmap, map, vmin = vmin, vmax = vmax, aperature = aperature, $
               peak = peak, iweight = iweight, $
               badpoints = badpoints, orig = orig
  
  IF n_elements(vmin) EQ 0 THEN vmin = -100
  IF n_elements(vmax) EQ 0 THEN vmax = +100

  IF NOT keyword_set(peak) AND NOT keyword_set(iweight) THEN BEGIN
    message, 'Please set PEAK or IWEIGHT to specify centroid method', /info
    return, -1
  ENDIF 
  
  vmap = fltarr(n_elements(map))
  
  IF keyword_set(peak) THEN BEGIN 
    for i = 0L, n_elements(vmap)-1 do begin
      IF keyword_set(aperature) THEN $
        vpmin = max(map[i].vel) - 203 $
      ELSE $
        vpmin = min(map[i].vel)

      velslice = where(vmin LE map[i].vel AND map[i].vel LE vmax AND $
                       map[i].vel GE vpmin, vcount)
      vv = map[i].vel[velslice]
      dd = map[i].data[velslice]

      dds = smooth(dd, 3)
      maxpt = where(dds EQ max(dds))

      vmap[i] = vv[maxpt]
    ENDFOR
  ENDIF ELSE IF keyword_set(iweight) THEN BEGIN 
    FOR i = 0L, n_elements(vmap)-1 DO BEGIN
      IF keyword_set(aperature) THEN $
        vpmin = max(map[i].vel) - 203 $
      ELSE $
        vpmin = min(map[i].vel)

      velslice = where(vmin LE map[i].vel AND map[i].vel LE vmax AND $
                       map[i].vel GE vpmin, vcount)
      intpt = int_tabulated(map[i].vel[velslice], map[i].data[velslice])
      vintpt = int_tabulated(map[i].vel[velslice], $
                              map[i].vel[velslice]*map[i].data[velslice])
      vmap[i] = vintpt/intpt
    ENDFOR 
  ENDIF 

  IF n_elements(badpoints) NE 0 THEN BEGIN
    orig = vmap
    fixbaddies, map, vmap, badpoints
  ENDIF 

  return, vmap
END 
