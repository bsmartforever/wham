function compute_moments, map, vmin = vmin, vmax = vmax, badpoints = badpoints

  mm = fltarr(3, n_elements(map))

  IF n_elements(vmin) eq 0 THEN vmin = -100
  IF n_elements(vmax) eq 0 THEN vmax = +100

  for i = 0L, n_elements(map)-1 do begin
    p = map[i]
    velslice = where(vmin LE p.vel AND p.vel LE vmax, vcount)
    
    if vcount le 1 then begin
      mm[*, i] = replicate(!VALUES.F_NAN, 3)
      CONTINUE
    endif else begin
      v = p.vel[velslice]
      d = p.data[velslice]
    endelse
    
    m0 = int_tabulated(v, d)
    m1 = int_tabulated(v, d * v) / m0
    m2 = int_tabulated(v, d * (v - m1)^2.0) / m0

    mm[*, i] = [m0, m1, m2]
  endfor

  ;  IF n_elements(badpoints) NE 0 THEN BEGIN
  ;    orig = ii
  ;    fixbaddies, map, ii, badpoints
  ;  ENDIF

  return, mm
 end

  