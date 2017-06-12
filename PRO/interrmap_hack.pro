FUNCTION interrmap, map, vmin = vmin, vmax = vmax

  ii = fltarr(n_elements(map))

  IF NOT keyword_set(vmin) THEN vmin = -100
  IF NOT keyword_set(vmax) THEN vmax = +100

  dv = map[0].vel[1]-map[0].vel[0]
  for i = 0L, n_elements(ii)-1 do begin
    pointing = map[i]
    vpmin = max(pointing.vel) - 203

    velslice = where((vmin le pointing.vel and pointing.vel le vmax) AND $
        (pointing.vel GE vpmin) AND NOT $
        (pointing.vel EQ 0 AND pointing.data EQ 0), vcount)

    ii[i] = vcount LE 1 ? 0 : $
        int_tabulated_var(pointing.vel[velslice], pointing.var[velslice])
  endfor
    
  return, sqrt(ii)
end
