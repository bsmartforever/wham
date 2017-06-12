PRO gridspacing, cimin, cimax, gmin, gmax, del, adjust = adjust

  IF n_elements(adjust) EQ 0 THEN adjust = 0

  cmin = cimin LT cimax ? cimin : cimin-360
  cmax = cimax

  span = cmax-cmin
  
  grid = [0, 1, 2, 5, 10, 20, 30, 60]
  delw = where(span/5.0 GT grid)
  deli = (delw[n_elements(delw)-1] + 1 + adjust) < (n_elements(grid)-1)
  if (NOT keyword_set(del)) then del = grid[deli]

  mod_gmin=cmin mod del
  mod_gmax=cmin mod del

  gmin = cmin-mod_gmin
  gmax = cmax-mod_gmax

  IF gmin LT cmin THEN gmin = gmin+del
  IF gmax GT cmax THEN gmax = gmax-del

END       
  
