PRO fillmap, lon, lat, image, missing, decrange = decrange
  
  IF n_elements(decrange) EQ 0 THEN decrange = [-40, -20]

  t = systime(1)
  restore, '/d/wham/survey/idl/points.dat'

  plon = points[0, *]
  plat = points[1, *]

  euler, plon, plat, pra, pdec, 2

  indr = where(decrange[0] LE pdec AND pdec LE decrange[1])
  plon = plon[indr]
  plat = plat[indr]

  inmap = where(min(lon) LE plon AND plon LE max(lon) AND $
              min(lat) LE plat AND plat LE max(lat))
  plon = plon[inmap]
  plat = plat[inmap]

  npoints = n_elements(plon)
  blank = bytarr(npoints)+1

  window = 0.1
  tot = 0

  print, 'Filling ', strtrim(npoints, 2), ' points'
  FOR i = 0L, npoints-1 DO BEGIN 

    w1 = where(plat[i] - window LE lat AND lat LE plat[i] + window, wcnt)
    IF wcnt NE 0 THEN BEGIN 
      lonwindow = window/cos(plat[i] * !dtor)
      w2 = where(plon[i] - lonwindow LE lon[w1] AND $
                 lon[w1] LE plon[i] + lonwindow, wcnt)
    ENDIF 
    
    IF wcnt NE 0 THEN BEGIN 
      tot = tot+1
      blank[i] = 0
      IF wcnt GT 1 THEN $
        message, 'Non-unique fill point at '+strtrim(plon[i], 2)+$
        ', '+strtrim(plat[i], 2), /info
    ENDIF 

;   ELSE BEGIN
;     message, string('No pointing: ', plon[i], plat[i]), /info
;   ENDELSE 

    IF i MOD 1000 EQ 0 THEN print, 'Filling: ', i
  ENDFOR

  add = where(blank EQ 1)

  lon = [lon, plon[add]]
  lat = [lat, plat[add]]
  image = [reform(image), blank[add]*0 + missing]

  print, npoints-tot, ' points filled'
  print, 'Filling: ', systime(1)-t, ' seconds'
END 
