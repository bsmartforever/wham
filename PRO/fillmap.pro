PRO fillmap, lon, lat, image, missing, decrange = decrange, pointmin=pointmin, $
	pointmax=pointmax
  
  IF n_elements(decrange) EQ 0 THEN decrange = [-90, 90]
  window = 0.1

  t = systime(1)
  restore, '/d/wham/pro/data/points.dat'

  IF keyword_set(pointmin) THEN $
  	points = points[*, where(points[2, *] GE pointmin)]  
  IF keyword_set(pointmax) THEN $
  	points = points[*, where(points[2, *] LE pointmax)]  
  
  plon = points[0, *]
  plat = points[1, *]

  euler, plon, plat, pra, pdec, 2

  indr = where(decrange[0] LE pdec AND pdec LE decrange[1])
  plon = plon[indr]
  plat = plat[indr]

  inmap = where(min(lon) - (window/cos(plat*!dtor)) LE plon $
                AND plon LE max(lon) + (window/cos(plat*!dtor)) $
                AND min(lat) - window LE plat $
                AND plat LE max(lat) + window)
  plon = plon[inmap]
  plat = plat[inmap]

  npoints = n_elements(plon)
  blank = bytarr(npoints)+1

  tot = 0

  print, 'Fill-checking ', strtrim(npoints, 2), ' points'
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
;      IF wcnt GT 1 THEN $
;        message, 'Non-unique fill point at '+strtrim(plon[i], 2)+$
;        ', '+strtrim(plat[i], 2), /info
    ENDIF 

;   ELSE BEGIN
;     message, string('No pointing: ', plon[i], plat[i]), /info
;   ENDELSE 

    IF i MOD 1000 EQ 0 THEN print, 'Checking: ', i
  ENDFOR

  ;; special case for a missing block near l=238.93, b=-5.94 
  w1 = where(-5.95 LE plat AND plat LE -5.91, wcnt)
  IF wcnt NE 0 THEN BEGIN 
    w2 = where(238.7 LE plon[w1] AND plon[w1] LE 239.1, wcnt)
    IF wcnt NE 0 THEN BEGIN 
      print, 'Found it at ', w1[w2], plon[w1[w2]], plat[w1[w2]], blank[w1[w2]]
      blank[w1[w2]] = 0
    ENDIF 
  ENDIF

  add = where(blank EQ 1)
  
  lon = [lon, plon[add]]
  lat = [lat, plat[add]]
  image = [reform(image), blank[add]*0 + missing]

  print, npoints-tot, ' points filled'
  print, 'Filling: ', systime(1)-t, ' seconds'
END 
