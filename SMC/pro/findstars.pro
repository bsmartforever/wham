FUNCTION isclose, map, glon, glat, radius, ccnt

  w1 = where(glat - radius LE map.glat AND $
             map.glat LE glat + radius, ccnt)
  IF ccnt NE 0 THEN BEGIN 
    lonradius = radius/cos(glat * !dtor)
    w2 = where(glon - lonradius LE map[w1].glon AND $
              map[w1].glon LE glon + lonradius, ccnt)
  ENDIF

  IF ccnt EQ 0 THEN $
    return, -1 $
  ELSE $
    return, w1[w2]
  
END 


PRO findstars, map, stars, closemap, closestars, nclose, dist, $
                  window = window

  IF n_elements(window) EQ 0 THEN window = 0.5

  euler, map.glon, map.glat, ra, dec, 2

  mindec = min(dec)

  euler, stars.glon, stars.glat, ra, dec, 2
  amindec = where(dec GE mindec)
  closestars = stars[amindec]
  closemap = lonarr(n_elements(closestars), 3) + (-1)
  nclose = intarr(n_elements(closestars))

  print, 'Selected ', n_elements(closestars), ' stars above dec =', mindec

  FOR i = 0, n_elements(closestars)-1 DO BEGIN 

    c = spectnear(map, closestars[i].glon, closestars[i].glat, $
                  window, ccnt)

    IF ccnt NE 0 THEN BEGIN 

      ;; only record three closest beams... should be max for most survey spacings
      ;; could be increased for Nyquist samplings.
      closemap[i, 0:(ccnt-1 < 2)] = c[0:(ccnt-1 < 2)]
      nclose[i] = (ccnt < 3)
      IF ccnt GT 3 THEN begin
        message, 'HD', strtrim(closestars[i].hd, 2) + ' has ' + $
          strtrim(n_elements(c), 2) + ' matches'
      end

    ENDIF

    IF i MOD 1000 EQ 0 THEN print, i

  ENDFOR 

  noclose = where(closemap[*, 0] EQ -1)
  
  print, strtrim(n_elements(noclose), 2), ' stars did not match'

  ;; Pass back the index arrays of stars that matched a beam. This is a
  ;; bit sneaky--closestars is switch from the stellar array to an
  ;; index array
  good = where(nclose NE 0, gcount)
  IF gcount EQ  0 THEN BEGIN 
    closestars = -1
    closemap = -1
    dist = -1
    return
  ENDIF ELSE BEGIN 
    closestars = amindec[good]
    closemap = closemap[good, *]
    nclose = nclose[good]
  ENDELSE 
  ;; find out some things about these pointings
  dist = fltarr(n_elements(good), 3)
  FOR i = 0, n_elements(closestars)-1 DO BEGIN 
    FOR j = 0, nclose[i]-1 DO BEGIN 

       dist[i, j] = sphdist(stars[closestars[i]].glon, $
                           stars[closestars[i]].glat, $
                           map[closemap[i, j]].glon, $
                           map[closemap[i, j]].glat, /deg)
    ENDFOR
  ENDFOR 

END   
