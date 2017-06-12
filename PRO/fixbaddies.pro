PRO fixbaddies, map, ii, badpoints, radius = radius

  IF n_elements(radius) EQ 0 THEN radius = 1.0

  ubad = badpoints[uniq(badpoints, sort(badpoints))]
  ubad = ubad[where(ubad NE -1)]
  
  FOR i = 0, n_elements(ubad)-1 DO BEGIN 
    ;; find nearby points

    near = spectnear(map, map[ubad[i]].glon, map[ubad[i]].glat, $
                     radius, ncnt, dist = dist)

    IF ncnt LE 1 THEN BEGIN 
      message, 'No pointings within '+strtrim(radius, 2)+' degrees of '+ $
        map[ubad[i]].name, /info
    ENDIF ELSE BEGIN 
      ;; Make sure these points are not in the bad points list as
      ;; well. This also gets rid of the original point

      good = indgen(ncnt)
      FOR j = 0, ncnt-1 DO BEGIN 
        isbad = where(near[j] EQ ubad)
        IF isbad[0] NE -1 THEN good[j] = -1
      ENDFOR
      good = where(good NE -1)
      IF good[0] EQ -1 THEN BEGIN 
      message, 'No good pointings within '+strtrim(radius, 2)+' degrees of '+ $
        map[ubad[i]].name, /info
      ENDIF ELSE BEGIN 
        ;; Finally we can average some of the good points to 'fix' the
        ;; bad one. We weight by distance from the original point.

        near = near[good]
        dist = dist[good]

        ii[ubad[i]] = total(ii[near]*dist)/total(dist)
      ENDELSE 
    ENDELSE 
  ENDFOR 
  ;SAVE, Near, NCnt, Dist, file = '/d/bluesky/madsen/fixbaddies_0_240.dat'
  
END 
