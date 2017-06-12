FUNCTION spectnear, map, glon, glat, radius, cnt, dist = dist

  if ~isa(radius) then radius = 0.5

  ;; To make this faster, we reduce the number of viable pointings to
  ;; a box of radius width on a side. SPHDIST is only run on the
  ;; pointings in this much smaller box
  
  mglon = abs(reduceto180(map.glon - glon))
  mglat = abs(map.glat - glat)
  
  w1 = where(mglat le radius, cnt)
  IF cnt NE 0 THEN BEGIN 
    
    ;; check for glat = +/-90 since it returns a negative value of cosine with floating point rounding
    lonradius = (90 - abs(glat) le 1e-4) ? 180 : (radius/cos(glat * !dtor) < 180)

    w2 = where(mglon[w1] le lonradius, cnt)
    IF cnt NE 0 THEN BEGIN 
      dist = sphdist(mglon[w1[w2]], mglat[w1[w2]] + glat, 0, glat, /deg)
      w3 = where(dist LE radius, cnt)
    ENDIF
  ENDIF 

  IF cnt EQ 0 THEN $
    return, -1 $
  ELSE BEGIN 
    dist = dist[w3]
    return, w1[w2[w3]]
  ENDELSE 

;; Basic idea, but too slow with large maps:

;;  dvect = sphdist(map.glon, map.glat, glon, glat, /deg)
;;  w = where(dvect LE radius, cnt)
;;  return, w

END   
