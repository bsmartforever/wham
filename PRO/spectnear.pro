FUNCTION spectnear, map, glon, glat, radius, cnt, dist = dist

  ;; To make this faster, we reduce the number of viable pointings to
  ;; a box of glon, glat on a side. sphdist is only run on the
  ;; pointings in this much smaller box

  mglon = map.glon
  mglat = map.glat
  
  w1 = where(glat - radius LE mglat AND $
             mglat LE glat + radius, cnt)
  IF cnt NE 0 THEN BEGIN 
    lonradius = radius/cos(glat * !dtor)
    w2 = where(glon - lonradius LE mglon[w1] AND $
               mglon[w1] LE glon + lonradius, cnt)
    IF cnt NE 0 THEN BEGIN 
      dist = sphdist(mglon[w1[w2]], mglat[w1[w2]], glon, glat, /deg)
      w3 = where(dist LE radius, cnt)
    ENDIF
  ENDIF 

  IF cnt EQ 0 THEN $
    return, -1 $
  ELSE BEGIN 
    dist = dist[w3]
    return, w1[w2[w3]]
  ENDELSE 

;  dvect = sphdist(map.glon, map.glat, glon, glat, /deg)
;  w = where(dvect LE radius, cnt)

;  return, w

END   
