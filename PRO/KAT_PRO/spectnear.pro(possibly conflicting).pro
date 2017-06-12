FUNCTION spectnear, map, glon, glat, radius, cnt, dist = dist, magellanic=magellanic

  ;; To make this faster, we reduce the number of viable pointings to
  ;; a box of glon, glat on a side. sphdist is only run on the
  ;; pointings in this much smaller box

if (NOT keyword_set(magellanic)) then begin  
  w1 = where(glat - radius LE map.glat AND $
             map.glat LE glat + radius, cnt)
  IF cnt NE 0 THEN BEGIN 
    lonradius = radius/cos(glat * !dtor)
    w2 = where(glon - lonradius LE (map[w1]).glon AND $
               (map[w1]).glon LE glon + lonradius, cnt)
    IF cnt NE 0 THEN BEGIN 
      dist = sphdist((map[w1[w2]]).glon, (map[w1[w2]]).glat, glon, glat, /deg)
      w3 = where(dist LE radius, cnt)
    ENDIF
  ENDIF 
endif else begin
  mlat=glat & mlon=glon
  w1 = where(mlat - radius LE map.mlat AND $
             map.mlat LE mlat + radius, cnt)
  IF cnt NE 0 THEN BEGIN 
    lonradius = radius/cos(mlat * !dtor)
    w2 = where(mlon - lonradius LE (map[w1]).mlon AND $
               (map[w1]).mlon LE mlon + lonradius, cnt)
    IF cnt NE 0 THEN BEGIN 
      dist = sphdist((map[w1[w2]]).mlon, (map[w1[w2]]).mlat, mlon, mlat, /deg)
      w3 = where(dist LE radius, cnt)
    ENDIF
  ENDIF 
endelse

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
