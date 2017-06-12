FUNCTION mapsliceidx, map, minlon, maxlon, minlat, maxlat, $
                      radec = radec, count = count
  IF NOT keyword_set(radec) THEN BEGIN 
    minlon = reduceto360(minlon)
    maxlon = reduceto360(maxlon)
    IF minlon LT maxlon THEN $
      index = where(map.glon GE minlon AND map.glon LE maxlon AND $
                    map.glat GE minlat AND map.glat LE maxlat, icount) $
    ELSE $
      index = where(map.glon GE minlon OR  map.glon LE maxlon AND $
                    map.glat GE minlat AND map.glat LE maxlat, icount)
  ENDIF ELSE BEGIN 
    euler, map.glon, map.glat, ra, dec, 2
    index = where(ra GE minlon*15 AND ra LE maxlon*15 AND $
                  dec GE minlat AND dec LE maxlat, icount) 
  ENDELSE  

  IF icount EQ 0 THEN BEGIN
    count = 0
    return, -1
  ENDIF ELSE BEGIN 
    count = n_elements(index)
    return, index
  ENDELSE 
END
