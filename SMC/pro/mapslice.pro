FUNCTION mapslice, map, minlon, maxlon, minlat, maxlat, $
                   radec = radec, count = count, index = index
                   
;+
;NAME: mapslice
;SYNTAX: sliced_map = mapslice(map, minlon, maxlon, minlat, maxlat, $
;	[radec=radec] [, count=count] [, index=index]
;-

  index = mapsliceidx(map, minlon, maxlon, minlat, maxlat, $
                      radec = radec, count = count)

  IF count EQ 0 THEN $
    return, -1 $
  ELSE $
    return, map[index]
END
