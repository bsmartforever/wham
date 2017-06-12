FUNCTION boundary, map, value
  IF n_elements(value) EQ 0 THEN value = 0.0
  
  slonsp = 0.98
  slatsp = 0.98*sqrt(3.0)/2.0

  lonsp = slonsp/2.0
  latsp = slatsp/2.0

  lons = map.glon
  lats = map.glat
  
  b = map
  i = 0
  
  lat = min(lats)
  WHILE (lat LE max(lats)) DO BEGIN
      slice = where(lons GE 0 AND lons LE 360 AND $
                    lats GE lat-0.5 AND lats LE lat+0.5)

      b(i).glon = min(lons(slice)) - slonsp/cos(lat/!radeg)
      b(i).glat = lat
      b(i).data = b(0).data*0.0 + value
      
      b(i+1).glon = max(lons(slice)) + slonsp/cos(lat/!radeg)
      b(i+1).glat = lat
      b(i+1).data = b(0).data*0.0 + value

      lat = lat+latsp
      i = i+2
  ENDWHILE 

  lon = min(lons)
  WHILE (lon LE max(lons)) DO BEGIN
      slice = where(lons GE lon-0.5 AND lons LE lon+0.5 AND $
                    lats GE -90 AND lats LE 90)
      biggest = max(abs([min(lats(slice)), max(lats(slice))]))

      slice = where(lons GE lon-0.5/cos(biggest/!radeg) AND $
                    lons LE lon+0.5/cos(biggest/!radeg) AND $
                    lats GE -90 AND lats LE 90)

      b(i).glat = min(lats(slice))-slatsp
      b(i).glon = lon
      b(i).data = b(0).data*0.0 + value
      
      b(i+1).glat = max(lats(slice))+slatsp
      b(i+1).glon = lon
      b(i+1).data = b(0).data*0.0 + value

      lon = lon+lonsp
      i = i+2
  ENDWHILE 
  
  return, b(0:i-1)
END 
