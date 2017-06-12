PRO fillmapgreg, lon, lat, image, missing
  
  ;; Get all pre-defined WHAM block pointings
  restore, '/d/wham/survey/idl/points.dat'
  
  ;; Reduce the array to the ones in the current map
  Points_Struct = REPLICATE( {Lon:0.0, Lat:0.0}, N_ELEMENTS(Points[0, *])) 
  FOR i = 0L, N_ELEMENTS(Points_Struct)-1 DO BEGIN
     Points_Struct[i].Lon = Points[0, i]
     Points_Struct[i].Lat = Points[1, i]
  ENDFOR
  Points_Struct = Points_Struct[WHERE(Points_Struct.Lon GE MIN(Lon)-1 AND $
                                      Points_Struct.Lon LE MAX(Lon)+1 AND $
                                      Points_Struct.Lat GE MIN(Lat)-1 AND $
                                      Points_Struct.Lat LE MAX(Lat)+1)]
  
  ;; Go through Points_Struct and see if a [Lon,Lat] exists
  ;; If not, put a 'missing' entry in the Map

  FOR i = 0, N_ELEMENTS(Points_Struct)-1 DO BEGIN
     Ind = WHERE(ABS(Lon-Points_Struct[i].Lon) LE 0.1 AND $
                 ABS(Lat-Points_Struct[i].Lat) LE 0.1, Count)
     
     IF Count EQ 0 THEN BEGIN
        Lon = [Lon, Points_Struct[i].Lon]
        Lat = [Lat, Points_Struct[i].Lat]
        Image = [Image, Missing]
     ENDIF
  ENDFOR
  
END 
