PRO smoothmap, map, vcenter, vwidth, min, max, image, smimage, $
               multi = multi, useimage = useimage, $
               nohe = nohe, log = log, zmax = zmax, zmin = zmin, $
               bound = bound, title = title, scale = scale, nodisp = nodisp, $
               xm = xm, ym = ym

  ;; parse params
  IF NOT keyword_set(log) THEN he = 1
  IF NOT keyword_set(xm) THEN xm = [1, 1]
  IF NOT keyword_set(ym) THEN ym = [1, 2]
  
  IF NOT keyword_set(scale) THEN $  
  IF NOT keyword_set(scale) THEN $
    scale = 1/float((1 > !p.multi(1) > !p.multi(2))) * 1.0
  oldcs = !p.charsize
  !p.charsize = scale*2
  
  ;; get the limits of the map; add a little extra for the beam size
  glons = map.glon
  glats = map.glat

  ;; compute the map boundary to avoid TRIGRID streaking
  IF keyword_set(bound) THEN BEGIN 
      t = systime(1)
      b = boundary(map)
      glons = [glons, b.glon]
      glats = [glats, b.glat]
      print, 'Boundary: ', systime(1)-t, ' seconds'
  END 

  minlat = floor(min(glats) - 1)
  maxlat = ceil(max(glats) + 1)
  minlon = floor(min(glons) - $
                 1 / cos((abs(maxlat) > abs(minlat))/!radeg))
  maxlon = ceil(max(glons) + $
                1 / cos((abs(maxlat) > abs(minlat))/!radeg))

  ;; if multi is set (normally only set by 'blobmapall') then include
  ;; the advance keyword and add a border for the colorbar
  IF NOT keyword_set(nodisp) THEN BEGIN 
      IF keyword_set(multi) THEN BEGIN 
          map_set, 0, avg([-maxlon, -minlon]), /ait, $
            limit = [minlat, -maxlon, maxlat, -minlon], $
            /noborder, /advance, /iso, title = title, $
            xmargin = xm, ymargin = ym
      ENDIF ELSE BEGIN
          map_set, 0, avg([-maxlon, -minlon]), /ait, $
            limit = [minlat, -maxlon, maxlat, -minlon], $
            /noborder, /iso, title = title, $
            xmargin = xm, ymargin = ym
      ENDELSE
  ENDIF 
 
  t = systime(1)
  IF NOT keyword_set (useimage) THEN BEGIN 
      ;; These arrays hold the map that will be displayed
      image = fltarr(n_elements(map))
      logimage = image
          
      ;; Load up the array with velocity sliced data
      FOR i = 0, n_elements(map)-1 DO BEGIN
          IF vwidth EQ 0 THEN BEGIN
              ;; user wants average map so select the 200 km/s aperture
              vrange = where(map(i).vel GE (max(map(i).vel)-200.0), vcount)
          ENDIF ELSE BEGIN 
              ;; only a select velocity interval
              vrange = where(map(i).vel GE (vcenter - vwidth/2.0) $
                             AND map(i).vel LE (vcenter + vwidth/2.0), vcount)
          ENDELSE 
          image(i) = int_tabulated(map(i).vel(vrange), map(i).data(vrange))
          image(i) = image(i)/(max(map(i).vel(vrange))-min(map(i).vel(vrange)))
          
          logimage(i) = alog10(image(i) > 1)
      ENDFOR

  ENDIF ELSE BEGIN
      logimage = alog10(image >  1)
  ENDELSE 
      
  min = min(image)
  max = max(image)
  
  IF NOT keyword_set(zmax) THEN BEGIN
      zmax = max
  ENDIF
  IF NOT keyword_set(zmin) THEN BEGIN
      zmin = min
  ENDIF

  print, 'Loading: ', systime(1)-t, ' seconds'

  ;; Scale here to avoid taking boundary into account.
  ;; The whole color table is not used to avoid problems
  ;; with maps that have a drawing color at the top (e.g. 39 & 40)
  ;;
  ;; Other options that seem to work well here are:
  ;; bimage = bytscl(logimage <  alog10(500.0), top = !d.table_size*.95)
  ;; bimage = hist_equal(logimage, top = !d.table_size*.95)
  ;; bimage = hist_equal(image, top = !d.table_size*.95)
  IF keyword_set(nohe) THEN BEGIN
      scimage = bytscl(image, top = !d.table_size-2)
  ENDIF ELSE IF keyword_set(log) THEN BEGIN 
      scimage = bytscl(logimage < alog10(zmax) > alog10(zmin), $
                       top = !d.table_size-2)
  ENDIF ELSE BEGIN
      scimage = hist_equal(image, top = !d.table_size-2)
  ENDELSE 
  
  ;; set the color of the area outside the data
  IF !d.name EQ 'PS' THEN BEGIN
      missing = byte(!d.table_size-1)
  ENDIF ELSE BEGIN
      missing = byte(0)
  ENDELSE
      
  ;; if a boundary has been computed, set the boundary points to the
  ;; appropriate color
  IF keyword_set(bound) THEN BEGIN
      bimage = bytarr(n_elements(b)) + byte(missing)
      scimage = [scimage, bimage]
  ENDIF 

  ;; Create a regularly gridded image
  x = -glons
  y = glats
  triangulate, x, y, triangles

  t = systime(1)

  smimage = byte(round(trigrid(x, y, scimage, triangles, $
                    [0.25, 0.25], missing = missing)) > 0 < (!d.table_size-1))

  print, 'Smoothing: ', systime(1)-t, ' seconds'
  t = systime(1)
  
  IF NOT keyword_set(nodisp) THEN BEGIN
      ;; warp image to map projection
      mimage = map_image(smimage, xs, ys, $
                         xsize, ysize, compress = 1, $
                         latmin = min(y), latmax = max(y), $
                         lonmin = min(x), lonmax = max(x), $
                         scale = 0.06)

      print, 'Map warping: ', systime(1)-t, ' seconds'

      ;; finally display image
      tv, mimage, xs, ys, xsize = xsize, ysize = ysize

      ;; plot the axes labels and the grid
      gridspacing, minlat, maxlat, glatmin, glatmax, glatdel
      gridspacing, minlon, maxlon, glonmin, glonmax, glondel
      
      FOR lat = glatmin, glatmax, glatdel DO BEGIN
          oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, $
            intarr(1000)+lat, line = 1
          xyouts, -(maxlon+(maxlon-minlon)*0.01 $
                    / (!x.window(1)-!x.window(0)) / cos(lat/!radeg)), $
            lat, '!6' + strtrim(string(fix(lat)), 2), $
            align = 1.0, charsize = scale
      END 
      
;  alt = 0
      FOR lon = glonmax, glonmin, -glondel DO BEGIN
;      IF alt EQ 0 THEN BEGIN 
          oplot, intarr(1000)-lon, minlat+indgen(1000)*(maxlat-minlat)/1000.0,$
            line = 1
          xyouts, -lon, minlat-(maxlat-minlat)*0.03*scale $
            / (!y.window(1)-!y.window(0)), $
            '!6' + strtrim(string(fix(lon)), 2), $
            align = 0.5, charsize = scale
;          alt = 1
;      ENDIF ELSE BEGIN
;          alt = 0
;      ENDELSE 
      ENDFOR 
      
      ;; a nice border
      oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, intarr(1000)+minlat
      oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, intarr(1000)+maxlat
      oplot, intarr(1000)-minlon, minlat+indgen(1000)*(maxlat-minlat)/1000.0
      oplot, intarr(1000)-maxlon, minlat+indgen(1000)*(maxlat-minlat)/1000.0

;      IF keyword_set(multi) THEN BEGIN 
;          map_set, 0, avg([-maxlon, -minlon]), /ait, $
;            limit = [minlat, -maxlon, maxlat, -minlon], $
;            /noborder, /advance, /iso, $
;            xmargin = xm, ymargin = ym
;      ENDIF 

  ENDIF 

  !p.charsize = oldcs
END

