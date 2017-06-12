PRO whammap, map, vcenter, vwidth, image, smimage, bimage = bimage, $
             mind = mind, maxd = maxd, smooth = smooth, smgrid = smgrid, $
             multi = multi, useimage = useimage, title = title, $
             log = log, linear = linear, beamradius = beamradius, $
             zmax = zmax, zmin = zmin, cbottom = cbottom, $
             labels = labels, beamsonly = beamsonly, nozero = nozero, $
             loncenter = loncenter, latcenter = latcenter, limits = limits, $
             font = font, scale = scale, full = full, radec = radec, $
             backwards = backwards, fill = fill, replace = replace, $
             lonlabtop = lonlabtop, noborder = noborder, $
             nolonborder = nolonborder, nolatborder = nolatborder, $
             lonlabadj = lonlabadj, latlabadj = latlabadj, nodata = nodata, $
             nogrid = nogrid, missing = missing, background = background, $
             _extra = extra

  ;; anything to do?
  IF n_elements(map) LE 1 THEN BEGIN
    message, 'That''s not really a map...', /info
    return
  ENDIF 

  ;; parse params
  IF NOT keyword_set(font) THEN font = '!3'
  IF NOT keyword_set(cbottom) THEN cbottom = 0
  IF NOT keyword_set(smgrid) THEN smgrid = 0.05
  IF n_elements(beamradius) EQ 0 THEN beamradius = 0.5
  IF n_elements(beamradius) EQ 1 THEN $
    beamradius = fltarr(n_elements(map))+beamradius
  IF n_elements(lonlabadj) EQ 0 THEN lonlabadj = 0
  IF n_elements(latlabadj) EQ 0 THEN latlabadj = 0
  IF n_elements(latcenter) EQ 0 THEN latcenter = 0
  IF keyword_set(noborder) THEN BEGIN 
    nolonborder = 1
    nolatborder = 1
  ENDIF 
  IF n_elements(replace) NE 0 THEN BEGIN 
    ;; do this here for efficiency
    map_names = stregex(map.name, 'b[0-9]+_[0-9]+\.', /extr)
  ENDIF 

  mlon = map.glon
  mlat = map.glat

  IF keyword_set(radec) THEN BEGIN 
    euler, mlon, mlat, mlon, mlat, 2
    IF keyword_set(backwards) THEN mlon = 360-mlon
  ENDIF 

  IF NOT keyword_set(scale) THEN $  
    scale = 1/float((1 > !p.multi(1) > !p.multi(2))) * 1.0

  ;; get the limits of the map; add a little extra for the beam size
  
  IF n_elements(limits) EQ 0 THEN BEGIN 
    IF n_elements(full) NE 0 THEN BEGIN
      minlat = -90
      maxlat = +90
      loncenter = full
      minlon = full-180
      maxlon = full+180
;     maxlon = full+179.9999  ; 180 doesn't allow border to draw correctly
    ENDIF ELSE BEGIN 
      minlat = floor(min(mlat) - 1) > (-90)
      maxlat = ceil(max(mlat) + 1) < (+90)
      minlon = floor(min(mlon) - 1) > 0
;                     1 / cos((abs(maxlat) > abs(minlat))/!radeg)) > 0
      maxlon = ceil(max(mlon) + 1) < 360
;                    1 / cos((abs(maxlat) > abs(minlat))/!radeg)) < 360
      IF n_elements(loncenter) EQ 0 THEN loncenter = avg([maxlon, minlon])

      ;; check for special case mapping over 0
      IF (minlon LT 1) AND (maxlon GT 359) THEN BEGIN
        minlon = floor(min(mlon[where(mlon GT 180)]))
;                     1 / cos((abs(maxlat) > abs(minlat))/!radeg)) > 0
        maxlon = ceil(max(mlon[where(mlon LT 180)]))
;                    1 / cos((abs(maxlat) > abs(minlat))/!radeg)) < 360

        minlon = minlon - 360
        IF n_elements(loncenter) EQ 0 THEN loncenter = avg([minlon, maxlon])
      ENDIF 
    ENDELSE 
  ENDIF ELSE BEGIN 
    minlon = limits[0]
    maxlon = limits[1]
    minlat = limits[2]
    maxlat = limits[3]
    IF n_elements(loncenter) EQ 0 THEN BEGIN 
      IF minlon GT maxlon THEN $
        loncenter = avg([minlon - 360, maxlon]) $
      ELSE $
        loncenter = avg([minlon, maxlon])
    ENDIF 
  ENDELSE 

  ;; If multi is set then include the advance keyword
  IF keyword_set(multi) THEN BEGIN 
    map_set, latcenter, -loncenter, /hammer, $
      limit = [minlat, -maxlon, maxlat, -minlon], $
      /noborder, /iso, title = title, $
      _extra = extra, $
      /advance
  ENDIF ELSE BEGIN
    map_set, latcenter, -loncenter, /hammer, $
      limit = [minlat, -maxlon, maxlat, -minlon], $
      /noborder, /iso, title = title, $
      _extra = extra
  ENDELSE      
  
  ;; This array hold the shape of the beam, i.e. a circle. You can
  ;; make the number of verticies larger to get more circular beams at
  ;; the expense of much larger Postscript files. ~35,000 beams with
  ;; 20 verticies creates about a 7 MB file. 
  a = findgen(20)*(!pi*2/20.)
  a = [a, a[0]]

  IF NOT keyword_set(nodata) THEN BEGIN 
    IF keyword_set(beamsonly) THEN BEGIN

      ;; Just plot the beam positions, not the intensity map
      FOR i = 0L, n_elements(map)-1 DO BEGIN
        oplot, -mlon[i]+beamradius[i]*sin(a)/cos(mlat[i]/!radeg), $
          mlat[i]+beamradius[i]*cos(a)
      ENDFOR 

    ENDIF ELSE BEGIN 

      IF NOT keyword_set (useimage) THEN BEGIN 

        ;; These arrays hold the map that will be displayed. We generate
        ;; it here since the user didn't pass us one.
        image = fltarr(n_elements(map))
        logimage = image
        
        FOR i = 0L, n_elements(map)-1 DO BEGIN
          vv = map[i].vel
          dd = map[i].data
          
          ;; check for replacement elements
          IF n_elements(replace) NE 0 THEN BEGIN
            r = where(strpos(replace.name, map_names[i]) NE -1, rcount)
            IF rcount NE 0 THEN BEGIN 
              vv = replace[r].vel
              dd = replace[r].data
            ENDIF
          ENDIF 

          IF vwidth EQ 0 THEN BEGIN
            ;; user wants average map so select the 200 km/s aperture
            vrange = where(vv GE (max(vv)-203), vcount)
            vcount = 100
          ENDIF ELSE BEGIN 
            ;; only a select velocity interval; ignore data
            ;; outside aperture
            vrange = where(vv GE (vcenter - vwidth/2.0) $
                           AND vv LE (vcenter + vwidth/2.0) $
                           AND vv GE (max(vv)-200), $
                           vcount)
          ENDELSE 

          IF vcount EQ 0 THEN BEGIN
            ;; No data points in the velocity range. Bummer.
            print, 'No data for ', i, ' at ', $
              mlon[i], ',', mlat[i]
            image[i] = 0
          ENDIF ELSE IF vcount EQ 1 THEN BEGIN
            ;; Only one data point in the velocity range. Ugh. We'll
            ;; treat it as 2 equal data points separated by dv
            image[i] = dd[vrange] / $
              (vv[vrange]-vv[vrange-1])
          ENDIF ELSE BEGIN 
            image[i] = $
              int_tabulated(vv[vrange], dd[vrange]) / $
              (max(vv[vrange])-min(vv[vrange]))
          ENDELSE 

          ;; print, 'Beam ' + string(i) + ' used ' + string(vcount) + $
          ;;        ' data points'

        ENDFOR
      ENDIF ;; useimage

      IF NOT keyword_set(linear) THEN $
        logimage = alog10(image > min(image(where(image GT 0))))
      
      mind = min(image)
      maxd = max(image)

      ;; Set the display min/maxes if the user didn't specify
      IF NOT keyword_set(zmax) THEN BEGIN
        zmax = maxd
      ENDIF
      IF NOT keyword_set(zmin) THEN BEGIN
        IF keyword_set(log) THEN BEGIN 
          zmin = mind > min(image(where(image GT 0)))
        ENDIF ELSE BEGIN 
          zmin = mind
        ENDELSE               
      ENDIF

      ;; Create the color scaled image. The top color is not used in
      ;; case we are using a colormap that has a drawing color at the
      ;; top (e.g. 39 & 40) or we are using it for the background.

      ;; cbottom allows the user to avoid using a low color range.

      IF keyword_set(log) THEN BEGIN 
        bimage = bytscl(logimage, $
                        max = alog10(zmax), min = alog10(zmin), $
                        top = !d.table_size-2-cbottom) $
          + cbottom
      ENDIF ELSE IF keyword_set(linear) THEN BEGIN 
        bimage = bytscl(image, $
                        max = zmax, min = zmin, $
                        top = !d.table_size-2-cbottom) $
          + cbottom
      ENDIF ELSE BEGIN 
        bimage = hist_equal(logimage, top = !d.table_size-2-cbottom) $
          + cbottom
      ENDELSE 
      
      IF keyword_set(smooth) THEN BEGIN 

        ;; Set the color of the area outside the data
        IF n_elements(missing) EQ 0 THEN BEGIN 
          IF !d.name EQ 'PS' THEN BEGIN
            missing = byte(!d.table_size-1)
          ENDIF ELSE BEGIN
            missing = byte(!p.background)
          ENDELSE
        ENDIF 

        ;; Fill in points not in the given map, if the user wants a
        ;; nice, smooth boundary around the map.
        IF keyword_set(fill) THEN $
          fillmap, mlon, mlat, bimage, missing
        
        ;; Create a regularly gridded image

        ;; We have to morph the longitude for several reasons:

        ;; (1) We subtract loncenter and reduceto180 to transform to
        ;;     +/-180. This avoids seam problems by making the endpoints
        ;;     of the trigrid the edges of the map. We transform back
        ;;     during the map_patch below.
        ;; (2) We invert the sense because IDL can't handle sky map
        ;;     projections by itself.

        x = -reduceto180((mlon-loncenter))
        y = mlat

        tglimits = [min(x), min(y), max(x), max(y)]

        switch smooth OF

          1: 
          2: BEGIN 

            IF smooth EQ 1 THEN BEGIN 
              ;; Planar gridding (fast, but wrong at high lats)
              
              triangulate, x, y, triangles
              
              smimage = byte(round(trigrid(x, y, bimage, triangles, $
                                           [smgrid, smgrid], tglimits, $
                                           missing = missing)))
;                > 0 < (!d.table_size-2-cbottom)
            ENDIF ELSE BEGIN 
              ;; Spherical gridding (slow, but correct at high lats)
              ;; NOTE: I have found this to crash IDL with larger
              ;; datasets. No idea yet how to get around this. It also
              ;; craps out when given the 0-360 boundary with small grid
              ;; spacings.
              
              f = bimage
              
              triangulate, x, y, triangles, $
                sphere = s, fvalue = f, /degrees

              ;; total hack
;            IF abs(tglimits[2] + tglimits[0]) LE 0.01 THEN BEGIN
;             tglimits[0]= tglimits[0]+1
;              tglimits[2] = tglimits[2]-1
;            ENDIF 
              
              smimage = byte((trigrid(f, [smgrid, smgrid], tglimits, $
                                      sphere = s, /degrees, $
                                      missing = missing)) $
                             > 0 < (!d.table_size-2-cbottom))
              
            ENDELSE 

            ;; If the user requested, set zero pixels to the background
            ;; color after all the scaling has happened
            IF n_elements(nozero) NE 0 THEN BEGIN 
              IF n_elements(nozero) EQ 1 THEN BEGIN
                zeros = where(smimage EQ 0)
                smimage[zeros] = missing
              ENDIF ELSE BEGIN 
                ;; user passed in an explicit list of beams to zero
                smimage[nozero] = missing
              ENDELSE
            ENDIF 
            
            ;; Note that we are now transforming the lon0/1 back to the
            ;; original coordinate system so that the smoothed image ends up
            ;; on the right place on the map.
;      mimage = map_patch(smimage, lat0 = tglimits[1], lat1 = tglimits[3], $
;                         lon0 = tglimits[0]-loncenter, $
;                         lon1 = tglimits[2]-loncenter, $
;                         xsize = xsize, ysize = ysize, $
;                         xstart = xs, ystart = ys, missing = missing)

            mimage = map_image(smimage, xs, ys, xsize, ysize, $
                               latmin = tglimits[1], latmax = tglimits[3], $
                               lonmin = tglimits[0]-loncenter, $
                               lonmax = tglimits[2]-loncenter, $
                               /bilin, compress = 1, scale = 0.06, $
                               missing = missing)
            
            ;; finally display image
            tv, mimage, xs, ys, xsize = xsize, ysize = ysize
            print, 'x', xs, ys
            break
          END 
          
          3: BEGIN 
            ;; contour maps
            print, 'making contour map'
            contour, bimage, x, y, /irr, /overplot, _extra = extra
            break
          END
        ENDSWITCH
      ENDIF ELSE BEGIN ;; smooth

        ;; 24/8 bit color selector
        IF !d.n_colors GT 256 THEN BEGIN
          cimage = (1 + 256L + 256L * 256L) * bimage
        ENDIF ELSE BEGIN
          cimage =  bimage
        ENDELSE 

        ;; don't plot zero value pixels if they requested
        IF n_elements(nozero) NE 0 THEN BEGIN 
          IF n_elements(nozero) EQ 1 THEN BEGIN 
            nozerobeams = where(cimage NE 0)
;          map = map[nozerobeams]
          ENDIF ELSE BEGIN 
            nozerobeams = intarr(n_elements(cimage))
            nozerobeams[nozero] = 1
            nozerobeams = where(nozerobeams NE 1)
          ENDELSE 

          cimage = cimage[nozerobeams]
          mlon = mlon[nozerobeams]
          mlat = mlat[nozerobeams]
        ENDIF 

        FOR i = 0L, n_elements(cimage)-1 DO BEGIN
          IF mlat[i] GT 89.5 THEN BEGIN 
            ;; special case for the pole pointing(s)
            polyfill, a*!radeg, mlat[i]-beamradius[i], color = cimage[i]
          ENDIF ELSE BEGIN 
            ;; plot the beams
            polyfill, $
              -mlon[i]+beamradius[i]*sin(a)/cos(mlat[i]/!radeg), $
              mlat[i]+beamradius[i]*cos(a), color = cimage[i]
            
            ;; print the values 
            IF (NOT keyword_set(multi) AND keyword_set(labels)) THEN BEGIN
              xyouts, -mlon[i], mlat[i], align = 0.5, $
                strtrim(string(image[i], format = '(F8.2)'), 2)
            ENDIF
          ENDELSE 
        ENDFOR 
      ENDELSE ;; smooth
    ENDELSE ;; beamsonly
  ENDIF ;; nodata
  
  ;; plot the axes labels and the grid

  dlat = maxlat-minlat
  dlon = minlon LT maxlon ? maxlon-minlon : minlon-(maxlon-360)

  IF n_elements(full) NE 0 THEN BEGIN 
    glatmin = -90
    glatmax = +90
    glatdel = 15
    glonmin = 0
    glonmax = 360
    glondel = 30
  ENDIF ELSE BEGIN 
    gridspacing, minlon, maxlon, glonmin, glonmax, glondel, $
      adjust = lonlabadj
    gridspacing, minlat, maxlat, glatmin, glatmax, glatdel, $
      adjust = latlabadj
  ENDELSE 

  FOR lat = glatmin, glatmax, glatdel DO BEGIN
    IF NOT keyword_set(nogrid) THEN $
      oplot, -maxlon+lindgen(1000)*dlon/1000.0, $
      intarr(1000)+lat, line = 1, /noclip
    IF n_elements(full) EQ 0 THEN $
      xyouts, -(maxlon+dlon*0.02*scale $
                / (!x.window[1]-!x.window[0]) / cos(lat/!radeg)), $
      lat, font + strtrim(string(fix(lat)), 2), $
      align = 1.0, charsize = scale
  ENDFOR 

  FOR lon = glonmax, glonmin, -glondel DO BEGIN
    IF NOT keyword_set(nogrid) THEN $
      oplot, intarr(1000)-lon, $
      minlat+lindgen(1000)*dlat/1000.0, $
      line = 1, /noclip
    IF keyword_set(lonlabtop) THEN $
      latlab = maxlat+dlat*0.01/(!y.window[1]-!y.window[0]) $
    ELSE $
      latlab = minlat-dlat*0.03*scale/(!y.window[1]-!y.window[0])
    IF n_elements(full) EQ 0 THEN BEGIN 
      lonnum = keyword_set(backwards) ? 360-lon : lon
      lontext = keyword_set(radec) ? $
        strtrim(string(lonnum/15.0, format = '(F6.2)'), 2) : $
        strtrim(string(fix(reduceto360(lonnum))), 2)
      xyouts, -lon, latlab, $
        font + lontext, $
        align = 0.5, charsize = scale
    ENDIF 
  ENDFOR 
  
  ;; a nice border
  IF NOT keyword_set(nolonborder) THEN BEGIN 
    oplot, -maxlon+lindgen(1001)*dlon/1000.0, $
      intarr(1001)+(minlat + 1e-2), /noclip
    oplot, -maxlon+lindgen(1001)*dlon/1000.0, $
      intarr(1001)+(maxlat - 1e-2), /noclip
  ENDIF 
  IF NOT keyword_set(nolatborder) THEN BEGIN 
    oplot, intarr(1001)-(minlon + 1e-2), $
      minlat+lindgen(1001)*dlat/1000.0, /noclip
    oplot, intarr(1001)-(maxlon - 1e-2), $
      minlat+lindgen(1001)*dlat/1000.0, /noclip
  ENDIF 
END
