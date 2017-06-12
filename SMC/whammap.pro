
PRO whammap, map, vcenter, vwidth, image, smimage, bimage = bimage, $
             mind = mind, maxd = maxd, smooth = smooth, smgrid = smgrid, $
             multi = multi, useimage = useimage, title = title, $
             log = log, linear = linear, beamradius = beamradius, $
             zmax = zmax, zmin = zmin, cbottom = cbottom, labels = labels, $
             beamsonly = beamsonly, beam_color=beam_color, beam_thick=beam_thick,color = color, $
             nozero = nozero, loncenter = loncenter, latcenter = latcenter, $
             limits = limits, $
             font = font, scale = scale, full = full, radec = radec, $
             backwards = backwards, fill = fill, replace = replace, $
             lonlabtop = lonlabtop, noborder = noborder, $
             nolonborder = nolonborder, nolatborder = nolatborder, $
             lonlabadj = lonlabadj, latlabadj = latlabadj, $
             nodata = nodata, nogrid = nogrid, nolabels = nolabels, $
             noframe = noframe, missing = missing, background = background, $
             padding = padding, lon_labels = lon_labels, lat_labels = lat_labels, $
             coordbounds=coordbounds, charsize=charsize,charthick=charthick,$
             magellanic=magellanic, gal2mag=gal2mag, xytitles=xytitles,$
             glondel=glondel,glatdel=glatdel,_extra = extra
;+
;WHAMMAP syntax:
;
;PRO whammap, map, vcenter, vwidth, image, smimage, bimage = bimage, $
;             mind = mind, maxd = maxd, smooth = smooth, smgrid = smgrid, $
;             multi = multi, useimage = useimage, title = title, $
;             log = log, linear = linear, beamradius = beamradius, $
;             zmax = zmax, zmin = zmin, cbottom = cbottom, $
;             labels = labels, beamsonly = beamsonly, color = color, $
;             nozero = nozero, loncenter = loncenter, latcenter = latcenter, $
;             limits = limits, $
;             font = font, scale = scale, full = full, radec = radec, $
;             backwards = backwards, fill = fill, replace = replace, $
;             lonlabtop = lonlabtop, noborder = noborder, $
;             nolonborder = nolonborder, nolatborder = nolatborder, $
;             lonlabadj = lonlabadj, latlabadj = latlabadj, $
;             nodata = nodata, nogrid = nogrid, nolabels = nolabels, $
;             noframe = noframe, missing = missing, background = background, $
;             padding = padding, lon_labels = lon_labels, lat_labels = lat_labels, $
;             coordbounds = coordbounds, $
;             _extra = extra
;
;OPTIONAL KEYWORD INPUTS:
;   coordbounds: manually specified limits. Only relevant if full=full is not
;       set. Syntax: coordbounds=[minlon, maxlon, minlat, maxlat]
;-

; MODIFICATIONS:
;             
; 2005-8-16 ASH: added padding keyword (set the padding at the edge of the map)
; 2010-4-27 LMH: added lon_labels & lat_labels; lonlabtop deprecated but still allowed
;              :   lon/lat_labels = 0, no labels; 1, standard location; 2, alt location
; 2013-4-21 KAT: added magellanic keyword. Expects map.mlat, map.mlon
; 2013-3    KAT: fixed charsize keyword such that the varying values no longer alter the 
;                  size of the plotting region.
; 2013-5-17 KAT: added xytitles keyword to label the x and y axes.
;                  xytitles = 1 + magellanic = 0 => 'Galactic Longitude/Latitude (Degrees)'
;                  xytitles = 1 + magellanic = 1 => 'Magellanic Stream Longitude/Latitude (Degrees)'
;                  xytitles 2 element string array => user specified labels.
; 2013-5-17 KAT: fixed colors such that color and background point to color indices 253 & 254.
;                  These are color indices that are ignored in the mapping and in colorbar.
;                  User now specifies color and background with a string name; see cgColor 
;                  allowed string names. Requires the coyote idl library. 
;                     e.g., color='red', background='blue'
;                  Previously, artifical contours were created because the color and background
;                  indices could correspond the indices used in mapping and creating the colorbar.
; 2013-10   KAT: Tweaks to the creation of the smimage to search for nearest neighbors in GRIDDATA.
;                  This removes the issue with duplicate points. I also added some error handling 
;                  for when the smimage dimensions are too small. Too small of dimensions will 
;                  will result in no map being produced, especially when smooth = 1 or 2. 
;
;

  ;; anything to do?
  IF n_elements(map) LE 1 THEN BEGIN
    message, 'That''s not really a map...', /info
    return
  ENDIF 
  ;; parse params
  IF NOT keyword_set(font) THEN font = '!3'
  IF NOT keyword_set(cbottom) THEN cbottom = 0
  IF NOT keyword_set(smgrid) THEN smgrid = 0.05
  IF n_elements(beamradius) EQ 0 THEN beamradius = 0.6
  IF n_elements(beamradius) EQ 1 THEN $
    beamradius = fltarr(n_elements(map))+beamradius
  IF n_elements(latcenter) EQ 0 THEN latcenter = 0

  ;; coordinate label params
  if keyword_set(nolabels) then begin
    lon_labels = 0
    lat_labels = 0
  endif
  if keyword_set(lonlabtop) then lon_labels = 2
  if n_elements(lon_labels) eq 0 then lon_labels = 1
  if n_elements(lat_labels) eq 0 then lat_labels = 1  
  IF n_elements(lonlabadj) EQ 0 THEN lonlabadj = 0
  IF n_elements(latlabadj) EQ 0 THEN latlabadj = 0

  IF keyword_set(noborder) THEN BEGIN 
    nolonborder = 1
    nolatborder = 1
  ENDIF 
  padding = keyword_set(padding) ? padding : 1
  IF n_elements(replace) NE 0 THEN BEGIN 
    ;; do this here for efficiency
    map_names = stregex(map.name, 'b[0-9]+_[0-9]+\.', /extr)
  ENDIF 

  if NOT keyword_set(charsize) then charsize=!p.charsize
  ;if !d.name eq 'PS' then charsize=charsize*double(!d.x_ch_size)/double(!d.x_vsize)

  if NOT keyword_set(charthick) then charthick=!p.charthick

p_str=!p

if (size(color,/type) eq 2) OR (size(background,/type) eq 2) then begin
    print,' '
    print,'*** Please pass strings containing color names, not intger values. ***'
    print,'      *** Assuming backgound = white and color = black ***' 
    print,'       *** See cgcolor.pro for details on valid names ***'
    print,' '
endif

if size(background,/type) eq 7 then begin
   background=cgColor(background,/triple)
   TVLCT,background,255
endif else TVLCT, 255, 255, 255, 255 ; White color

if size(color,/type) eq 7 then begin
   color=cgColor(color,/triple)
   TVLCT,color,254
endif else TVLCT, 0, 0, 0, 254       ; Black color

;!P.Color = 254
;255 = 255

  if (NOT keyword_set(magellanic)) AND (NOT keyword_set(gal2mag)) then begin
     mlon = map.glon
     mlat = map.glat
  endif else if (keyword_set(magellanic)) AND (NOT keyword_set(gal2mag)) then begin
     mlon = map.mlon
     mlat = map.mlat
  endif else begin
    gal2mag,map.glon,map.glat,mlon,mlat
    limits=[min(mlon),max(mlon),min(mlat),max(mlat)]
    magellanic=1
  endelse

  if keyword_set(limits) then begin
    subindex=where((mlon le limits[1]) $
         and (mlon ge limits[0]) $
         and (mlat lt limits[3]) $
         and (mlat ge limits[2]))
    ;map=map(subindex)
    ;if keyword_set(useimage) then image=image(subindex)
  endif

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
      if (not keyword_set(loncenter)) then loncenter = full
      minlon = full-180
      maxlon = full+180
;     maxlon = full+179.9999  ; 180 doesn't allow border to draw correctly

      lon_labels = 0
      lat_labels = 0
    ENDIF ELSE BEGIN 
        IF keyword_set(coordbounds) THEN BEGIN
            minlon = coordbounds[0]
            maxlon = coordbounds[1]
            minlat = coordbounds[2]
            maxlat = coordbounds[3]
        ENDIF ELSE BEGIN
            minlat = floor(min(mlat) - padding) > (-90)
            maxlat = ceil(max(mlat) + padding) < (+90)
            minlon = floor(min(mlon) - padding) > 0
      ;                     1 / cos((abs(maxlat) > abs(minlat))/!radeg)) > 0
            maxlon = ceil(max(mlon) + padding) < 360
      ;                    1 / cos((abs(maxlat) > abs(minlat))/!radeg)) < 360
        ENDELSE
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
      /noborder, /iso, title = title, charsize = 1, $
      _extra = extra, $
      /advance,color=254
  ENDIF ELSE BEGIN
    map_set, latcenter, -loncenter, /hammer, $
      limit = [minlat, -maxlon, maxlat, -minlon], $
      /noborder, /iso, title = title, charsize = 1, $
      _extra = extra,color=254
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
         IF NOT keyword_set(beam_color) then beam_color=254
         IF NOT keyword_set(beam_thick) then beam_thick=2
        oplot, -mlon[i]+beamradius[i]*sin(a)/cos(mlat[i]/!radeg), $
          mlat[i]+beamradius[i]*cos(a), color = beam_color,thick=beam_thick
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

          IF n_elements(vv) EQ 1 THEN BEGIN
            ;; input data has no velocity info
            vcount = -1
          ENDIF ELSE IF vwidth EQ 0 THEN BEGIN
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

          IF vcount EQ -1 THEN BEGIN
            image[i] = dd
          ENDIF ELSE IF vcount EQ 0 THEN BEGIN
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
      IF N_ELEMENTS(zmax) EQ 0 THEN BEGIN
        zmax = maxd
      ENDIF
      IF N_ELEMENTS(zmin) EQ 0 THEN BEGIN
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
                        top = !d.table_size-3-cbottom) $
          + cbottom
      ENDIF ELSE IF keyword_set(linear) THEN BEGIN 
        bimage = bytscl(image, $
                        max = zmax, min = zmin, $
                        top = !d.table_size-3-cbottom) $
          + cbottom
      ENDIF ELSE BEGIN 
        bimage = hist_equal(logimage, top = !d.table_size-3-cbottom) $
          + cbottom
      ENDELSE 
      
      IF keyword_set(smooth) THEN BEGIN 

        ;; Set the color of the area outside the data
        IF keyword_Set(missing) THEN BEGIN 
          IF !d.name EQ 'PS' THEN BEGIN
            missing = byte(255)
            print, missing
            print, "IS THIS HERE?!?!?"
          ENDIF ELSE BEGIN
            ;missing = byte(255)
            missing = byte(50)
            print, "ARE YOU WORKING!?!?!?!"
          ENDELSE
        ENDIF 

        ;; Fill in points not in the given map, if the user wants a
        ;; nice, smooth boundary around the map.
        
        ;; if plotting the full sky, untrimmed WHAM map, just fill in dec <= -20
        IF keyword_set(fill) THEN BEGIN 
         IF n_elements(full) EQ 0 OR n_elements(map) LT 37565 THEN BEGIN 
            fillmap, mlon, mlat, bimage, missing
         ENDIF ELSE BEGIN 
            fillmap, mlon, mlat, bimage, missing, decrange = [-90, -20]
          ENDELSE
        ENDIF 
   
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
        nx = (max(x)-min(x))/smgrid + 1
        ny = (max(y)-min(y))/smgrid + 1
        xout = min(x) + indgen(nx)*smgrid
        yout = min(y) + indgen(ny)*smgrid

        switch smooth OF

          1: 
          2: BEGIN 

            IF smooth EQ 1 THEN BEGIN 
              ;; Planar gridding (fast, but wrong at high lats)
              
              triangulate, x, y, triangles
              
              smimage = byte(round(trigrid(x, y, bimage, triangles, $
                                           [smgrid, smgrid], tglimits, $
                                           missing = missing)))
              if (n_elements(smimage[0,*]) lt 10) OR (n_elements(smimage[*,0]) lt 10) then begin
                print,''
                print,'***   Size of small image is too small to produce a map    ***'
                print,'*** Please make sure that either the map has enough points ***'
                print,'***            Or set smgrid to a smaller value            ***'
                print,''
                return
              endif 

;                > 0 < (!d.table_size-2-cbottom)
            ENDIF ELSE BEGIN 
              f = bimage
              
;              triangulate, x, y, triangles, $
;                sphere = s, fvalue = f, /degrees
              
;              smimage = byte((trigrid(f, [smgrid, smgrid], tglimits, $
;                                      sphere = s, /degrees, $
;                                      missing = missing)) $
;                             > 0 < (!d.table_size-2-cbottom))

;              triangulate, x, y, triangles, sphere = tsphere, /degrees, $
;                           fvalue = f

              if keyword_set(magellanic) then qhull, x, y, triangles, sphere = tsphere, /delaunay $
              else TRIANGULATE, x, y, triangles, /DEGREES

              ;; Break out scale trimming since kringing can occassionally 
              ;; raise max pixels above input maximum. This is not a big
              ;; deal unless missing=255 (PS, white typcially) where
              ;; the missing then gets trimmed to 254 instead.
              
              if keyword_set(magellanic) then begin
                gd = griddata(x, y, f,METHOD = method,$
                   tri = triangles, missing = -1, $
                   xout = xout, yout = yout, /grid, /sphere, /degree) $
                   < (!d.table_size-1-cbottom)
              endif else begin
                method="NaturalNeighbor"
                gd = GRIDDATA(x, y, f, /DEGREES, SPHERE = KEYWORD_SET(sphere), $
                    METHOD = method, missing = -1, $
                    XOUT = oLon, YOUT = oLat, TRIANGLES = triangles) 
              endelse 

              gd_missing = where(gd eq missing, gdm_cnt)
              if gdm_cnt ne 0 then gd[gd_missing] = missing
              
              smimage = byte( gd > 0 )
            ENDELSE 

            ;; If the user requested, set zero pixels to the background
            ;; color after all the scaling has happened
            IF keyword_set(nozero) NE 0 THEN BEGIN 
              IF n_elements(nozero) EQ 1 THEN BEGIN
                zeros = where(smimage EQ 0,num_zeros)
                if ((num_zeros ne 0) and (keyword_set(cbottom))) then smimage[zeros] = 255
              ENDIF ELSE BEGIN 
                ;; user passed in an explicit list of beams to zero
                if ((num_zeros ne 0) and (keyword_set(cbottom))) then smimage[nozero] = 255
              ENDELSE
            ENDIF 
            
            ;; Note that we are now transforming the lon0/1 back to the
            ;; original coordinate system so that the smoothed image ends up
            ;; on the right place on the map.
          ;pause
            ;Note that I changed missing = missing to -1
            missing_loc=where(smimage eq 255,missing_num)
            if missing_num ne 0 then smimage[missing_loc]=0
            mimage = map_patch(smimage, lat0 = tglimits[1], lat1 = tglimits[3], $
                         lon0 = tglimits[0]-loncenter, $
                         lon1 = tglimits[2]-loncenter, $
                         xsize = xsize, ysize = ysize, $
                         xstart = xs, ystart = ys, missing = 255)
            ;help,missing
            mimage[where(mimage eq 255)]=0
              
              ;mimage = byte( mimage > 0 )

            ;pause

           ; This is the old code, but for some reason this caused errors.
           ; mimage = map_image(smimage, xs, ys, xsize, ysize, $
           ;                    latmin = tglimits[1], latmax = tglimits[3], $
           ;                    lonmin = tglimits[0]-loncenter, $
           ;                    lonmax = tglimits[2]-loncenter, $
           ;                    /bilin, compress = 1, scale = 0.06)
            ;pause

            top=255-3
            bottom=0
            tv, bytscl(mimage,top=(top-bottom),min=minvalue,max=maxvalue), xs, ys, xsize = xsize, ysize = ysize

            ;print,'kill here: '
            ;pause

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

        FOR i = 0L, n_elements(map)-1 DO BEGIN
            ;; special case for the pole pointing(s)
          IF (mlat[i] GT (90. - beamradius[i])) THEN BEGIN
            polyfill, a*!radeg, mlat[i]-beamradius[i], color = cimage[i]
          ENDIF ELSE IF (mlat[i] LT (-90 + beamradius[i])) THEN BEGIN
      ; South pole is causing strange behavior with beamradius=0.1 for WMAP
      ; can't figure out why, so comment out for now (ASH 2010-7-26)
;            polyfill, a*!radeg, mlat[i]+beamradius[i], color = cimage[i]
          ENDIF ELSE BEGIN 
            ;; plot the beams
            polyfill, $
              -mlon[i]+beamradius[i]*sin(a)/cos(mlat[i]/!radeg), $
              mlat[i]+beamradius[i]*cos(a), color = cimage[i],_extra = extra
            
            ;; print the values 
            IF (NOT keyword_set(multi) AND keyword_set(labels)) THEN BEGIN
              xyouts, -mlon[i], mlat[i], align = 0.5, $
                strtrim(string(image[i], format = '(F8.2)'), 2),color=254
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
      intarr(1000)+lat, line = 1, /noclip,color=254
    IF keyword_set(lat_labels) THEN begin
      
      ;; right or left
      if lat_labels eq 2 then $
        lonlab_loc = -(minlon-dlon*0.01*scale $
          / (!x.window[1]-!x.window[0]) / cos(lat/!radeg)) $
      else $
        lonlab_loc = -(maxlon+dlon*0.01*scale $
          / (!x.window[1]-!x.window[0]) / cos(lat/!radeg))

     lonlab_loc=lonlab_loc-double(!d.x_ch_size)/double(!d.x_size)*(!x.window[1]-!x.window[0])*double(glonmax-glonmin)*(charsize-1.0)/2.0

   if (abs(glatmax) ne abs(glatmin)) then begin ;If a full map, skip the lat=0 as the lon label to avoid lon and lat label occupying same space. 
      if abs(lat-round(lat)) lt 0.05 then $
      xyouts, lonlab_loc, lat, $
        font + strtrim(string(fix(round(lat))), 2), $
        align = (2 - lat_labels), charsize = charsize,color=254,charthick=charthick $
      else $
      xyouts, lonlab_loc, lat, $
        font + strtrim(string(fix(lat,type=4),format='(F6.1)'), 2), $
        align = (2 - lat_labels), charsize = charsize,color=254,charthick=charthick
    endif 
  endif
  ENDFOR 



  FOR lon = glonmax, glonmin, -glondel DO BEGIN
    IF NOT keyword_set(nogrid) THEN $
      oplot, intarr(1000)-lon, $
        minlat+lindgen(1000)*dlat/1000.0, $
        line = 1, /noclip,color=254
    IF keyword_set(lon_labels) THEN BEGIN 

      ;; top or bottom
      IF lon_labels eq 2 THEN $
        latlab_loc = maxlat+dlat*0.01/(!y.window[1]-!y.window[0]) $
      ELSE $
        latlab_loc = minlat-dlat*0.04*scale/(!y.window[1]-!y.window[0])

      lonnum = keyword_set(backwards) ? 360-lon : lon
      if keyword_set(radec) then $
         lontext=strtrim(string(lonnum/15.0, format = '(F6.2)'), 2) $
         else begin
              if abs(lon-round(lon)) lt 0.05 then begin
                  if keyword_set(magellanic) then begin
                     if lonnum gt 360. then lonnum=lonnum-360
                     lontext=strtrim(string(fix(round(lonnum))), 2) 
                  endif else $   
                  lontext=strtrim(string(fix(round(reduceto360(lonnum)))), 2) 
              endif else begin
                  if keyword_set(magellanic) then begin
                     if lonnum gt 360. then lonnum=lonnum-360
                     lontext=strtrim(string(fix(lonnum,type=4),format='(F6.1)'), 2)
                   endif else $
                  lontext=strtrim(string(fix(reduceto360(lonnum),type=4),format='(F6.1)'), 2)
              endelse
         endelse
 
     ;This subtracts the half of the char vertical size in data units from the char latitude location
     ;!d.y_ch_size*charsize - vertical size of char in pixels
     ;!d.y_size - vertical window size in pixels
     ;(!y.window[1]-!y.window[0]) - percentage of plot height used for plotting 
     ;This is done by calculating the  
     latlab_loc=latlab_loc-double(!d.y_ch_size)/double(!d.y_size)*(!y.window[1]-!y.window[0])*double(glatmax-glatmin)*(charsize-1.0)/2.0

     if abs(glatmax) ne abs(glatmin) then $
      xyouts, -lon, latlab_loc, $
        font + lontext, $
        align = 0.5, charsize = charsize,color=254,charthick=charthick,/data $   
     else $
      xyouts, -lon, 0, $
        font + lontext, $
        align = 0.5, charsize = charsize,color=254,charthick=charthick,/data  

    ENDIF 
  ENDFOR 

 

  IF NOT keyword_set(noframe) THEN BEGIN 
    ;; a nice border
    IF NOT keyword_set(nolonborder) THEN BEGIN 
      oplot, -maxlon+lindgen(1001)*dlon/1000.0, $
             intarr(1001)+(minlat + 1e-2), /noclip,color=254
      oplot, -maxlon+lindgen(1001)*dlon/1000.0, $
             intarr(1001)+(maxlat - 1e-2), /noclip,color=254
    ENDIF 
    IF NOT keyword_set(nolatborder) THEN BEGIN 
      oplot, intarr(1001)-(minlon + 1e-2), $
             minlat+lindgen(1001)*dlat/1000.0, /noclip,color=254
      oplot, intarr(1001)-(maxlon - 1e-2), $
             minlat+lindgen(1001)*dlat/1000.0, /noclip,color=254
    ENDIF 
  ENDIF 

if (n_elements(xytitles) eq 1) AND (NOT keyword_set(magellanic)) then begin

    xyouts,avg(!x.window),!y.window[0]*0.25,'Galactic Longitude (Degrees)',$
      charsize=charsize,charthick=charthick,/normal,align=0.5,color=254
    xyouts,!x.window[0]*0.5,avg(!y.window),'Galactic Latitude (Degrees)',$
      charsize=charsize,charthick=charthick,/normal,align=0.5,orientation=90,color=254

endif else if (n_elements(xytitles) eq 1)  AND (keyword_set(magellanic)) then begin

    xyouts,avg(!x.window),!y.window[0]*0.25,'Magellanic Stream Longitude (Degrees)',$
      charsize=charsize*1.3,charthick=charthick,/normal,align=0.5,color=254
    xyouts,!x.window[0]*0.5,avg(!y.window),'Magellanic Stream Latitude (Degrees)',$
      charsize=charsize*1.3,charthick=charthick,/normal,align=0.5,orientation=90,color=254

endif else if (n_elements(xytitles) eq 2) AND (size(xytitles,/type) eq 7) then begin

    xyouts,avg(!x.window),!y.window[0]*0.25,xytitles[0],$
      charsize=charsize*1.3,charthick=charthick,/normal,align=0.5,color=254
    xyouts,!x.window[0]*0.5,avg(!y.window),xytitles[1],$
      charsize=charsize*1.3,charthick=charthick,/normal,align=0.5,orientation=90,color=254

endif 

!p=p_str

END
