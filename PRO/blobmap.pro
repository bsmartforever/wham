PRO blobmap, map, vcenter, vwidth, min, max, multi = multi, title = title, $
             nolabels = nolabels, he = he, log = log, zmax = zmax, $
             beamsonly = beamsonly

  ;; parse params
  IF NOT keyword_set(he) THEN BEGIN
      IF keyword_set(log) THEN BEGIN
          IF NOT keyword_set(zmax) THEN BEGIN
              zmax = max(map.data)
          ENDIF
      ENDIF ELSE BEGIN
          he = 1
      ENDELSE
  ENDIF 

  ;; get the limits of the map; add a little extra for the beam size
  minlat = floor(min(map(*).glat) - 1)
  maxlat = ceil(max(map(*).glat) + 1)
  minlon = floor(min(map(*).glon) - $
                 1 / cos((abs(maxlat) > abs(minlat))/!radeg))
  maxlon = ceil(max(map(*).glon) + $
                1 / cos((abs(maxlat) > abs(minlat))/!radeg))

  ;; if multi is set (normally only set by 'blobmapall') then include
  ;; the advance keyword and add a border for the colorbar
  IF keyword_set(multi) THEN BEGIN 
      map_set, 0, avg([-maxlon, -minlon]), /ait, $
        limit = [minlat, -maxlon, maxlat, -minlon], $
        /noborder, /advance, xmargin = [0, 15], /iso, $
        title = title
  ENDIF ELSE BEGIN
      map_set, 0, avg([-maxlon, -minlon]), /ait, $
        limit = [minlat, -maxlon, maxlat, -minlon], $
        /noborder, /iso, $
        title = title
  ENDELSE      
  
  ;; This array hold the shape of the beam, i.e. a circle
  a = findgen(50)*(!pi*2/50.)
  a = [a, a(0)]

  IF keyword_set(beamsonly) THEN BEGIN
      FOR i = 0, n_elements(map)-1 DO BEGIN
          oplot, -map(i).glon+0.5*sin(a)/cos(map(i).glat/!radeg), $
            map(i).glat+0.5*cos(a)
      ENDFOR 
  ENDIF ELSE BEGIN 
      ;; These arrays hold the map that will be displayed
      image = fltarr(n_elements(map))
      logimage = image
      
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

      ;; print, 'Beam ' + string(i) + ' used ' + string(vcount) + ' data points'
      ENDFOR

      ;; Create the color scaled image:
      ;; Only the top 95% of the color table is used to avoid problems
      ;; with maps that have a drawing color at the top (e.g. 39 & 40)
      ;;
      ;; Other options that seem to work well here are:
      ;;bimage = hist_equal(logimage, top = !d.table_size*.95)

      IF keyword_set(he) THEN BEGIN 
          bimage = hist_equal(image, top = !d.table_size*.98)
      ENDIF ELSE BEGIN 
          bimage = bytscl(logimage <  alog10(zmax), top = !d.table_size*.98)
      ENDELSE 

      IF !d.n_colors GT 256 THEN BEGIN
          cimage = (1 + 256L + 256L * 256L) * bimage
      ENDIF ELSE BEGIN
          cimage =  bimage
      ENDELSE 
      
      FOR i = 0, n_elements(map)-1 DO BEGIN
          ;; plot the beams
          polyfill, -map(i).glon+0.5*sin(a)/cos(map(i).glat/!radeg), $
            map(i).glat+0.5*cos(a), color = cimage(i)
          
          ;; print the values 
          IF (NOT keyword_set(multi) AND NOT keyword_set(nolabels)) THEN BEGIN
              xyouts, -map(i).glon, map(i).glat, align = 0.5, $
                strtrim(string(image(i), format = '(F8.2)'), 2)
          ENDIF
      ENDFOR 
  ENDELSE 
  
;  IF keyword_set(multi) THEN BEGIN 
;      map_set, 0, avg([-maxlon, -minlon]), /ait, $
;        limit = [minlat, -maxlon, maxlat, -minlon], $
;        /noborder, /advance, xmargin = [0, 15], /iso
;  ENDIF 
;  ENDIF ELSE BEGIN
;      map_set, 0, avg([-maxlon, -minlon]), /ait, $
;        limit = [minlat, -maxlon, maxlat, -minlon], $
;        /noborder, /iso
;  ENDELSE      

  ;; plot the axes labels and the grid
  gridspacing, minlat, maxlat, glatmin, glatmax, glatdel
  gridspacing, minlon, maxlon, glonmin, glonmax, glondel

  scale = 1/float((1 > !p.multi(1) > !p.multi(2))) * 1.0
  
  FOR lat = glatmin, glatmax, glatdel DO BEGIN
      oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, intarr(1000)+lat, $
        line = 1
      xyouts, -(maxlon+(maxlon-minlon)*0.01 $
                / (!x.window(1)-!x.window(0)) / cos(lat/!radeg)), $
        lat, '!6' + strtrim(string(fix(lat)), 2), $
        align = 1.0, charsize = scale
  END 

  FOR lon = glonmax, glonmin, -glondel DO BEGIN
      oplot, intarr(1000)-lon, minlat+indgen(1000)*(maxlat-minlat)/1000.0, $
        line = 1
      xyouts, -lon, minlat-(maxlat-minlat)*0.03/(!y.window(1)-!y.window(0)), $
        '!6' + strtrim(string(fix(lon)), 2), $
        align = 0.5, charsize = scale
  END 

  ;; a nice border
  oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, intarr(1000)+minlat
  oplot, -maxlon+indgen(1000)*(maxlon-minlon)/1000.0, intarr(1000)+maxlat
  oplot, intarr(1000)-minlon, minlat+indgen(1000)*(maxlat-minlat)/1000.0
  oplot, intarr(1000)-maxlon, minlat+indgen(1000)*(maxlat-minlat)/1000.0
  
  ;; if the user wants the min/max values of the constructed image
  ;; back, set them up
  IF n_params() EQ 5 THEN BEGIN
      min = min(image)
      max = max(image)
  END
  
END
