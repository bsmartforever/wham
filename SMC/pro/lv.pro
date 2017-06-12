;; lv.pro - LON/LAT vs. velocity diagrams

;; 12/21/09: removed hacks for 132 data point arrays (LMH)

compile_opt idl2

;FUNCTION lonfullticks, axis, index, value
;  lon = value-60
;  IF lon LE 0 THEN lon = 360+lon
;  IF lon EQ 360 THEN lon = 0
;  return, string(lon, format = '(I3)')
;END 

PRO lv, map, slice, width = width, lat = latvs, lonvs = lonvs, $
        norm = norm, stripsz = stripsz, log = log, title = title, $
        smooth = smooth, vres = vres, cres = cres, image = image, $
        bad = bad, full = full, lti = lti, lmi = lmi, $
        missing = missing, zmin = zmin, zmax = zmax, cbottom = cbottom, $
        vrange = vrange, crange = crange, noerase = noerase, $
        xgrid = xgrid, ygrid = ygrid, _extra = extra

  IF NOT keyword_set(latvs) AND NOT keyword_set(lonvs) THEN BEGIN
    message, 'Please specify either /LAT or /LON for a LAT vs. VEL or VEL vs. LON plot', /info
    return
  ENDIF

  ;; Take a 2 degree swath in the other axis if not specified
  IF NOT keyword_set(width) THEN width = 1

  ;; Default resolutions for smooth version
  IF NOT keyword_set(vres) THEN vres = 4
  IF NOT keyword_set(cres) THEN cres = 0.5
  dv = 2.0

  ;; make 0.8 degree high strips if not specified
  IF NOT keyword_set(stripsz) THEN stripsz = 0.8
  
  ;; background color
  IF n_elements(missing) EQ 0 THEN BEGIN 
    IF !d.name EQ 'PS' THEN BEGIN
      missing = byte(!d.table_size-1)
    ENDIF ELSE BEGIN
      missing = byte(!p.background)
    ENDELSE
  ENDIF 

  ;; PostScript has scalable pixels
  sp = !d.flags AND 1
  
  IF keyword_set(latvs) THEN BEGIN

    realwidth = width/cos(map.glat/!radeg)  ; Lon window correction
    mslice = where(map.glon GT slice-realwidth AND map.glon LT slice+realwidth)
    mapsl = map[mslice]
    IF n_elements(bad) NE 0 THEN badsl = bad[mslice]

    if n_elements(crange) ne 0 then begin
      mslice = where(mapsl.glat ge crange[0] and mapsl.glat le crange[1])
      mapsl = mapsl[mslice]
      IF n_elements(bad) NE 0 THEN badsl = badsl[mslice]
    endif

    coord = mapsl.glat
    vel = mapsl.vel
    data = mapsl.data
    
    ctitle = 'Latitude'
    otitle = '!12l!3'
    cstep = 0.85

  ENDIF ELSE BEGIN
    mslice = where(map.glat GT slice-width AND map.glat LT slice+width)
    mapsl = map[mslice]
    IF n_elements(bad) NE 0 THEN badsl = bad[mslice]

    if n_elements(crange) ne 0 then begin
      mslice = where(mapsl.glon ge crange[0] and mapsl.glon le crange[1])
      mapsl = mapsl[mslice]
      IF n_elements(bad) NE 0 THEN badsl = badsl[mslice]
    endif

    coord = mapsl.glon
    vel = mapsl.vel
    data = mapsl.data

    stripsz = stripsz/cos(slice*!dtor)
 
    ctitle = 'Longitude'
    otitle = '!18b!3'
    cstep = 0.98/cos(slice*!dtor)

  ENDELSE 
  
  ;; intensity chop
  IF NOT keyword_set(cbottom) THEN cbottom = 0
  IF n_elements(zmin) EQ 0 THEN zmin = 0.01
  IF n_elements(zmax) EQ 0 THEN zmax = max(data) 

  print, zmin, zmax, max(data)

  ;; title
  if n_elements(title) eq 0 then begin
    title = ctitle + ' vs. Velocity at ' + otitle + ' = ' + $
            strtrim(string(slice, format = '(F6.2)'), 2) + ' (' + $
            strtrim(string(width*2, format = '(F6.2)'), 2) + $
            ' degrees average)'
  endif 
    
  minc = min(coord)
  maxc = max(coord)

  if n_elements(vrange) eq 0 then begin 
    minv = min(vel)
    maxv = max(vel)
  endif else begin
    minv = vrange[0]
    maxv = vrange[1]
  endelse 

  IF (n_elements(full) ne 0) THEN BEGIN 
    IF keyword_set(lonvs) THEN BEGIN 
      minc = 0
      maxc = 360
      if n_elements(lti) then yti = lti else xti = 30
      if n_elements(lmi) then ymi = lmi else xmi = 3
;      xtf = 'lonfullticks'
      xtf = lambda('a,i,v: (reduceto360(v - (180 - ' + full.ToString() + '))).ToString("(I3)")')
    ENDIF ELSE BEGIN 
      minc = -90
      maxc = +90
    ENDELSE 
  ENDIF ELSE BEGIN
    if N_ELEMENTS(crange) ne 0 then begin
      minc = crange[0]
      maxc = crange[1]
    endif
  ENDELSE

  IF keyword_set(smooth) THEN BEGIN
    maxc = floor((maxc-minc)/cres)*cres+minc
    maxv = floor((maxv-minv)/vres)*vres+minv
    crange = [minc-cres, maxc+cres]
    vrange = [minv-vres, maxv+vres]
  ENDIF ELSE BEGIN 
    crange = [minc-2, maxc+2]
    vrange = [minv-2, maxv+2]
  ENDELSE 
  
  if keyword_set(lonvs) then begin
    xax = coord
    yax = vel
    xrange = reverse(crange)
    yrange = vrange
    xtitle = 'Galactic ' + ctitle
    ytitle = 'LSR Velocity (km/s)'
  endif else begin
    xax = vel
    yax = coord
    xrange = vrange
    yrange = crange
    xtitle = 'LSR Velocity (km/s)'
    ytitle = 'Galactic ' + ctitle
  endelse
  
  plot, xax, yax, /nodata, xstyle = 5, ystyle = 5, $
        yrange = yrange, xrange = xrange, title = title, noerase = noerase
  
  ;; compute the scaling facters if needed below for non-PS devices
  px = !x.window * !d.x_vsize
  py = !y.window * !d.y_vsize
  ;;  sx = px[1] - px[0] + 1
  ;;  sy = py[1] - py[0] + 1
  xscale = abs((px[1]-px[0])/(!x.crange[1]-!x.crange[0]))
  yscale = abs((py[1]-py[0])/(!y.crange[1]-!y.crange[0]))
  
  IF keyword_set(smooth) THEN BEGIN 

    ;; average spectra with similar coordinates and pad out the ends
    ;; to avoid trigrid streaking0.85/cos(slice*!radeg)
    ngrid = ceil((maxc-minc)/cstep)
    cgrid = indgen(ngrid)*cstep+minc

    FOR i = 0, ngrid-1 DO BEGIN 
      bunch = where(cgrid[i]-cstep/2.0 LE coord AND $
                    coord LE cgrid[i]+cstep/2.0, bcount)
      IF bcount NE 0 THEN BEGIN 
        IF n_elements(bad) NE 0 THEN BEGIN 
          exclude = where(badsl[bunch] EQ 1)
        ENDIF ELSE BEGIN 
          exclude = -1
        ENDELSE 
        
        savg = sparith(mapsl[bunch], /average, exclude = exclude)

        ;; remove bogus NAN points added from spectral ends, if needed
        keep = where(finite(savg.vel, /nan) eq 0, keep_count)
        sx = savg.vel[keep]
        sy = sx * 0 + cgrid[i]
        sd = savg.data[keep]
        
        x = (i EQ 0) ? sx : [x, sx]
        y = (i EQ 0) ? sy : [y, sy]
        d = (i EQ 0) ? sd : [d, sd]

        ;; now pad
        IF min(sx) GT minv THEN BEGIN
          npad = fix((savg.vel[0]-minv)/dv) + 1
          pad = -(indgen(npad)*dv+1) + savg.vel[0]

          x = [x, pad]
          y = [y, pad*0 + cgrid[i]]
          d = [d, pad*0 + (-999)]          
        ENDIF 
        
        IF max(sx) LT maxv THEN BEGIN
          npad = fix((maxv - max(sx))/dv) + 1
          pad = (indgen(npad)*dv + 1) + max(sx)
          
          x = [x, pad]
          y = [y, pad*0 + cgrid[i]]
          d = [d, pad*0 + (-999)]          
        ENDIF 
       
      ENDIF ELSE BEGIN 
        ;; no matches at this cgrid point. Fill with zeros (assume we
        ;; are outside the survey boundaries)

        npad = fix((maxv-minv)/dv)
        pad = indgen(npad)*dv + minv

        x = (i EQ 0) ? pad : [x, pad]
        y = (i EQ 0) ? pad*0 + cgrid[i] : [y, pad*0 + cgrid[i]]
        d = (i EQ 0) ? pad*0 + (-999) : [d, pad*0 + (-999)]
      ENDELSE 

    ENDFOR 
      
;    x = reform(vel, n_elements(vel))
;    y = reform((intarr(n_elements(vel[*, 0]))+1)#coord, n_elements(vel)) 
;    d =  data

    ;; if full requested for lv, transform to that longitude in the center (180)
    IF (n_elements(full) ne 0) AND keyword_set(lonvs) THEN BEGIN 
      y = y + (180 - full)
      y = reduceto360(y)
;      big = where(y GT 360)
;      y[big] = y[big]-360
    ENDIF

    triangulate, x, y, triangles    

    fixup = where(d EQ -999, fcount)
    IF keyword_set(log) THEN BEGIN 
      dd = alog10(d > zmin <  zmax)
    ENDIF ELSE BEGIN 
      dd = d > zmin < zmax
    ENDELSE   

;    IF fcount THEN smin = min(dd[where(d NE -999)]) ELSE smin = min(dd)
;    print, smin
    if keyword_set(log) then begin 
      smin = alog10(zmin)
      smax = alog10(zmax)
    endif else begin 
      smin = zmin
      smax = zmax
    endelse 

    bd = bytscl(dd, min = smin, max = smax, $
                top = !d.table_size-2-cbottom) + cbottom
    IF fcount NE 0 THEN bd[fixup] = missing
   
    image = byte(round(trigrid(x, y, bd, triangles, $
                          [vres, cres], [minv, minc, maxv, maxc], $
                          missing = missing, xgrid = xgrid, ygrid = ygrid)) $
      > 0 < byte(!d.table_size-1))

    if keyword_set(lonvs) then image = rotate(image, 1)
      
    IF sp THEN BEGIN 
      if keyword_set(latvs) then begin
        tv, image, minv - vres/2.0, minc - cres/2.0, $
          xsize = maxv-minv, ysize = maxc-minc, /data
      endif else begin
        tv, image, maxc + cres/2.0, minv - vres/2.0, $
          xsize = -(maxc-minc), ysize = maxv-minv, /data
      endelse
    ENDIF ELSE BEGIN 
      if keyword_set(latvs) then begin
        sx = round((maxv-minv)*xscale) + 1
        sy = round((maxc-minc)*yscale) + 1
        lx = round((minv - !x.crange[0] - vres/2.0)*xscale) + px[0]
        ly = round((minc - !y.crange[0] - cres/2.0)*yscale) + py[0]
      endif else begin
        sx = round((maxc-minc)*xscale) + 1
        sy = round((maxv-minv)*yscale) + 1
        lx = round((maxc - !x.crange[0] - cres/2.0)*xscale) + px[0]
        ly = round((minv - !y.crange[0] - vres/2.0)*yscale) + py[0]
      endelse

      tv, congrid(image, sx, sy), lx, ly
    ENDELSE 
    
  ENDIF ELSE BEGIN   ;; non-smoothed
  
    IF NOT keyword_set(log) THEN BEGIN
      maxd = max(data < zmax)
      mind = min(data > zmin)
    ENDIF ELSE BEGIN
      maxd = max(alog10(data > zmin < zmax))
      mind = min(alog10(data > zmin))
    ENDELSE 

    FOR i = 0, n_elements(mapsl)-1 DO BEGIN
      real_data = mapsl[i].data[where(finite(mapsl[i].data))]
      strip = bytarr(n_elements(real_data), 2)
      
      if KEYWORD_SET(norm) then begin 
        strip[*,0] = bytscl(real_data)
      endif else begin 
        IF NOT keyword_set(log) THEN BEGIN
          strip[*, 0] = bytscl(real_data, min = mind, max = maxd)
        ENDIF ELSE BEGIN
          strip[*, 0] = bytscl(alog10(real_data > 1e-5), $
                               min = mind, max = maxd)
        ENDELSE
      endelse
              
      strip[*, 1] = strip[*, 0]
      if keyword_set(lonvs) then strip = transpose(strip)

      sv = max(mapsl[i].vel) - min(mapsl[i].vel)
      sc = stripsz
      lv = min(mapsl[i].vel[0])
      lc = coord[i] - stripsz/2.0
;if mapsl[i].glat le 0 then stop
      sx = (keyword_set(lonvs) ? sc : sv)
      sy = (keyword_set(lonvs) ? sv : sc)
      lx = (keyword_set(lonvs) ? lc : lv)
      ly = (keyword_set(lonvs) ? lv : lc)
        
      IF ~sp THEN BEGIN
        sx = round(sx * xscale) > 1
        sy = round(sy * yscale) > 1
;        lx = round((lx - !x.crange[0]) * xscale + px[0])
;        ly = round((ly - !y.crange[0]) * yscale + py[0])
      ENDIF

      if sp then begin
        tv, strip, lx, ly, xsize = sx, ysize = sy, /data
      endif else begin
        tv, congrid(strip, sx, sy), lx, ly, /data
      endelse
      
      
    ENDFOR
  ENDELSE 

  ;; put axes on now
  
  
  if keyword_set(lonvs) and (n_elements(full) ne 0) then begin
    plot, xax, yax, /nodata, xstyle = 1, ystyle = 1, /noerase, $
      yrange = yrange, xrange = xrange, $
      xtickinterval = xti, xminor = xmi, xtickformat = xtf, $
      xtitle = xtitle, ytitle = ytitle, $
      _extra = extra
  endif else begin
    plot, xax, yax, /nodata, xstyle = 1, ystyle = 1, /noerase, $
      yrange = yrange, xrange = xrange, $
      xtitle = xtitle, ytitle = ytitle, $
      _extra = extra
  endelse

END 
