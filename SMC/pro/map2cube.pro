FUNCTION map2cube, map_in, outfile, lon_range=lon_range, lat_range=lat_range, $
            lon_delta=lon_delta, lat_delta=lat_delta, valid_vel=valid_vel, $
            radec=radec, lon180=lon180, cubein=cubein, usecubein=usecubein, $
            lon_ref=lon_ref, object=object, commentarr=commentarr, $
            reduceto100=reduceto100, vel_data=vel_data_out, $
            loncenter=loncenter
            
;+
;map2cube
;PURPOSE: regrid a WHAM map to a caree projection and output a FITS file
;SYNTAX: cube = map2cube(map_in, outfile)
;OPTIONAL KEYWORD INPUTS:
;   lon_range = lon_range: 2-element vector for longitude range
;   lat_range = lat_range: 2-element vector for latitude range
;   /valid_vel: only include velocities which are in every pixel
;       default: use all velocities, setting pixels with no velocity to NaN
;   /radec: output in RA, dec. Default: output in Galactic coordinates
;   cubein=cubein: an input cube to use
;   /usecubein: write out a FITS file with the input cube, instead of
;       projecting from map_in
;   lon_ref=lon_ref: longitude (or RA, if /radec) of reference pixel. 
;       Default: glon[0]
;   object=object: OBJECT keyword. Default: 'WHAM Northern Sky Survey - V 1.1'
;-

;  on_error, 2

  if n_elements(lon_delta) eq 0 then lon_delta = 0.5    ; will use -lon_delta
  if n_elements(lat_delta) eq 0 then lat_delta = 0.5

IF keyword_set(usecubein) THEN BEGIN
  out=cubein
  avg_dv = out.vel[1] - out.vel[0]
ENDIF ELSE BEGIN  
  map = map_in
  
  IF keyword_set(radec) THEN euler, map.glon, map.glat, mlon, mlat, 2 ELSE BEGIN
  	mlon=map.glon
  	mlat=map.glat
  ENDELSE
  IF keyword_set(lon180) THEN loncenter = 180.0
  IF keyword_set(loncenter) THEN mlon[where(mlon GT loncenter)] -= 360.0
  
  if n_elements(lon_range) eq 2 then $
    lon_min = min(lon_range, max=lon_max) $
  else $
    lon_min = min(mlon, max=lon_max)
    
  if n_elements(lat_range) eq 2 then $
    lat_min = min(lat_range, max=lat_max) $
  else $
    lat_min = min(mlat, max=lat_max)

  ;; remove stars
  
  wham_pro = strlen(getenv('WHAM_PRO')) GT 2 ? getenv('WHAM_PRO') : '/d/wham/pro'
  
  IF N_ELEMENTS(sao6) EQ 0 THEN restore, wham_pro + '/data/sao.dat'
  findstars, map, sao6, closemap, closestars, nclose
  fix_points = closemap[uniq(closemap, sort(closemap))]
  fix_points = closemap[where(closemap NE -1)]
  print, "Removing ", strtrim(N_ELEMENTS(fix_points), 2), " pointings."
  IF NOT array_equal(fix_points, -1) THEN remove, fix_points, map

  IF keyword_set(radec) THEN euler, map.glon, map.glat, mlon, mlat, 2 ELSE BEGIN
  	mlon=map.glon
  	mlat=map.glat
  ENDELSE
  
  IF keyword_set(lon180) THEN mlon[where(mlon GT 180)] -= 360.0

  IF keyword_set(reduceto100) THEN message, "ERROR: /reduceto100 doesn't work!"
  ;; figure out velocity span of output cube
  v = keyword_set(reduceto100) ? (map.vel)[33:-1, *] : map.vel
  v[where(v EQ 0.0)] = !values.F_NAN    ; v=0.0 values at end of vel array mess
  ;;                                    ; up avg_dv and velocity assignment
  avg_dv = mean((abs(v - shift(v, 1, 0)))[1:*, *], /nan)
  print, 'avg_dv: ', avg_dv, '; minmax: ', minmax( (abs(v - shift(v, 1, 0)))[1:-3, *])
  
;  if keyword_set(valid_vel) then begin
;    ;; only velocities that are in *every* pointing
;    minv = max(v[0, *])
;    maxv = min(v[-1, *])
;  endif else begin
    minv = min(v, max=maxv)
;  endelse

  ;; set output lon/lat grid
  out_glon = reverse(indgen(round((lon_max - lon_min)/lon_delta))*lon_delta + lon_min)
  out_glat = indgen(round((lat_max - lat_min)/lat_delta))*lat_delta + lat_min

  ;; set up output velocity channels
  nchan = round((maxv-minv)/avg_dv + 1)
  out_vel = indgen(nchan)*avg_dv + minv
  out_vel_mean = fltarr(nchan)
  out_vel_var = fltarr(nchan)
  
  cube = fltarr(N_ELEMENTS(out_glon), N_ELEMENTS(out_glat), nchan)
  vel_data_out = fltarr(nchan, n_elements(map)) - 1

  ;; for each pointing, find which out_vel element is closest to its first velocity element.
  ;;   value_locate returns an index (j) from out_vel that is closest, but smaller than map.vel[0].
  ;;   if the difference is more than half the out_vel spacing, we tweak the index up one.  

  start = reform(value_locate(out_vel, v[0,*]))
  v_diff = v[0,*] - out_vel[start]
  tweak = where(v_diff gt avg_dv/2.0, tcount)
  if tcount ne 0 then ++start[tweak]
  
  ;; initialize the input arrays for gridding
  data = fltarr(N_ELEMENTS(map))
  vel_data = data
  
  ;; some out-of-loop helpers
  glon = mlon
  glat = mlat
  vsize = total(finite(v), 1, /int)  ; number of real data points in each pointing
;  print, 'Calculating cube for ' + string(nchan, format='(I3)') + ' channels.'
  if keyword_set(valid_vel) then begin
    istart = value_locate(out_vel, max(v[0, *]))
    istop = value_locate(out_vel, min(v[-1, *]))
  endif else begin
    istart=0
    istop = nchan-1
  endelse
;  stop
  for i = istart, istop do begin
    print, i, nchan, format='($, "Channel", I4, "/", I4)'

    ;; 1) compute the offset into each pointing's velocity vector for this channel
    ;; 2) set elements where the offset is off either end of the spectrum to NAN
    ;; 3) populate input "map" with remaining good data
    
    offsets = i - start  

    oor = where((offsets lt 0) or (offsets gt vsize-1), ocount, $
                  complement = good, ncomplement = ngood)

    if ocount ne 0 then begin
      data[oor] = !values.F_NAN
      vel_data[oor] = !values.F_NAN
    endif

    if ngood ne 0 then begin
      for j = 0L, n_elements(good)-1 do begin
        gj = good[j]
        data[gj] = map[gj].data[offsets[gj]]
        vel_data[gj] = map[gj].vel[offsets[gj]]
      endfor
    endif
    
    x = glon
    y = glat

 ;   triangulate, x, y, t, sphere =  tsphere, /degrees, fvalue = data
 ;   channel = griddata(x, y, data, /linear, /sphere, /degrees, $
 ;                      /grid, xout = out_glon, yout = out_glat, $
 ;                      triangles = t, $
 ;                      missing = !values.f_nan)
      ;; search_ellipse = 2.0  is useful for other griddata algorithms

    triangulate, x, y, t, sphere =  tsphere, /degrees, fvalue = data
    channel = griddata(x, y, data, /inverse, power = 2, $
                        /sphere, /degrees, $
                        /grid, xout = out_glon, yout = out_glat, $
                        triangles = t, search_ellipse = 3, $
                        missing = !values.f_nan)
;    channel = griddata(x, y, data, /linear, $
;                        /sphere, /degrees, $
;                        /grid, xout = out_glon, yout = out_glat, $
;                        triangles = t, $
;                        missing = !values.f_nan)

    cube[*, *, i] = channel
    
    out_vel_mean[i] = mean(vel_data, /nan)
    out_vel_var[i] = variance(vel_data, /nan)
    vel_data_out[i,*] = vel_data
    print, ": ", strtrim(out_vel[i],2), $
        " (mean actual vel ", strtrim(out_vel_mean[i], 2), ")"
    
;    erase
;    tvscl, congrid(alog10(channel), $
;                   512, fix(512*N_ELEMENTS(out_glat)/N_ELEMENTS(out_glat)))

  endfor

  out = {cube: cube[*,*,istart:istop], glon: out_glon, glat: out_glat, $
     vel: out_vel[istart:istop], vel_mean:out_vel_mean[istart:istop], vel_var:out_vel_var[istart:istop]}
;  avg_dv = mean((abs(map_in.vel - shift(map_in.vel, 1, 0)))[1:*, *], /nan)

ENDELSE ;;; IF keyword_set(usecubein)
  

  if N_ELEMENTS(outfile) eq 1 then begin
    fxhmake, hdr, out.cube, /date, /init
    object = keyword_set(object) ? object : "WHAM Northern Sky Survey - V 1.1"
    fxaddpar, hdr, "OBJECT", object
    restwav = 656.3E-9
    IF tag_exist(map_in[0], 'fcenter') THEN IF (map_in[0].fcenter NE 0) THEN $
        restwav = map_in[0].fcenter * 1e-10
    print, 'restwav = ', restwav
    fxaddpar, hdr, "RESTWAV", restwav, 'rest wavelength (m)'
    fxaddpar, hdr, "BSCALE", 1.0
    fxaddpar, hdr, "BZERO", 0.0
    fxaddpar, hdr, "BUNIT", "mR (m s^-1)^-1", "milliRayleighs per m/s"
    fxaddpar, hdr, "VELREF", 1, "LSR reference frame"
    ctype = keyword_set(radec) ? ['RA---CAR','DEC--CAR']:['GLON-CAR','GLAT-CAR']

; ASH 2013-2-13: need to set longpole=0 for anything that's not a 
;   zenith projection. Also need reference latitude to be zero, so set that and
;       reference pixel accordingly.
;   See section 2.8 of Calabretta et al A&A 395, 1077
;   (also noted in the documentation to make_astr)
    ;lon_ref = keyword_set(lon_ref) ? lon_ref : 0.0
    lon_ref = n_elements(lon_ref) GT 0 ? lon_ref : out.glon[0]
    WHILE(lon_ref GT 360.0) DO BEGIN
        print, 'lon_ref = ', lon_ref, '; subtracting 360'
        lon_ref -= 360.0
    ENDWHILE
    WHILE(lon_ref LT 0.0) DO BEGIN
        print, 'lon_ref = ', lon_ref, '; adding 360'
        lon_ref += 360.0
    ENDWHILE
    lon_ref_pix = 0 + (lon_ref - out.glon[0])/(-lon_delta) + 1
    print, 'lon_ref_pix = ', lon_ref_pix, lon_ref, out.glon[0], -lon_delta
    lat_ref=0.0
    lat_ref_pix = 0 + (lat_ref - out.glat[0])/lat_delta + 1
;    make_astr, astr, crpix=[lon_ref_pix, lat_ref_pix], $
;        crval=[lon_ref, lat_ref], $
;        delt = [-lon_delta, lat_delta], ctype = ctype, longpole=0, latpole=90.0
    make_astr, astr, crpix=[lon_ref_pix, lat_ref_pix], $
        crval=[lon_ref, lat_ref], cd = [[-lon_delta, 0], [0, lat_delta]], $
        ctype = ctype, longpole=0, latpole=90.0

    putast, hdr, astr, cd_type=1
    fxaddpar, hdr, "CTYPE3", "VELO-LSR"
    fxaddpar, hdr, "SPECSYS", "LSRK"
    fxaddpar, hdr, "CRVAL3", out.vel[0]*1000.0
    fxaddpar, hdr, "CRPIX3", 1.0
    fxaddpar, hdr, "CDELT3", avg_dv*1000.0
    fxaddpar, hdr, "CUNIT1", "deg"
    fxaddpar, hdr, "CUNIT2", "deg"
    fxaddpar, hdr, "CUNIT3", "m/s"
    fxaddpar, hdr, "CROTA3", 0.0
    fxaddpar, hdr, "EQUINOX", 2000.0
    
    commentarr = keyword_set(commentarr) ? commentarr : [$
        "WHAM Northern Sky Survey - V 1.1", $
        "REFERENCE: Haffner, Reynolds, Tufte, Madsen, Jaehnig, &", $
        "Percival. 2003, ApJS, 149, 405. When citing these", $
        "data, please indicate that WHAM has been funded by a grant from the", $
        "National Science Foundation in your acknowledgments. Please visit"    , $
        "http://www.astro.wisc.edu/wham/ for updates, full documentation, and", $
        "further detail about these data."]
    
    FOR i=0, n_elements(commentarr) - 1 DO $
        fxaddpar, hdr, "COMMENT", commentarr[i]

    mwrfits, out.cube, outfile, hdr, /create
  endif

  return, out

END 
