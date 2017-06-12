@/Applications/exelis/idl/lib/graphics/colorbar.pro

function displaymap, map, value = value, $
  linear = linear, log = log, zmin = zmin, zmax = zmax, $
  top = top, bottom = bottom, rayleighs = rayleighs, $
  cbtitle = cbtitle, cbposition = cbposition, ct = ct, ctreverse = ctreverse, $
  no_horizon = no_horizon, no_grid = no_grid, no_labels = no_labels, $
  no_lonlabels = no_lonlabels, no_latlabels = no_latlabels, $
  current = current, $
  _ref_extra = ex_keys

  compile_opt idl2, logical_predicate

  ;; anything to do?
  if map.length le 1 then begin
    message, 'That''s not really a map...', /info
    return, !null
  endif

  ;; check if we should reuse a window
  if isa(current) then begin
    if isa(current, /number) then begin 
      if current ne 0 then begin
        ;; use current window
        w = getwindows(/current)
      endif
    endif else if isa(current, 'GRAPHIC') then begin
      w = current.window
    endif
    if isa(w) then begin
      w.erase
      current = w
    endif
  endif

  ;; Only pass on these inherited keywords to GRIDMAP
  ex_gridmap = ['vmin', 'vmax', 'delta', 'missing', 'method', 'min_points', 'search_ellipse']

  ;; Create a regularly gridded version first
  gridmap, map, mgrid, lon, lat, value = value, z = z, lon_full = lon_full, $
    _extra = ex_gridmap

  ;; Remove the GRIDMAP keywords from the full inherited list since 
  ;; the IMAGE object routine will complain below
  if isa(ex_keys) then begin 
    ex_image = list()
    foreach k, ex_keys do $
      if total(ex_gridmap.matches(k.tolower())) eq 0 then ex_image.add, k
    ex_image = ex_image.toArray(/no_copy)
  end
  
  ;; See if user wants auto-Rayleigh scaling--assumes input is in ADU/sec
  adu2r = 22.8    ; current best conversion value
  if keyword_set(rayleighs) then begin
    mgrid /= adu2r
    z /= adu2r
    if ~isa(cbtitle) then begin
      if keyword_set(log) then $
        cbtitle = 'log $\it I\rm$ [R]' $
      else if keyword_set(linear) then $
        cbtitle = '$\it I\rm$ [R]' $
      else cbtitle = 'HE $\it I\rm$ [R]'
      endif
  endif

  ;; Scale per use request or apply default HIST_EQUAL  

  ;; Set the display min/maxes if the user didn't specify
  mind = mgrid.min(max = maxd, /nan)
  if ~isa(zmax) then begin
    zmax = maxd
  endif
  if ~isa(zmin) then begin
    if keyword_set(log) then begin
      zmin = mind > min(mgrid[where(mgrid gt 0)])
    endif else begin
      zmin = mind
    endelse
  endif

  ;; Set range of final display image. By default, we avoid the top color since in
  ;; some colormaps it's the drawing color. 
  if ~isa(bottom) then bottom = 0b
  if ~isa(top) then begin
    if bottom ne 0 then $
      top = 255b - bottom $
    else $
      top = 254b
  endif
  if (top + bottom) gt 255 then $
    message, 'Byte image limits (TOP, BOTTOM) range greater than 255. Colormap wrapping may occur.', /info
  
  ;; scale and clip to limits
  if keyword_set(linear) then begin
    mimage = mgrid > zmin < zmax
;    mimage = bytscl(mgrid, min = zmin, max = zmax, top = top) + bottom
    range = [zmin, zmax]
  endif else if keyword_set(log) then begin
    mimage = mgrid > (mgrid.filter(lambda(n:n gt 0))).min()
    mimage = alog10(mgrid > zmin < zmax)
;    mimage = bytscl(alog10(mgrid > min(mgrid[where(mgrid gt 0)])), $
;      min = alog10(zmin), max = alog10(zmax), top = top) + bottom
    range = alog10([zmin, zmax])
  endif else begin
    nbins = 5000d
    binsize = (zmax - zmin) / nbins
    he_cpd = hist_equal(z, minv = zmin, maxv = zmax, /histogram_only)
    he_cpd_b = bytscl(he_cpd, top = top)
    mimage = he_cpd_b[((mgrid > zmin) - zmin) / binsize] + bottom
    range = [bottom, top]
  endelse

  ;; Prepare the colortable
  if ~isa(ct) then ct = 56    ; A red-temperature scale
  ctable = colortable(ct, reverse = (isa(ctreverse) && (ctreverse ne 0) ? 1 : 0))
  ctable = ctable[bottom:top,*]

  ;; And now generate the display

  ;; Create a flipped lon array to get the image orientation correct.
  ;; The coordinates are fixed using a custom axis display function below.

  lon_flip = 360 - lon

  ;; Some IDL restrictions for map projections we have to check/conform to:
  ;;   1) lon_min has to be in the +/- 180 range.
  ;;   2) lon_max has to be > lon_min (numerically); if it's not, we add 360.
  ;;      lon_max can range up to +540, thankfully...

  if ~lon_full then begin
    map_lon_min = lon_flip[0]
    map_lon_max = (lon_flip[-1] lt map_lon_min) ? lon_flip[-1] + 360 : lon_flip[-1] 
  endif else begin
    map_lon_min = 0.0
    map_lon_max = 360.0
  endelse
  map_limit = [lat[0], map_lon_min, lat[-1], map_lon_max]

  ii = image(mimage, lon_flip, lat, $
    map_projection = 'Stereo', $
    grid_units = 'degrees', $
    center_longitude = lon_flip[lon_flip.length / 2], $
    margin = [0, 0.2, 0, 0.1], $
    rgb_table = ctable, $
    limit = map_limit, $
;    longitude_min = lon_flip.min(), $
;    longitude_max = lon_flip.max(), $
;    latitude_min = lat.min(), $
;    latitude_max = lat.max(), $
    linestyle = 'solid', $
    color = 'light blue', $
    label_color = 'blue', $
    label_position = 0, $
    font_name = 'DejaVuSans', $
    font_size = 12, $
    label_show = (keyword_set(no_labels) ? 0 : 1), $
    label_format = 'mapgrid_gal_360flip_labels', $
    horizon_linestyle = (keyword_set(no_horizon) ? 'none' : 'solid'), $
    horizon_thick = 3, $
    clip = 0, $
    current = current, $
    _strict_extra = ex_image $
  )

  ii.mapprojection.mapgrid.transparency = 50
  ii.mapprojection.mapgrid['Longitudes'].label_angle = 0
  if keyword_set(no_grid) then ii.mapprojection.mapgrid.hide = 1
  if keyword_set(no_lonlabels) then ii.mapprojection.mapgrid['Longitudes'].label_show = 0
  if keyword_set(no_latlabels) then ii.mapprojection.mapgrid['Latitudes'].label_show = 0

  if ~isa(cbposition) then cbposition = [0.2, 0.1, 0.8, 0.125]

  ii_cb = colorbar(target = ii, $
;    range = range, $
    orientation = 0, $
    textpos = 0, $
    tickdir = 1, $
    border = 1, $
    position = cbposition, $
    title = cbtitle, $
    font_name = 'DejaVuSans', $
    font_size = 12, $
    taper = 1 $
  )

  ;; Fix up colorbar for histogram equalized scaling
  if ~keyword_set(log) && ~keyword_set(linear) then begin
    cb_values = ii_cb.tickvalues - bottom
    foreach bv, cb_values, i do begin
      lookup = (where(he_cpd_b ge bv))[0]
      cb_values[i] = lookup * binsize + zmin
    endforeach
    ii_cb.tickname = cb_values.toString("('$\it ',F0.2,'$')")
  endif
  
  ;; Store reference to colorbar in image's UVALUE
  
  ii.uvalue = ii_cb
  return, ii

end
