pro gridmap, map, outgrid, xc, yc, z = z, $
  vmin = vmin, vmax = vmax, value = value, $
  delta = delta, dimension = dimension, start = start, $
  missing = missing, method = method, _extra = ex_keys
  
  if ~isa(vmin) then vmin = map.vel.min()
  if ~isa(vmax) then vmax = map.vel.max()

  if ~isa(method) then method = 'NaturalNeighbor'
  if ~isa(missing) then missing = !values.f_nan
  
  if ~isa(value) then value = intmap(map, vmin=vmin, vmax=vmax, moment=0)
  
  if value.length ne map.length then begin
    message, 'VALUE must be the same length as MAP.'
    return
  endif

  x = map.glon
  y = map.glat
  z = value

  ;grid_input, x, y, f, xyz, new_f, /sphere, /degrees, epsilon=0.1
  ;lon = !radeg * atan(xyz[1,*],xyz[0,*])
  ;lat = !radeg * asin(xyz[2,*])

  lon=x
  lat=y
  qhull, lon, lat, triangles, sphere = tsphere, /delaunay
  ;triangulate, lon, lat, triangles, /degrees, sphere=tsphere, fvalue=z

  grid_min = [min(x), min(y)]
  grid_max = [max(x), max(y)]

  if ~isa(start) then begin
    start = grid_min
  endif else begin
    if start.ndim eq 0 then begin
      start = [start, start]
    endif else if start.length ne 2 then begin
      message, 'START can only contain at most 2 elements; returning', /info
      return
    endif
  endelse

  if ~isa(delta) then begin
    delta = [0.25, 0.25]
  endif else begin
    if delta.ndim eq 0 then begin
      delta = [delta, delta]
    endif else if delta.length ne 2 then begin
      message, 'DELTA can only contain at most 2 elements; returning', /info
      return
    endif
  endelse
  
  if ~isa(dimension) then begin
    dimension = ceil((grid_max - start) /delta)    
  endif else begin
    if dimension.ndim eq 0 then begin
      dimension = [dimension, dimension]
    endif else if dimension.length ne 2 then begin 
      message, 'DIMENSION can only contain at most 2 elements; returning', /info
      return
    endif
  endelse

  xc = indgen(dimension[0]) * delta[0] + start[0]
  yc = indgen(dimension[1]) * delta[1] + start[1]

  outgrid = griddata(lon, lat, z, /sphere, /degrees, $
    tri = triangles, missing = missing, $
    start = start, delta = delta, dimension = dimension, $
    method = method, _extra = ex_keys)

;  zvar = variance(z)

;  outgrid = griddata(lon, lat, z, /kriging, /sphere, /degrees, $
;    tri = triangles, min_points = 4, search_ellipse = 5, missing = 0, $
;    start = grid_min, delta = delta, dimension = dim, $
;    variogram = [2, 4.0, 0.0, zvar])

;  outgrid = krig2d(z, lon, lat, spherical = [4.0, 0.0], $
;    bounds = [xc.min(), yc.min(), xc.max(), yc.max()], nx = xc.length, ny = yc.length) 

  outgrid = reverse(outgrid, 1, /overwrite)
  xc = reverse(xc, /overwrite)

  print, z.min(), z.max(), outgrid.min(), outgrid.max()    
end
