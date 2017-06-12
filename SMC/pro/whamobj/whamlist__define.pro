; docformat = 'rst rst'

;+
; Class for WHAM collection of pointings.
; 
; :Properties:
;
;
; :Author: Matt Haffner
;-

pro WHAMList::Resync

  compile_opt idl2, logical_predicate

;; These first two are marginally slower (~20%) for ~100 elements; not tested for larger collections yet 
;
;  t = systime(/sec)
;  if n_elements(self.glon) ne 0 then (self.glon).Remove, /all
;  if n_elements(self.glat) ne 0 then (self.glat).Remove, /all
;  foreach e, self do begin
;    self.glon.Add, e.glon
;    self.glat.Add, e.glat
;  endforeach
;  print, systime(/sec) - t
;  
;  t = systime(/sec)
;  if n_elements(self.glon) ne 0 then (self.glon).Remove, /all
;  if n_elements(self.glat) ne 0 then (self.glat).Remove, /all
;  a = self.ToArray()
;  for i=0, N_ELEMENTS(a)-1 do begin
;    self.glon.Add, a[i].glon
;    self.glat.Add, a[i].glat
;  endfor
;  print, systime(/sec) - t
  
;  t = systime(/sec)

;  if n_elements(self.glon) ne 0 then (self.glon).Remove, /all
;  if n_elements(self.glat) ne 0 then (self.glat).Remove, /all
;  l = fltarr(self.count())
;  b = l
;  foreach e, self, i do begin
;    l[i] = e.glon
;    b[i] = e.glat
;  endforeach
;  self.glon.Add, l, /extract
;  self.glat.Add, b, /extract

  l = fltarr(self.count())
  b = l
  foreach e, self, i do begin
    l[i] = e.glon
    b[i] = e.glat
  endforeach
  self.glon = list(l, /extract)
  self.glat = list(b, /extract)

;  print, systime(/sec) - t

end

pro WHAMList::Add, p, pointings = pointings, noresync = noresync, _extra = _extra

  compile_opt idl2, logical_predicate
  
  ;; Recurse over lists or an array of objects
  if isa(p, 'LIST') || isa(p, 'OBJREF', /array) then begin

    if keyword_set(pointings) || $
        isa(p, 'WHAMList') || $
        (obj_isa(p, 'WHAMPointing') eq N_ELEMENTS(p)) then $
      ;; Shortcut if we know the list or array contains only pointings
      self.LIST::Add, p, /extract, _extra = _extra $
    else $
      ;; This takes longer, but is safer
      foreach e, p do self.Add, e, /noresync, _extra = _extra

    self.Resync

    return
  endif

  ;; single pointing
  if isa(p, 'WHAMPointing', /scalar) then begin
    self.LIST::Add, p, _extra = _extra
    if ~keyword_set(noresync) then self.Resync
    return
  endif

  ;; otherwise, it's not something we can add
  message, 'Can''t add element of type ' + typename(p) + ' to a WHAMList.', /info

end

function WHAMList::Slice, minlon, maxlon, minlat, maxlat, $
                           radec = radec, count = count

  compile_opt idl2, logical_predicate

  glon = self.glon.ToArray()
  glat = self.glat.ToArray() 

  IF NOT keyword_set(radec) THEN BEGIN 
    ;; Do slice in Galatic space; if minlon/maxlon swapped, go across zero
    IF minlon LT maxlon THEN $
      index = where(glon GE minlon AND glon LE maxlon AND $
                    glat GE minlat AND glat LE maxlat, icount) $
    ELSE $
      index = where(glon GE minlon OR  glon LE maxlon AND $
                    glat GE minlat AND glat LE maxlat, icount)
  ENDIF ELSE BEGIN 
    ;; Do slice in Equatorial space
    euler, glon, glat, ra, dec, 2
    index = where(ra GE minlon*15 AND ra LE maxlon*15 AND $
                  dec GE minlat AND dec LE maxlat, icount) 
  ENDELSE  

  IF icount EQ 0 THEN BEGIN
    ;; nothing in those coordinate boundaries; return an empty group
    count = 0
    return, WHAMList()
  ENDIF ELSE BEGIN 
    count = n_elements(index)
    return, self[index]
  ENDELSE 
end

function WHAMList::Near, l, b, radius, count = count, dist = dist

  compile_opt idl2, logical_predicate

  ;; To make this faster, we reduce the number of viable pointings to
  ;; a box of glon, glat on a side. sphdist is only run on the
  ;; pointings in this much smaller box
  
  glon = self.glon.ToArray()
  glat = self.glat.ToArray() 
  
  w1 = where(b - radius LE glat AND $
             glat LE b + radius, count)
  IF count NE 0 THEN BEGIN 
    lonradius = radius/cos(b * !dtor)
    w2 = where(l - lonradius LE glon[w1] AND $
               glon[w1] LE l + lonradius, count)
    IF count NE 0 THEN BEGIN 
      dist = sphdist(glon[w1[w2]], glat[w1[w2]], l, b, /deg)
      w3 = where(dist LE radius, count)
    ENDIF
  ENDIF 

  IF count EQ 0 THEN $
    return, -1 $
  ELSE BEGIN 
    dist = dist[w3]
    return, w1[w2[w3]]
  ENDELSE 

end

pro WHAMList::FindStars, stars, closemap, closestars, nclose, closedist, $
                  window = window, verbose = verbose

  compile_opt idl2, logical_predicate

  IF ~isa(window) THEN window = 0.5

;  euler, map.glon, map.glat, ra, dec, 2
;
;  mindec = min(dec)
;
;  euler, stars.glon, stars.glat, ra, dec, 2
;  amindec = where(dec GE mindec)

  ;; Only work with a subset of the stars list, when possible
  glon = self.glon.ToArray()
  glat = self.glat.ToArray() 
  
  min_lon = min(glon, max = max_lon)
  min_lat = min(glat, max = max_lat)
  
  min_lat -= window
  max_lat += window
  lonfactor = 1/ cos(max(abs([min_lat, max_lat])) * !dtor)
  min_lon -= window * lonfactor
  max_lon += window * lonfactor
  
  subset = where(min_lat le stars.glat and stars.glat le max_lat and $
                 min_lon le stars.glon and stars.glon le max_lon, sub_count)

  if sub_count eq 0 then begin 
    ;; No bright stars within this collection
    nclose = 0
    closestars = -1
    closemap = -1
    closedist = -1
  endif 

  ss_stars = stars[subset]
  closestars = list()
  closemap = list()
  closedist = list()
;  nclose = intarr(n_elements(ss))

  if KEYWORD_SET(verbose) then print, 'Initial subset cut contains ', strtrim(sub_count, 2), ' stars.'

  FOREACH star, ss_stars, i DO BEGIN 

    beams = self.Near(star.glon, star.glat, window, count = n_close, dist = d_close)

    IF n_close NE 0 THEN BEGIN 
      closestars.Add, subset[i]   ; Index of star in input array
      closemap.Add, beams         ; Beam indices in the input map that the star affects
      closedist.Add, d_close      ; distance of the star from the center of each of these beams
      
;      closemap[i, 0:(ccnt-1 < 2)] = c[0:(ccnt-1 < 2)]
;      nclose[i] = ccnt
;      IF ccnt GT 3 THEN $
;        print, 'HD', strtrim(closestars[i].hd, 2), ' has ', $
;        strtrim(n_elements(c), 2), ' matches'

    ENDIF

    IF i MOD 1000 EQ 0 and KEYWORD_SET(verbose) THEN print, i

  ENDFOREACH

;  noclose = where(closemap[*, 0] EQ -1)
  
  if KEYWORD_SET(verbose) then $  
    print, strtrim(n_elements(ss_stars) - N_ELEMENTS(closestars), 2), ' stars did not match'

  ;; Pass back the index arrays of stars that matched a beam. This is a
  ;; bit sneaky--closestars is switch from the stellar array to an
  ;; index array
  IF N_ELEMENTS(closestars) EQ 0 THEN BEGIN 
    closestars = []
    return
  ENDIF ELSE BEGIN 
    closestars = closestars.ToArray()   ; convert to a simple array
    nclose = intarr(N_ELEMENTS(closestars))
    foreach cm, closemap, i do nclose[i] = N_ELEMENTS(cm)
  ENDELSE 

;  ;; find out some things about these pointings
;  dist = fltarr(n_elements(good), 3)
;  FOR i = 0, n_elements(closestars)-1 DO BEGIN 
;    FOR j = 0, nclose[i]-1 DO BEGIN 
;
;       dist[i, j] = sphdist(stars[closestars[i]].glon, $
;                           stars[closestars[i]].glat, $
;                           map[closemap[i, j]].glon, $
;                           map[closemap[i, j]].glat, /deg)
;    ENDFOR
;  ENDFOR 

END   

function WHAMList::Integrate, vmin, vmax

  data = fltarr(self.count())

  ;; No velocity range specified; use full integration
  if N_PARAMS() eq 0 then begin
    foreach p, self, i do data[i] = p.int
  endif else if N_PARAMS() eq 2 then begin
    foreach p, self, i do data[i] = p.Integrate(vmin, vmax)
  endif else begin
    message, 'Incorrect number of arguments; pass 2 or none.', /info
    return, !null
  endelse
  
  return, data

end

;; Can't just use Map here since LIST now has that method!
function WHAMList::SkyMap, vmin, vmax, zmin = zmin, zmax = zmax

  compile_opt idl2, logical_predicate

  lon = self.glon.ToArray()
  lat = self.glat.ToArray()
  data = fltarr(self.count())
  
  ;; No velocity range specified; use full integration
  if N_PARAMS() eq 0 then begin
    foreach p, self, i do data[i] = p.int
  endif else if N_PARAMS() eq 2 then begin
    foreach p, self, i do data[i] = p.Integrate(vmin, vmax)
  endif else begin
    message, 'Incorrect number of arguments; pass 2 or none.', /info
    return, !null
  endelse

  ;; need to "flip" longitude to get correct sky orientation; transform with (360 - lon)
  ;;   to avoid problems with LIMIT lonmin restrction of > -180
  fliplon = lambda(l: (360 - l))

  padding = keyword_set(padding) ? padding : 3
  minlat = floor(min(lat) - padding) > (-90)
  maxlat = ceil(max(lat) + padding) < (+90)
  
  ;; note the min/max sense is reversed with lon orientation flipping
  maxlon = fliplon(reduceto360(floor(min(lon) - padding) > 0))
  minlon = fliplon(reduceto360(ceil(max(lon) + padding) < 360))

  IF n_elements(loncenter) EQ 0 THEN BEGIN
    loncenter = avg([minlon, maxlon])
  ENDIF else begin
    loncenter = fliplon(loncenter)
  endelse

  m = map('Hammer', $
    font_size = 12, $
    limit = [minlat, minlon, maxlat, maxlon], $
    center_longitude = loncenter, $
    color = 'gray', $
    linestyle = 'dot', $
    label_color = 'black', $
    label_position = 0, $
    label_format = 'mapgrid_gal_360flip_labels', $
    horizon_thick = 1, $
    margin = [0.04,0,0.14,0], $
    xtitle = 'Galactic Longitude', $
    ytitle = 'Galactic Longitude')
  
  if ~isa(zmin) then zmin = min(data)
  if ~isa(zmax) then zmax = max(data)
  
  data_scaled = bytscl(data, max = zmax, min = zmin)
  tvlct, colors, /get
  
  ;; "circle"
  a = findgen(20)*(!pi*2/20.)
  a = [a, a[0]]
  
  points = list()
  m.refresh, /disable
  tic
  
; polygons are slightly slower than ellipse in NG (~10%)
; 
;  for i = 0, N_ELEMENTS(data_scaled)-1 do $
;    !null = polygon( $
;        lon[i]+0.5*sin(a)/cos(lat[i] * !dtor), $
;        lat[i]+0.5*cos(a), /data, /fill_background, $
;        LINESTYLE= ' ', $
;        fill_color = reform(colors[data_scaled[i], *])  )

  e_lon = fliplon(lon)

; ELLIPSE is still fairly slow... 300 pointings take about 7 sec on a 2011 i7 iMac
;  
;  e_major = 0.5/cos(lat * !dtor)
;  e_color = transpose(colors[data_scaled, *])
;  for i = 0, N_ELEMENTS(data_scaled)-1 do $
;    points.add, ELLIPSE(e_lon[i], lat[i], $
;      minor = 0.5, major = e_major[i], /data, $
;      linestyle = ' ', fill_color = e_color[*, i])

; overplot is fast, but the points don't follow the projection, if it's changed
;   since we have to do the xy->uv mapping here: PLOT() doesn't use map projection
;   for some reason. Also, can't get non-circular beams using symbols.
;   
  xy = m.MapForward(e_lon, lat)
  m_plot = plot(xy, vert_colors = data_scaled, rgb_table = 3, $
      symbol = "o", linestyle = ' ', /sym_filled, /over)
  
  toc
  m.refresh

  return, m

end

function WHAMList::GetParam, param

  compile_opt idl2, logical_predicate

  ;; make sure we have a valid key first; for now we make the 
  ;;   big assumption that the pointing have identical header
  ;;   entries. We can do this per element, but it will get
  ;;   a bit more expensive.
  if ~((self[0]).params).HasKey(param) then begin
    message, 'Key "' + param + '" not found in these pointings.', /info
    return, !null
  endif

  ;; we don't know what type will be returned by this 
  ;;  key, so use list to build it, then allow it 
  ;;  to return the auto-typed array.
  plist = list()
  foreach e, self do plist.Add, (e.params)[param]
  return, plist.ToArray()
  
end

pro WHAMList::GetProperty, name = name, $
  glon = glon, glat = glat, $
  p_names = p_names, p_shortnames = p_shortnames, $
  ndp = ndp, vlsr = vlsr, vgeo = vgeo, $
  day = day, time = time, block = block, pointing = pointing, int = int

  compile_opt idl2, logical_predicate

  name = self.name

  ;; these are cached in the WHAMList to be faster
  if ARG_PRESENT(glon) then glon = self.glon.ToArray()
  if ARG_PRESENT(glat) then glat = self.glat.ToArray()

  ;; these are generated on the fly
  if ARG_PRESENT(p_names) then begin
    p_names = strarr(self.count())
    foreach e, self, i do p_names[i] = e.name
  endif

  if ARG_PRESENT(p_shortnames) then begin
    p_shortnames = strarr(self.count())
    foreach e, self, i do p_shortnames[i] = e.shortname
  endif

  if ARG_PRESENT(ndp) then begin
    ndp = intarr(self.count())
    foreach e, self, i do ndp[i] = e.ndp
  endif

  if ARG_PRESENT(vlsr) then begin
    vlsr = fltarr(self.count())
    foreach e, self, i do vlsr[i] = e.vlsr
  endif
  
  if ARG_PRESENT(vgeo) then begin
    vgeo = fltarr(self.count())
    foreach e, self, i do vgeo[i] = e.vgeo
  endif

  if ARG_PRESENT(day) then begin
    day = lonarr(self.count())
    foreach e, self, i do day[i] = e.day
  endif

  if ARG_PRESENT(time) then begin
    time = fltarr(self.count())
    foreach e, self, i do time[i] = e.time
  endif

  if ARG_PRESENT(block) then begin
    block = intarr(self.count())
    foreach e, self, i do block[i] = e.block
  endif

  if ARG_PRESENT(pointing) then begin
    pointing = intarr(self.count())
    foreach e, self, i do pointing[i] = e.pointing
  endif

  if ARG_PRESENT(int) then begin
    int = self.Integrate()
  endif
  
end

pro WHAMList::SetProperty, name = name
  compile_opt idl2, logical_predicate

  self.name = name
end

FUNCTION WHAMList::_overloadBracketsRightSide, isRange, $
   Subscript1, Subscript2, Subscript3, Subscript4, $
   Subscript5, Subscript6, Subscript7, Subscript8

  compile_opt idl2, logical_predicate

  if (N_PARAMS() eq 2) and (isRange[0] eq 0) then begin
    if isa(Subscript1, 'STRING') then begin
      ;; GetParam shorthand
      return, self.GetParam(Subscript1)
    endif else if isa(Subscript1, /ARRAY) then begin
      ;; We can do this much faster than LIST since we know
      ;;   our elements are all the same type, so we can
      ;;   utilize standard IDL array slicing.
      
      a = self.ToArray()
      return, WHAMList(a[Subscript1], /extract, /pointings)

;     return, WHAMList(self.Get(position = Subscript1), /extract, /pointings)
 
      ;; should add code to handle ranges too, but make sure this works first...
            
    endif
  endif

  ;; otherwise LIST handles it
  return, self.LIST::_overloadBracketsRightSide( isRange, Subscript1 , Subscript2, Subscript3, Subscript4, Subscript5, Subscript6, Subscript7, Subscript8 )

end    

;+
; :Description:
;    Output user-friendly info for HELP on a WHAMPointing object
;
; :Params:
;    myName : in, required, type=string
;      variable name for the object
;
; :Author: Matt Haffner
;-
;function WHAMList::_overloadHelp, myName
;
;  return, string(FORMAT='(%"%-15s WHAMList[%d]: \"%s\"")', $
;      myName, n_elements(self.pointings), self.name)
;
;end

;+
; :Description:
;    User-friendly PRINT output for WHAMPointing objects.
;    Testing only for now.
;
; :Author: Matt Haffner
;-
;function WHAMList::_overloadPrint

;  foreach tag_names(self), field do begin
;    help, self, output = out
;  out = 'Success: '

;  return, out

;end

function WHAMList::Init, p, name = name, _extra = _extra

  compile_opt idl2, logical_predicate

  if self.LIST::Init() then begin 

    if isa(name, 'STRING') then self.name = name else name = ''
    
    self.glon = list()
    self.glat = list()
    
    ;; Add pointings if user passed a parameter
    if isa(p) then self.add, p, _extra = _extra
  
    return, 1
  
  endif else return, 0
  
end

pro WHAMList__define
    
  compile_opt idl2, logical_predicate

  on_error, 2
  if double(!version.release) lt 8.4 then message, 'Sorry, requires IDL version 8.4 or above.'
  
  struct = {WHAMList, $
            name: string(''), $
            glon: list(), glat: list(), $
            inherits LIST $
          }

end