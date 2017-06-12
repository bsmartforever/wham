PRO curspect, map, map2, map2scale = map2scale, apclip = apclip, $
              intrange = intrange, image = image, radec = radec, $
              hi1 = hi1, hi2 = hi2, radius = radius, $
              im_window = im_window, sp_window = sp_window, _extra = e

;+
;NAME: curspect
;SYNTAX: curspect, map [,map2] [,map2scale=map2scale] [,intrange=intrange] $
;   [,image=image] [,/radec] [,/hi1] [,/hi2] [radius=radius]
;
;INPUTS:
;   map - map to click on
;OPTIONAL INPUTS:
;   map2 - map to show spectra of when directions are clicked
;-

; 2005-8-15 ASH added radius keyword (specify radius within which to search)

  if n_elements(map2scale) eq 0 then map2scale = 1.0
  IF keyword_set(hi1) THEN rad1 = 0.25 ELSE rad1 = 0.5
  IF keyword_set(hi2) THEN rad2 = 0.25 ELSE rad2 = 0.5
  if n_elements(im_window) eq 0 then im_window = !d.window else im_window = im_window[0]
  if n_elements(sp_window) eq 0 then sp_window = 9
  
  IF keyword_set(radius) THEN BEGIN
	  rad1 = radius
	  rad2 = radius
  ENDIF

  IF n_elements(image) EQ 0 THEN BEGIN 
    wset_or_create, sp_window
    wset, im_window
    wshow, im_window, 1
    xs0 = !x
    ys0 = !y
  ENDIF 

  print, 'Left button plots, Middle button overplots, Right button exits'

  done = 0

  WHILE NOT done DO BEGIN 
    cursor, lon, lat, /down

    done = (!mouse.button AND 4) EQ 4
    over = (!mouse.button AND 2) EQ 2

    IF NOT done THEN BEGIN
      IF lon LE 0 THEN lon = -lon ELSE lon = 360-lon
      IF keyword_set(radec) THEN $
        euler, lon, lat, lon, lat, 1
      
      near = spectnear(map, lon, lat, rad1, ncnt)

      ;; mapslice returns 0, a scalar if no spectra are near
      IF ncnt EQ 0 THEN BEGIN
        print, 'No spectra within ' + strtrim(rad1, 2) + ' degrees of click'
      ENDIF ELSE BEGIN 
        
        print, ncnt, ' pointing(s) near, using element ' $
          + strtrim(near[0], 2) 
        
        IF NOT keyword_set(hi1) THEN BEGIN 
          bpos = rstrpos(((map[near[0]])).name, 'b')
          shname = strmid(((map[near[0]])).name, bpos + 1)
          dotpos = rstrpos(shname, '.')
          
          IF dotpos NE -1 THEN $
            shname = strmid(shname, 0, dotpos)
            
          var = ((map[near[0]])).var
        ENDIF ELSE BEGIN 
          shname = 'Element ' + strtrim(near[0], 2) 
          var = ((map[near[0]])).data * 0
        ENDELSE 
        
        IF n_elements(image) NE 0 THEN BEGIN 
          print,  'Image value for ' + shname + ' = ' + $
            strtrim(image[near[0]], 2)
        ENDIF ELSE BEGIN 
          IF n_elements(intrange) EQ 2 THEN $
            print, 'Intensity (' + strtrim(intrange[0], 2) + ' - ' + $
            strtrim(intrange[1], 2) + ') = ' + $
            strtrim(intspect((map[near[0]]), intrange[0], $
                             intrange[1]), 2) + $
;                             intrange[1])/684.1, 2) + $
            ' R'
          
          wset, sp_window
          IF over THEN BEGIN
            !x = xs1
            !y = ys1
            IF ~keyword_set(hi1) and keyword_set(apclip) THEN begin 
              vpmin = max((map[near[0]]).vel) - 203
              velslice = where((map[near[0]]).vel GE vpmin, vcount)
              
              oplot, ((map[near[0]]).vel)[velslice], ((map[near[0]]).data)[velslice]
            endif ELSE begin
              ploterror, (map[near[0]]).vel, (map[near[0]]).data, $
                sqrt(var), /nohat, /over, /traditional
            endelse
          ENDIF ELSE BEGIN 
            title = 'Block ' + shname $
              + ' (l=' + strtrim(string(((map[near[0]])).glon), 2) $
              + ' b=' + strtrim(string(((map[near[0]])).glat), 2) + ')'

            IF ~keyword_set(hi1) and keyword_set(apclip) THEN begin 
              vpmin = max(((map[near[0]])).vel) - 203
              velslice = where(((map[near[0]])).vel GE vpmin, vcount)

              plot, (((map[near[0]])).vel)[velslice], (((map[near[0]])).data)[velslice], $
                    title = title, $
                    xstyle = 3, ystyle = 3,  $
                    _extra = e
            endif  ELSE begin
              ploterror, ((map[near[0]])).vel, ((map[near[0]])).data, $
                    sqrt(var), /nohat, $
                    title = title, traditional = 1, $
                    xstyle = 3, ystyle = 3, $
                    _extra = e
            endelse
             
            xs1 = !x
            ys1 = !y
          ENDELSE 

          IF n_elements(map2) NE 0 THEN BEGIN
            near = spectnear(map2, lon, lat, rad2, ncnt)
            
            ;; mapslice returns 0, a scalar if no spectra are near
            IF ncnt EQ 0 THEN BEGIN
              print, 'No spectra from map #2 within ' + strtrim(rad2, 2) $
                     + ' degrees of click'
            ENDIF ELSE BEGIN 
              print, ncnt, ' pointing(s) in map #2 near, using element ' $
                + strtrim(near[0], 2) 

              IF ~keyword_set(hi2) and keyword_set(apclip) THEN begin 
                vpmin = max(map2[near[0]].vel) - 203
                velslice = where(map2[near[0]].vel GE vpmin, vcount)
              
                oplot, map2[near[0]].vel[velslice], $
                  map2[near[0]].data[velslice]*map2scale, $
                  color = color(90)
              endif else begin 
                oplot, map2[near[0]].vel, map2[near[0]].data*map2scale, $
                  color = color(90)
              ENDELSE 
            endelse 
          endif 

          wset, im_window

          !x = xs0
          !y = ys0

        ENDELSE 
      ENDELSE 
    ENDIF 
  ENDWHILE

END 
