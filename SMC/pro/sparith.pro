FUNCTION get_pname, pointing
  sname = tag_names(pointing, /struct)
  IF strpos(sname, 'POINTING') NE -1 THEN BEGIN 
    name = string(pointing.glon, format = '(F6.2)') + ', ' + $
      string(pointing.glat, format = '(F6.2)')
  ENDIF ELSE BEGIN 
    name = pointing.name
  ENDELSE 

  return, name
END 

FUNCTION get_shift, mapa, mapb, arange, brange, length

  avel = mapa.vel[where(finite(mapa.vel))]
  bvel = mapb.vel[where(finite(mapb.vel))]

  dv = avel-shift(avel, 1)
  dv = avg(dv[1:*], /nan)
  
  pshift = dv EQ 0.0 ? 0 : round((avel[0]-bvel[0])/dv)

  starta = -pshift > 0
  startb = pshift > 0
  enda = ((starta + (n_elements(bvel) - pshift)) < n_elements(avel))-1
  endb = ((startb + (n_elements(avel) + pshift)) < n_elements(bvel))-1
  length = min([[enda-starta+1], [endb-startb+1]])

  arange = indgen(length)+starta
  brange = indgen(length)+startb

  return, pshift
END 

FUNCTION sparith, mapa, mapb, $
                  add = add, subtract = subtract, multiply = multiply, $
                  divide = divide, average = average, total = total, $
                  exclude = exclude, verbose = verbose

  IF keyword_set(add) THEN op = 1 ELSE $
    IF keyword_set(subtract) THEN op = 2 ELSE $
    IF keyword_set(multiply) THEN op = 3 ELSE $
    IF keyword_set(divide) THEN op = 4 ELSE $
    IF keyword_set(average) THEN op = 5 ELSE $
    IF keyword_set(total) THEN op = 6 ELSE BEGIN
    print, 'Please specify at least one operation: '
    print, '  /add, /subtract, /multiply, /divide, /average, /total'
    return, 0
  ENDELSE 

  ;; constant operations
  IF n_params() GT 1 AND size(mapb, /type) NE 8 THEN BEGIN 
    IF op GT 4 THEN BEGIN
      message, 'Can''t /average or /total on constants!', /info
      return, mapa
    ENDIF 
    out = mapa
    CASE op OF
      1: out.data = out.data + mapb
      2: out.data = out.data - mapb
      3: out.data = out.data * mapb
      4: out.data = out.data / mapb
    ENDCASE 
    return, out
  ENDIF

  ;; check and setup single pointing operations
  IF op LE 4 THEN BEGIN 
    IF n_elements(exclude) NE 0 THEN $
      message, $
      'Note: EXCLUDE only used with /average or /total; parameter ignored', $
      /info

    IF n_elements(mapa) NE 1 OR n_elements(mapb) NE 1 THEN BEGIN
      print, 'This operation can only be performed on single pointings'
      return, 0
    ENDIF 

    pshift = get_shift(mapa, mapb, arange, brange, len)
;    print, pshift

    out = { $
            name: string(''), $
            vel: fltarr(len), $
            data: fltarr(len), $
            var: fltarr(len), $
            glon: 0.0, $
            glat: 0.0 $
          }
  ENDIF 

  SWITCH op OF 
    1: BEGIN 
      ;; add
      out.name = '(' + get_pname(mapa) + ') + (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] + mapb.data[brange]
      out.var = mapa.var[arange] + mapb.var[brange]
      break
    END 
    
    2: BEGIN 
      ;; subtract
      out.name = '(' + get_pname(mapa) + ') - (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] - mapb.data[brange]
      out.var = mapa.var[arange] + mapb.var[brange]
      out.glon = mapa.glon
      out.glat = mapa.glat
      break
    END 

    3: BEGIN 
      ;; multiply
      out.name = '(' + get_pname(mapa) + ') * (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] * mapb.data[brange]
      out.var = mapa.var[arange] + mapb.var[brange]
      break
    END 
  
    4: BEGIN 
      ;; divide
      out.name = '(' + get_pname(mapa) + ') / (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] / mapb.data[brange]
      out.var = mapa.var[arange] + mapb.var[brange]
      out.glon = mapa.glon
      out.glat = mapa.glat
      break
    END 

    5:
    6: BEGIN 
      ;; total
      IF n_params() GT 1 THEN $
        message, 'Note: only first parameter used for this operation', /info
      nsp = n_elements(mapa)

      IF nsp EQ 1 THEN BEGIN 

        out = { $
                name: 'TOTAL', $
                vel: mapa.vel, $
                data: mapa.data, $
                var: mapa.var, $
                n: 1, $
                glon: mapa.glon, $
                glat: mapa.glat $
              }

      ENDIF ELSE BEGIN 
        
        ;; give up here since all spectra would be excluded. Instead,
        ;; print warning and include all of them
        IF n_elements(exclude) EQ n_elements(mapa) THEN BEGIN 
          exclude = -1
          message, "WARNING: All spectra requested to be excluded. Including all instead.", /info
        ENDIF 
        
        dv = mapa[0].vel-shift(mapa[0].vel, 1)
        dv = avg(dv[1:*], /nan)
        
        minv = min(mapa.vel)
        maxv = max(mapa.vel)
        len = ceil((maxv-minv)/dv) + 1
        
        collect = { $
                    name: string(''), $
                    vel: findgen(len)*dv+minv, $
                    data: fltarr(len), $
                    var: fltarr(len), $
                    n: intarr(len) $
                  }
        
        FOR i = 0, nsp-1 DO BEGIN 
          
          IF n_elements(exclude) NE 0 THEN $
            IF (where(exclude EQ i))[0] NE -1 THEN BEGIN
              if keyword_set(verbose) then print, 'Skipping ', i
            CONTINUE 
          ENDIF 
          
          pshift = get_shift(collect, mapa[i], crange, mrange, len)
          if keyword_set(verbose) then print, i, pshift, len
          
          collect.data[crange] = collect.data[crange] + mapa[i].data[mrange]
          collect.var[crange] = collect.var[crange] + mapa[i].var[mrange]
          collect.n[crange] = collect.n[crange] + 1
        ENDFOR 
        
        hasdata = where(collect.n NE 0)
        
        out = { $
                name: 'TOTAL', $
                vel: collect.vel[hasdata], $
                data: collect.data[hasdata], $
                var: collect.var[hasdata], $
                n: collect.n[hasdata], $
                glon: (where(tag_names(mapa) eq 'GLON') eq -1) ? 0 : avg(mapa.glon), $
                glat: (where(tag_names(mapa) eq 'GLAT') eq -1) ? 0 : avg(mapa.glat) $
              }
      ENDELSE 
        
      IF op EQ 5 THEN BEGIN 
        ;; average
        out.name = 'AVERAGE'
        out.data = out.data/float(out.n)
        out.var = out.var/float(out.n)^2.0
      ENDIF 

      BREAK
    END 
  ENDSWITCH

  return, out
  
END 
