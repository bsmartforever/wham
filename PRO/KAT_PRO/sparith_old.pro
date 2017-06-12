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

FUNCTION get_shift, mapa, mapb, arange, brange, length, setlength=setlength, novar=novar

  dv = mapa.vel-shift(mapa.vel, 1)
  dv = avg(dv[1:*])
  
  pshift = dv EQ 0.0 ? 0 : round((mapa.vel[0]-mapb.vel[0])/dv)

  starta = -pshift > 0
  startb = pshift > 0
  enda = ((starta + (n_elements(mapb.vel) - pshift)) < n_elements(mapa.vel))-1
  endb = ((startb + (n_elements(mapa.vel) + pshift)) < n_elements(mapb.vel))-1
  length = min([[enda-starta+1], [endb-startb+1]])
  if keyword_set(samenum) then length = n_elements(mapa[0].vel)
  if keyword_set(setlength) then length=setlength
  arange = indgen(length)+starta
  brange = indgen(length)+startb

  return, pshift
END 

FUNCTION sparith, mapa, mapb, $
                  add = add, subtract = subtract, multiply = multiply, $
                  divide = divide, average = average, total = total, $
                  exclude = exclude, samenum=samenum, $
                  length=length, quiet=quiet

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

  if keyword_set(length) then setlength=length 
  if keyword_set(samenum) then setlength=n_elements(mapa[0].vel) 
  novara=0 & novarb=0 & novar=0
  novara=where(strcmp(tag_names(mapa),'var',/fold_case) eq 1)
  if (size(mapb,/type) ne 0) then varb=where(strcmp(tag_names(mapb),'var',/fold_case) eq 1)
  if ((novara eq -1) AND (novarb eq -1)) OR ((novara eq -1) AND (size(mapb,/type) eq 0)) then novar=1

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

    pshift = get_shift(mapa, mapb, arange, brange, len, setlength=setlength,novar=novar)

    if (NOT keyword_set(novar)) then begin
      out = { $
              name: string(''), $
              vel: fltarr(len), $
              data: fltarr(len), $
              var: fltarr(len), $
              glon: 0.0, $
              glat: 0.0 $
            }
    endif else begin
      out = { $
              name: string(''), $
              vel: fltarr(len), $
              data: fltarr(len), $
              glon: 0.0, $
              glat: 0.0 $
            }
    endelse

  ENDIF 

  SWITCH op OF 
    1: BEGIN 
      ;; add
      out.name = '(' + get_pname(mapa) + ') + (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] + mapb.data[brange]
      if (NOT keyword_set(novar)) then out.var = mapa.var[arange] + mapb.var[brange]
      break
    END 
    
    2: BEGIN 
      ;; subtract
      out.name = '(' + get_pname(mapa) + ') - (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] - mapb.data[brange]
      if (NOT keyword_set(novar)) then out.var = mapa.var[arange] + mapb.var[brange]
      out.glon = mapa.glon
      out.glat = mapa.glat
      break
    END 

    3: BEGIN 
      ;; multiply
      out.name = '(' + get_pname(mapa) + ') * (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] * mapb.data[brange]
      if (NOT keyword_set(novar)) then out.var = mapa.var[arange] + mapb.var[brange]
      break
    END 
  
    4: BEGIN 
      ;; divide
      out.name = '(' + get_pname(mapa) + ') / (' + get_pname(mapb) + ')'
      
      out.vel = mapa.vel[arange]
      out.data = mapa.data[arange] / mapb.data[brange]
      if (NOT keyword_set(novar)) then out.var = mapa.var[arange] + mapb.var[brange]
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

      if (NOT keyword_set(novar)) then begin
        out = { $
                name: 'TOTAL', $
                vel: mapa.vel, $
                data: mapa.data, $
                var: mapa.var, $
                n: 1, $
                glon: mapa.glon, $
                glat: mapa.glat $
              }
      endif else begin
        out = { $
                name: 'TOTAL', $
                vel: mapa.vel, $
                data: mapa.data, $
                n: 1, $
                glon: mapa.glon, $
                glat: mapa.glat $
              }
      endelse

      ENDIF ELSE BEGIN 
        
        ;; give up here since all spectra would be excluded. Instead,
        ;; print warning and include all of them
        IF n_elements(exclude) EQ n_elements(mapa) THEN BEGIN 
          exclude = -1
          message, "WARNING: All spectra requested to be excluded. Including all instead.", /info
        ENDIF 
        
        dv = mapa[0].vel-shift(mapa[0].vel, 1)
        dv = avg(dv[1:*])
        
        minv = min(mapa.vel)
        maxv = max(mapa.vel)
        len = ceil((maxv-minv)/dv) + 1
        if keyword_set(samenum) then len=n_elements(mapa[0].vel)   
        if keyword_set(length) then len=length     

        if (NOT keyword_set(novar)) then begin
          collect = { $
                      name: string(''), $
                      vel: findgen(len)*dv+minv, $
                      data: fltarr(len), $
                      var: fltarr(len), $
                      n: intarr(len) $
                    }
        endif else begin
          collect = { $
                      name: string(''), $
                      vel: findgen(len)*dv+minv, $
                      data: fltarr(len), $
                      n: intarr(len) $
                    }
        endelse
        

        ;initialize array
        ave_arr=fltarr(n_elements(collect.data),2)

        FOR i = 0, nsp-1 DO BEGIN 
          
          IF n_elements(exclude) NE 0 THEN $
            IF (where(exclude EQ i))[0] NE -1 THEN BEGIN
            CONTINUE 
          ENDIF 
          
          pshift = get_shift(collect, mapa[i], crange, mrange, len, setlength=setlength,novar=novar)

          collect.data[crange] = collect.data[crange] + mapa[i].data[mrange]
          if (NOT keyword_set(novar)) then collect.var[crange] = collect.var[crange] + mapa[i].var[mrange]
          collect.n[crange] = collect.n[crange] + 1

          if i eq 0 then begin
            data_arr=mapa[i].data[mrange]
            var_arr=mapa[i].var[mrange]
          endif else begin 
            data_arr=[[data_arr],[mapa[i].data[mrange]]]
            var_arr=[[var_arr],[mapa[i].data[mrange]]]
          endelse 

        ENDFOR 
        
        hasdata = where(collect.n NE 0)
        
        if (NOT keyword_set(novar)) then begin
          out = { $
                  name: 'TOTAL', $
                  vel: collect.vel[hasdata], $
                  data: collect.data[hasdata], $
                  var: collect.var[hasdata], $
                  n: collect.n[hasdata], $
                  glon: (where(tag_names(mapa) eq 'GLON') eq -1) ? 0 : avg(mapa.glon), $
                  glat: (where(tag_names(mapa) eq 'GLAT') eq -1) ? 0 : avg(mapa.glat) $
                }
        endif else begin
          out = { $
                  name: 'TOTAL', $
                  vel: collect.vel[hasdata], $
                  data: collect.data[hasdata], $
                  n: collect.n[hasdata], $
                  glon: (where(tag_names(mapa) eq 'GLON') eq -1) ? 0 : avg(mapa.glon), $
                  glat: (where(tag_names(mapa) eq 'GLAT') eq -1) ? 0 : avg(mapa.glat) $
                }
        endelse

      ENDELSE 
        
      flux=fltarr(n_elements(var_arr[*,0])) 
      err=fltarr(n_elements(var_arr[*,0])) 
      IF op EQ 5 THEN BEGIN 
        for i=0, n_elements(var_arr[*,0])-1 do begin
          good=where((data_arr[i,*] ne 0) AND (sqrt(var_arr[i,*]) ne 0) AND (finite(data_arr[i,*]) eq 1) AND (finite(var_arr[i,*]) eq 1),count)
          if count ne 0 then $
            ave_arr[i,*]=weight_ave(data_arr[i,good],sqrt(var_arr[i,good])) $
          else ave_arr[i,*]=[0,0]

          if count ne 0 then begin
            flux[i]=total(data_arr[i,good])/double(n_elements(good))
            err[i]=stddev(data_arr[i,good])/sqrt(double(n_elements(good)))
          endif $
          else ave_arr[i,*]=[0,0]
        endfor
        ;; average
        out.name = 'AVERAGE'

        ;pause

        ;This is just the average of the data. Note that out.data is basically just a sum of all the 
        ;spectra at each point with aligning x-positions. Now the sum of all the elements in the spectra
        ;is being divided by the number of spectra that were combined.
        ;See the collect.data[crange] = collect.data[crange] + mapa[i].data[mrange] code above
        out.data = ave_arr[hasdata,0];
        ;out.data = out.data/float(out.n)
        ;This is just the average-squared of the error at each x-position. So mathematically, 
        ;it is simply just the sum of the variences at each x-position divided by the square of the
        ;number of spectra-squared that were combined.
        if (NOT keyword_set(novar)) then out.var = (ave_arr[hasdata,1])^2.
        ;if (NOT keyword_set(novar)) then out.var = err[hasdata]^2./float(out.n)^2.0
        ;if (NOT keyword_set(novar)) then out.var = out.var/float(out.n)^2.0
      ENDIF 

      BREAK
    END 
  ENDSWITCH

  return, out
  
END 
