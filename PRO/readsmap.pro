PRO readsmap, basename, map, novlsr = novlsr

  ;; get a sorted list of map spectra
  sort_cmd = 'ls '+basename+'_*.spe | sort -n -t _ -k 2'
  spawn, sort_cmd, sort_result

  ;; extract the last filename
  last_one = sort_result(n_elements(sort_result)-1)

  ;; extract the maximum x and y values from the last filename
  last_slash = rstrpos(basename, "/")
  last_pos = rstrpos(last_one, "_")
  lastdotpos = rstrpos(last_one, ".")

  IF last_slash EQ -1 THEN BEGIN
      base = basename
      dir = ''
  END ELSE BEGIN
      base = strmid(basename, last_slash+1, 999)
      dir = strmid(basename, 0, last_slash+1)
  END 
  
  reads, strmid(last_one, last_pos+1, lastdotpos-last_pos), maxx
  
  map = replicate({pointing, $
          name: string(''), $
          vel: fltarr(133), $
          data: fltarr(133), $
          var: fltarr(133), $
          glon: float(1), $
          glat: float(1) $
        }, maxx)

  
  FOR i = 0, fix(maxx)-1 DO BEGIN
      map(i).name = basename

      ;; construct current filename to read
      filename = base + "_" + strtrim(string(i+1), 1)
      spectname =  dir + filename + ".spe"
      print, 'Reading file '+ filename 
      readspe, spectname, v, d, s

      ;; store velociy, data, and variance
      map(i).vel(*) = v
      map(i).data(*) = d
      map(i).var(*) = s

      ;; make sure associated files exist
      fitsname = dir + '../merged/' + filename + '.fts'
      paramname = dir + 'params/' + filename + '.par'
      garb = findfile(fitsname, count = ftsct)
      IF ftsct EQ 0 THEN BEGIN 
          message, 'FITS file not found: no coordinate information or VLSR correction', /info
      ENDIF ELSE BEGIN 
          temp = readfits(fitsname, header, /silent)
          map(i).glon = fxpar(header, 'dgal-lon')
          map(i).glat = fxpar(header, 'dgal-lat')
          IF !err EQ -1 THEN BEGIN 
              print, '  Demanded coordinates not found; using actual'
              map(i).glon = fxpar(header, 'gal-lon')
              map(i).glat = fxpar(header, 'gal-lat')
          ENDIF ELSE BEGIN
              act_glon = fxpar(header, 'gal-lon')
              act_glat = fxpar(header, 'gal-lat')
              print, '  Starting pointing error (arcmin):', $
                sphdist(act_glon, act_glat, map(i).glon, map(i).glat, /deg)*60.0
          ENDELSE 
          
          ;; fix negative longitudes
          IF map(i).glon LT 0 THEN map(i).glon = 360 + map(i).glon
          
          garb = findfile(paramname, count = prmct)
          IF prmct EQ 0 AND NOT keyword_set(novlsr) THEN BEGIN
              message, 'PARAM file not found: no VLSR correction', /info
          ENDIF ELSE BEGIN
              IF NOT keyword_set(novlsr) THEN BEGIN
                  ;; extract vlsr shift
                  vlsr_off = fxpar(header, 'vlsr')
                  
                  ;; extract geocoronal fit parameters
                  line = ''
                  get_lun, unum
                  openr, unum, paramname
                  readf, unum, line
                  close, unum
                  free_lun, unum
                  geomean = float(strmid(line, 8, 9))
                  
                  ;; vlsr correction 
                  map(i).vel(*) = v - geomean - 2.33 - vlsr_off
              ENDIF
          ENDELSE
      ENDELSE 
  ENDFOR
  
END
  
