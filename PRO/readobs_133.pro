FUNCTION readobs, name, dir, archive = archive, survey = survey, ext = ext, $
                  notime = notime, count = count

  jdcnv, 1997, 1, 1, 0, day0

  datadir = dir
  if n_elements(ext) eq 0 then ext = 'RAWSPEC'
  
  IF keyword_set(archive) THEN BEGIN 
    datadir = '/d/wham/archive/' + dir
;      IF keyword_set(sii) THEN datadir = datadir + '/sii' $
;      ELSE IF keyword_set(nii) THEN datadir = datadir + '/nii' $
;      ELSE datadir = datadir + '/ha'
  ENDIF

  IF keyword_set(survey) THEN BEGIN 
    datadir = '/d/wham/survey/' + dir
;      IF keyword_set(sii) THEN datadir = datadir + '/sii' $
;      ELSE IF keyword_set(nii) THEN datadir = datadir + '/nii'
  ENDIF 
  
  IF n_elements(notime) EQ 0 THEN BEGIN 
    files = file_search(datadir + '/combo/' + $
                        name + '-[0-9][0-9][0-9][0-9][0-9][0-9].fts', $
                        count = count)
  ENDIF ELSE BEGIN 
    files = file_search(datadir + '/combo/' + $
                        name + '*', $
                        count = count)
  ENDELSE 

  IF count NE 0 THEN BEGIN

    obs = replicate({observation, $
                     name: string(''), $
                     vel: fltarr(133), $
                     data: fltarr(133), $
                     var: fltarr(133), $
                     glon: float(1), $
                     glat: float(1), $
                     vlsr: float(1), $
                     zd: float(1), $
                     az: float(0), $
                     date: string(''), $
                     day: long(0), $
                     time: float(0), $
                     pacmd: float(0), $
                     pbcmd: float(0), $
                     pamon: float(0), $
                     pbmon: float(0), $
                     ccdtemp: float(0), $
                     etemp: float(0) $
                    }, count)

    FOR i = 0, count-1 DO BEGIN 

      spectname = files[i]
      
      readfspe, spectname, v, d, s, btheader, ext = ext

      obs[i].name = spectname
      
      ;; store velociy, data, and variance
      obs[i].vel(*) = v
      obs[i].data(*) = d
      obs[i].var(*) = s

      ;; read header of primary HDU
      get_lun, lun
      openr, lun, spectname 
      fxhread, lun, pheader
      close, lun
      free_lun, lun

      ;; extract coordinate infomation from header
      obs[i].glon = fxpar(pheader, 'dgal-lon')
      obs[i].glat = fxpar(pheader, 'dgal-lat')
      IF !err EQ -1 THEN BEGIN 
        IF NOT keyword_set(quiet) THEN $
          print, '  Demanded coordinates not found; using actual'
        obs[i].glon = fxpar(pheader, 'gal-lon')
        obs[i].glat = fxpar(pheader, 'gal-lat')
      ENDIF ELSE BEGIN
        act_glon = fxpar(pheader, 'gal-lon')
        act_glat = fxpar(pheader, 'gal-lat')
        IF NOT keyword_set(quiet) THEN $
          print, '  Starting pointing error (arcmin):', $
                 sphdist(act_glon, act_glat, obs[i].glon, obs[i].glat, /deg)*60.0
      ENDELSE 

      ;; fix negative longitudes
      IF obs[i].glon LT 0 THEN obs[i].glon = 360 + obs[i].glon
      
      ;; extract extra parameters
      obs[i].vlsr = fxpar(pheader, 'vlsr')
      obs[i].zd = fxpar(pheader, 'zenith_d')

      hourang = fxpar(pheader, 'ha')
      dec = fxpar(pheader, 'dec')
      olat = fxpar(pheader, 'obs-lat')
      hdtoaa, hourang*15.0, dec, olat, alt, az, /deg
      obs[i].az = az*!radeg

      obs[i].date = fxpar(pheader, 'date-obs')
      obs[i].day = parsedate(obs[i].date, /fits) - day0
      
      timestr = fxpar(pheader, 'time-obs')
      timeparts = str_sep(timestr, ":")
      obs[i].time = timeparts[0]+timeparts[1]/60.0+timeparts[2]/3600.0
      
      obs[i].pacmd = fxpar(pheader, 'pacmd')
      obs[i].pbcmd = fxpar(pheader, 'pbcmd')
      obs[i].pamon = fxpar(pheader, 'pamon')
      obs[i].pbmon = fxpar(pheader, 'pbmon')
      obs[i].ccdtemp = fxpar(pheader, 'ccdtemp')
      obs[i].etemp = fxpar(pheader, 'temp4')
    ENDFOR

    return, obs

  ENDIF ELSE BEGIN
    message, 'No files found', /info
  ENDELSE 
END 