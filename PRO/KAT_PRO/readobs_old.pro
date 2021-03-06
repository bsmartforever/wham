FUNCTION readobs, name, dir, archive = archive, survey = survey, $
                  ftsext = ftsext, notime = notime, count = count, $
                  extended = extended, files = files, nocombo = nocombo,quiet=quiet

;; 2007-10-18: Added files keyword -- provide an array of the files to read 
;;	(ASH)

  jdcnv, 1997, 1, 1, 0, day0

  IF n_elements(files) EQ 0 THEN datadir = dir
  if n_elements(ftsext) eq 0 then ftsext = 'RAWSPEC'
  
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
  
  IF n_elements(files) GT 0 THEN count = n_elements(files) ELSE $
  IF n_elements(notime) EQ 0 THEN BEGIN 
    files = file_search(datadir + '/combo/' + $
                        name + '-[0-9][0-9][0-9][0-9][0-9][0-9].fts', $
                        count = count)
  ENDIF ELSE BEGIN 
    if keyword_set(nocombo) then begin 
      files = file_search(datadir + '/' + $
                        name + '*', $
                        count = count)
    endif else begin 
      files = file_search(datadir + '/combo/' + $
                        name + '*', $
                        count = count)
    endelse
      
  ENDELSE 

  IF count NE 0 THEN BEGIN

    ;; peek and see how big the spectra are
    h = headfits(files[0], exten = ftsext)
    sp_size = fxpar(h, 'NAXIS2')
  
    obs = replicate({ $
                     name: string(''), $
                     vel: fltarr(sp_size), $
                     data: fltarr(sp_size), $
                     var: fltarr(sp_size), $
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
      
      readfspe, spectname, v, d, s, btheader, ext = ftsext

      obs[i].name = spectname
      
      ;; store velociy, data, and variance
      IF n_elements(v) GT n_elements(obs[i].vel) THEN BEGIN 
        range_max = n_elements(v)-1
        range_min = range_max - (n_elements(obs[i].vel)-1)

        obs[i].vel = v[range_min:range_max]
        obs[i].data = d[range_min:range_max]
        obs[i].var = s[range_min:range_max]
      ENDIF ELSE BEGIN
        obs[i].vel = v
        obs[i].data = d
        obs[i].var = s
      ENDELSE 

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
      
      IF keyword_set(extended) THEN BEGIN 

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
        IF size(timestr, /type) EQ 7 THEN BEGIN 
          timeparts = str_sep(timestr, ":")
          obs[i].time = timeparts[0]+timeparts[1]/60.0+timeparts[2]/3600.0
        ENDIF 
      
        obs[i].pacmd = fxpar(pheader, 'pacmd')
        obs[i].pbcmd = fxpar(pheader, 'pbcmd')
        obs[i].pamon = fxpar(pheader, 'pamon')
        obs[i].pbmon = fxpar(pheader, 'pbmon')
        obs[i].ccdtemp = fxpar(pheader, 'ccdtemp')
        obs[i].etemp = fxpar(pheader, 'temp4')
      ENDIF 
    ENDFOR

    return, obs

  ENDIF ELSE BEGIN
    message, 'No files found', /info
  ENDELSE 
END 
