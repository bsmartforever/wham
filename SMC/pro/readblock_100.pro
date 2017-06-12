;FUNCTION readvc, wave, pa

;  fn = '/d/wham/survey/idl/vc.' + wave
;  command = 'wc -l ' + fn
;  spawn, command, res
;  ndp = (fix(res))[0]
;  alldata = fltarr(2, ndp)
;  openr, 1, fn
;  readf, 1, alldata
;  close, 1
;  return, alldata

;END  

PRO readblock, block, map, obslines, raw = raw, quiet = quiet, $
               ha = ha, sii = sii, nii = nii, extended = extended, $
               altspe = altspe, ftsext = ftsext
               
;+
;NAME: readblock
;
;SYNTAX: readblock, block, map, obslines, raw = raw, quiet = quiet, $
;	ha = ha, sii = sii, nii = nii, extended = extended, $
;	altspe = altspe, ftsext = ftsext
;
;MODIFICATION HISTORY
;2007-10-25 ASH: fixed bug in which sorting algorithm would fail if there is
;	an underscore ('_') in basename
;	commented out unused 'base' and 'dir' variables
;-

;  on_error, 2

  jdcnv, 1997, 1, 1, 0, day0

  bsz = size(block)

  ;; parse wavelength
  IF keyword_set(sii) THEN wave = 'sii' ELSE $
    IF keyword_set(nii) THEN wave = 'nii' ELSE BEGIN
    wave = ''
    ha = 1
  ENDELSE 

;  IF NOT keyword_set(raw) AND NOT keyword_set(ha) THEN $
;    vc = readvc(wave)

  CASE bsz[n_elements(bsz)-2] OF
    2: BEGIN  
      ;; user passed in a block number
      IF keyword_set(ha) THEN $
        fn = "/d/wham/observing/blocks.done" $
      ELSE $
        fn = "/d/wham/observing/blocks.done." + wave
      
      ;; find out which directory the spectra reside in
      cmd = 'grep "^' + strtrim(string(block), 2) + $
        ' " ' + fn
      ;; print, cmd
      spawn, cmd, obslines

      ;; has it been observed?
      IF (obslines[0] EQ '') THEN BEGIN
        message, 'NOTE: Block ' + strtrim(string(block), 2) + $
          ' has not been observed.'
      ENDIF

      ;; it's possible that there are more than one lines, only parse the
      ;; last one
      IF (n_elements(obslines) NE 1) THEN BEGIN
        message, 'NOTE: Block ' + strtrim(string(block), 2) + $
          ' observed on more than one date. Only extracting the last one', $
          /info
        print, obslines
      ENDIF ELSE BEGIN
        print, 'Reading: ' + obslines
      ENDELSE 
      
      last = n_elements(obslines)-1
      block = 0 & date = ''
      reads, obslines[last], block, date
      month = fix(strmid(date, 4, 2))
      day = fix(strmid(date, 7, 2))
      year = fix(strmid(date, 10, 2))

      basename = '/d/wham/survey/' + $
        string(year, month, day, format = '(3i2.2)') + '/' + wave + $
        '/combo/b' + string(block, format = '(i0)')
    END
    
    7: basename = block
    ;; user passed in a string
    
    ELSE: BEGIN
      message, 'First argument must be a string or integer', /info
      return
    END 
  ENDCASE 
  
  ;; get a sorted list of map spectra
  ;; NOTE (ASH 2007-10-25): this sorting breaks if any of the directories in
  ;;	the path contain an underscore ('_')
  ;; fixed using popd construction here

  dir = strmid(basename, 0, strpos(basename, '/', /reverse_search))
  shortbasename = strmid(basename, strpos(basename, '/', /reverse_search) + 1)
  pushd, dir

  sort_cmd = '/bin/ls '+shortbasename+'_[0-9]*.fts | /usr/bin/sort -n -t _ -k 2'
  spawn, sort_cmd, sfiles

  IF sfiles[0] EQ '' THEN BEGIN
    popd
    message, 'No files found', /info
    return
  ENDIF 
  
  ;; extract the last filename
  last_one = sfiles(n_elements(sfiles)-1)

  ;; extract the maximum x and y values from the last filename
  last_slash = strpos(basename, "/", /reverse_search)
  last_pos = strpos(last_one, "_", /reverse_search)
  lastdotpos = strpos(last_one, ".", /reverse_search)

;  base = strmid(basename, last_slash+1, 999)
;  dir = strmid(basename, 0, last_slash+1)
  
  IF keyword_set(extended) THEN BEGIN
    map = replicate({xpointing_100, $
                     name: string(''), $
                     vel: fltarr(100), $
                     data: fltarr(100), $
                     var: fltarr(100), $
                     glon: float(0), $
                     glat: float(0), $
                     vlsr: float(0), $
                     zd: float(0), $
                     az: float(0), $
                     date: string(''), $
                     day: long(0), $
                     time: float(0), $
                     pacmd: float(0), $
                     pbcmd: float(0), $
                     pamon: float(0), $
                     pbmon: float(0), $
                     ccdtemp: float(0), $
                     etemp: float(0), $
                     chi2: float(0), $
                     border: fix(0), $
                     ngauss: fix(0), $
                     bkg: fltarr(4), $
                     bkgsd: fltarr(4), $
                     mean: fltarr(10), $
                     meansd: fltarr(10), $
                     width: fltarr(10), $
                     widthsd: fltarr(10), $
                     area: fltarr(10), $
                     areasd: fltarr(10), $
                     atmos: float(0), $
                     a75r: float(0) $
                    }, n_elements(sfiles))
  ENDIF ELSE BEGIN
    map = replicate({pointing, $
                     name: string(''), $
                     vel: fltarr(100), $
                     data: fltarr(100), $
                     var: fltarr(100), $
                     glon: float(0), $
                     glat: float(0), $
                     vlsr: float(0), $
                     zd: float(0) $
                    }, n_elements(sfiles))
  ENDELSE 
  
  loaded = intarr(n_elements(sfiles))
  FOR i = 0, n_elements(sfiles)-1 DO BEGIN

    spectname = sfiles[i]
    IF NOT keyword_set(quiet) THEN $
      print, 'Reading file '+ spectname

    if n_elements(ftsext) eq 0 then begin 
      IF keyword_set(raw) OR NOT keyword_set(ha) THEN BEGIN 
        ftsext = 'RAWSPEC'
      ENDIF ELSE BEGIN
        ftsext = 'GEOSUB'
      ENDELSE 
    endif

    readfspe, spectname, v, d, s, btheader, $
              ext = ftsext, pheader = pheader, errmsg = errmsg

    IF errmsg EQ '' THEN begin

      IF keyword_set(altspe) THEN BEGIN $
        slashpos = strpos(spectname, '/', /reverse_search)
        dotpos = strpos(spectname, '.', /reverse_search)
        spename = altspe + "/" + $
                  strmid(spectname, slashpos+1, dotpos-slashpos-1) + $
                  ".spe"
        IF file_test(spename, /read) THEN BEGIN
          readspe, spename, v, d, s
        ENDIF ELSE BEGIN 
          message, $
            'WARNING: ' + spename + ' not found! Using original data.', /info
        ENDELSE 
      ENDIF 

      map[i].name = spectname
      
      ;; store velociy, data, and variance

      IF n_elements(v) EQ 133 THEN BEGIN
        print, "NOTE: Long spectrum found: chopping spectrum to red 100 data points."
        map[i].vel = v[33:*]
        map[i].data = d[33:*]
        map[i].var = s[33:*]
      ENDIF ELSE BEGIN 
        map[i].vel = v
        map[i].data = d
        map[i].var = s
      ENDELSE 

      ;; extract coordinate infomation from primary header
      map[i].glon = fxpar(pheader, 'dgal-lon')
      map[i].glat = fxpar(pheader, 'dgal-lat')
      IF !err EQ -1 THEN BEGIN 
        IF NOT keyword_set(quiet) THEN $
          print, '  Demanded coordinates not found; using actual'
        map[i].glon = fxpar(pheader, 'gal-lon')
        map[i].glat = fxpar(pheader, 'gal-lat')
      ENDIF ELSE BEGIN
        act_glon = fxpar(pheader, 'gal-lon')
        act_glat = fxpar(pheader, 'gal-lat')
        IF NOT keyword_set(quiet) THEN $
          print, '  Starting pointing error (arcmin):', $
                 sphdist(act_glon, act_glat, map[i].glon, map[i].glat, /deg)*60.0
      ENDELSE 

      ;; fix negative longitudes
      IF map[i].glon LT 0 THEN map[i].glon = 360 + map[i].glon
      
      ;; extract a few other parameters
      map[i].vlsr = fxpar(pheader, 'vlsr')
      map[i].zd = fxpar(pheader, 'zenith_d')
      
      IF keyword_set(extended) THEN BEGIN 
        
        ;; extract lots of extra goodies
        ha = fxpar(pheader, 'ha')
        dec = fxpar(pheader, 'dec')
;              alt = 90-map[i].zd
        olat = fxpar(pheader, 'obs-lat')
        hdtoaa, ha*15.0, dec, olat, alt, az, /deg
        map[i].az = az*!radeg

;              map[i].az = asin(cos(dec*!dtor)*sin(ha*15.0*!dtor) / $
;                               cos(alt*!dtor))
        
        map[i].date = fxpar(pheader, 'date-obs')
        map[i].day = parsedate(map[i].date, /fits) - day0
        
        timestr = fxpar(pheader, 'time-obs')
        timeparts = str_sep(timestr, ":")
        map[i].time = timeparts[0]+timeparts[1]/60.0+timeparts[2]/3600.0
        
        map[i].pacmd = fxpar(pheader, 'pacmd')
        map[i].pbcmd = fxpar(pheader, 'pbcmd')
        map[i].pamon = fxpar(pheader, 'pamon')
        map[i].pbmon = fxpar(pheader, 'pbmon')
        map[i].ccdtemp = fxpar(pheader, 'ccdtemp')
        map[i].etemp = fxpar(pheader, 'temp4')
        
        ;; see if there is fit data here
        chi2test = fxpar(btheader, 'chi2')
        IF (!err NE -1) THEN BEGIN 
          map[i].chi2 = fxpar(btheader, 'chi2')
          map[i].border = fxpar(btheader, 'border')
          map[i].ngauss = fxpar(btheader, 'ngauss')
          map[i].atmos = fxpar(btheader, 'atmos')
          map[i].a75r = fxpar(btheader, 'a75r')
          
          FOR j = 1, map[i].border+1 DO BEGIN 
            map[i].bkg[j-1] = fxpar(btheader, 'bkg' + $
                                    strtrim(j, 2))
            bkgsd = fxpar(btheader, 'bkgsd' + strtrim(j, 2))
            map[i].bkgsd[j-1] = strtrim(bkgsd, 2) EQ '!!!!!' ? 0 : bkgsd
          ENDFOR 
          
          FOR j = 1, map[i].ngauss DO BEGIN 
            map[i].mean[j-1] = fxpar(btheader, 'mean' + $
                                     strtrim(j, 2))
            meansd = fxpar(btheader, 'meansd' + strtrim(j, 2))
            map[i].meansd[j-1] = strtrim(meansd,2) EQ '!!!!!' ? 0 : meansd
            map[i].width[j-1] = fxpar(btheader, 'width' + $
                                      strtrim(j, 2))
            widthsd = fxpar(btheader, 'widthsd' + strtrim(j, 2))
            map[i].widthsd[j-1] = strtrim(widthsd,2) EQ '!!!!!' ? 0 : widthsd
            map[i].area[j-1] = fxpar(btheader, 'area' + $
                                     strtrim(j, 2))
            areasd = fxpar(btheader, 'areasd' + strtrim(j, 2))
            map[i].areasd[j-1] = strtrim(areasd,2) EQ '!!!!!' ? 0 : areasd
          endfor 
        endif
      ENDIF  

;      IF NOT keyword_set(ha) THEN BEGIN
      ;; velocity correction applied based on pressure of
      ;; chamber A for now
      
;        pacmd = fxpar(pheader, 'PACMD')
      
;        diff = abs(round(vc[0, *]-pacmd/10.0))
;        mindiff = where(diff EQ min(diff))
      
;        IF diff(mindiff[0]) NE 0 THEN $
;          message, 'WARNING: Using correction for PA = ' + $
;          strtrim(string(vc[0, mindiff[0]]), 2) + ' but PACMD = ' + $
;          strtrim(string(pacmd/10.0), 2), /info
      
;        map[i].vel = v - vc[1, mindiff[0]] - map[i].vlsr
;      ENDIF 
      
      loaded[i] = 1
    endif ELSE BEGIN 
      loaded[i] = 0
    ENDELSE
  endfor 

  good = where(loaded EQ 1, gcount)
  IF gcount EQ 0 THEN BEGIN 
    print, "No pointings with requested extension found. Zero map returned."
  ENDIF ELSE BEGIN 
    IF gcount NE n_elements(sfiles) THEN BEGIN
      map = map[good]
      print, "Loaded ", strtrim(n_elements(map), 2), " pointings out of ", $
             strtrim(n_elements(sfiles), 2)
    ENDIF
  ENDELSE 
  
  popd

END

