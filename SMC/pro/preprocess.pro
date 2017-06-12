;+
; :Description:
;   Preprocess WHAM data for reduction.
;-

pro preprocess, dir, params = params, mpd = mpd, no_atm = no_atm, trust_biases = trust_biases
  compile_opt idl2

  ;; Apply atmospheric transmission estimate during COR_SPEC?
  if ~isa(no_atm) then no_atm = 0

  ;; if dir is supplied, only do those. Otherwise, find all date directories
  if ~isa(dir) or (dir eq '.') then begin

    ;; find all date directories at the CWD (note, we do not recurse here)
    paths = file_search('[0-9][0-9][0-9][0-9][0-9][0-9]*', /MARK_DIRECTORY, /TEST_DIRECTORY, $
      count = dcount)

    if dcount eq 0 then begin

      ;; maybe we are sitting in a date directory...
      cd, current=cwd
      cwd_date = file_basename(cwd)

      ;; if no, don't do anything
      if ~strmatch(cwd_date, '[0-9][0-9][0-9][0-9][0-9][0-9]*') then begin
        message, 'No date directories found.', /info
        return
      endif

      ;; yes, so collect all directories at this level
      paths = file_search('*', /MARK_DIRECTORY, /TEST_DIRECTORY, $
        count = dcount)

      if dcount eq 0 then begin
        ;; throw up our hands at this point...
        message, 'No directories found.', /info
        return
      endif

    endif
  end else begin
    paths = [dir]
  endelse

  ;; This is destructive, so make sure from the user
  print, 'About to preprocess ', strtrim(n_elements(paths), 2), ' directories, which will overwrite all combo spectra in:', paths
  ask = ''
  read, ask, prompt='Are you sure? (Y/N): '

  if strupcase(strmid(ask, 0, 1)) ne 'Y' then return

  ;; loop through each directory
  foreach d, paths do begin

    ;; try to extract the date dir for this path
    pdate = stregex(d, '[0-9]{6}', /extract)
    
    if pdate eq '' then begin
      
      ;; maybe we are sitting in a date directory...
      if isa(cwd_date) then begin
        pdate = cwd_date
      endif else begin
        cd, current=cwd
        pcwd = file_basename(cwd)

        pdate = stregex(pcwd, '[0-9]{6}', /extract)

        ;; if no, don't do anything
        if pdate eq '' then begin
          message, 'No date directories found in path "' + d + '" and not sitting in a date directory.', /info
          return
        endif
      endelse

    endif

    ;; Check for KPNO
    pday = parsedate(pdate)
    if pday lt parsedate('090301') then kpno = 1 else kpno = 0

    ;; makeSpect on all files - we read in the MPD file here if params not supplied
    ;;    and then customize whatever we need for each date
    if ~isa(params) then begin
      if ~isa(mpd) then begin
        ;; Read in defaults
        params = readmpd()

        ;; Replace some params with KNPO values, if needed
        if kpno then begin
          message, 'Loading KPNO parameters for makeSpect.', /info
          params.xoff = 37.50
          params.yoff = -21.25
          params.v0 = -112.31088
          params.v1 = 2.083
          params.v2 = -0.00036

          ;; TODO: This cetner is good for 10/19/2004 - 2007. Still need to incorporate 
          ;;       KPNO center determinations done earlier. 
          ;;       (e.g., makeSpect.mpd comments and centers.txt file)
          params.xc = 59.30
          params.yc = 60.69
        endif
      endif else begin
        ;; User supplying MPD file
        params = readmpd(mpd)
      endelse
    endif

    ;; figure out best bias value
    if stregex(d, '[0-9]{6}/?$', /boolean) then begin
      ;; path ends in a date directory -- search from here
      
      ;; look for KPNO biases in the merged directories first; 
      ;;    don't want KPNO biases from raw since they don't have full headers.
      bias = file_search(d, 'merged/bias-*', count = bcount)

      ;; if none found, look for CTIO biases in raw directories instead
      if bcount eq 0 then begin
        bias = file_search(d, 'raw/bias-*', count = bcount)
      endif
    endif else begin
      ;; Path does not end in a date directory--probably executing on one wavelength.
      ;; Try to make sure we grab all biases for this date anyway, so start search a directory up.
      bias = file_search(d + '/..', 'merged/bias-*', count = bcount)
      if bcount eq 0 then begin
        bias = file_search(d + '/..', 'raw/bias-*', count = bcount)
      endif
    endelse

    if bcount eq 0 then begin

      ;; no biases found, use model
      params.bval = estimate_bias(pdate)

    endif else begin

      ;; found some biases
      bvals = list()
      humids = list()
      temp_flag = 0

      ;; grab bias data, compute mean, and snag some sensor data
      foreach b, bias do begin
        bdata = readfits(b, header, /silent)
        hhash = header2hash(header)
        bvals.add, mean(bdata)
        humids.add, hhash['HUMID3']     ;; wall sensor humidity
        if hhash['CCDTEMP'] ge -99 then temp_flag++
      endforeach

      ;; compute model estimate for sanity comparison
      bval_model = estimate_bias(pdate, HUMID=mean(humids.toarray()))
      if temp_flag then begin

        ;; We know warm exposures float higher than the average, so shouldn't use them as a
        ;;  fiducial for the night. Use model estimate instead.
        message, 'At least one bias was taken with CCDTEMP > -99 deg C; using model bval instead', /info
        params.bval = bval_model

      endif else begin

        ;; OK, we are good to try using the mean of the biases on this night
        params.bval = mean(bvals.toarray())

        ;; Now, some sanity checking here based on humidity/date modeling.
        ;;    NOTE: Some dates aren't modeled well yet... although most survey data is.
        ;;          When generalizing/expanding this routine, should check or be careful of this
        ;;          or there will be a lot of unnecessary rejections.
        if (abs(params.bval - bval_model) ge 1.2) && ~keyword_set(trust_biases) then begin
          message, 'Mean of bias values differs from model by more than 1.2 ADU; using model instead: ' $
            + strtrim(bval_model, 2), /info
          params.bval = bval_model
        endif else begin
          message, 'Using bval from ' + strtrim(bcount, 2) + ' biases on this date: ' $
            + strtrim(params.bval, 2), /info
        endelse

      endelse
    endelse

    ;; Finally execute makeSpect!
    makeSpectDir, d, params=params, exclude='bias-*', /overwrite

    ;; And now create PROCSPECs -- new COR_SPEC should use pre-cleaning flats when appropriate!
    cor_spec_ctio, d, /chop, /silent, no_atm = no_atm

  endforeach

end
