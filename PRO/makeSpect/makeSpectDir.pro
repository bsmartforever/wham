;+
; :Description:
;
;-
pro makeSpectDir, dir, exclude = exclude, _extra=extra
  compile_opt idl2
  
  ;; find all raw directories under the passed-in parameter dir
  raw = file_search(dir, 'raw', /TEST_DIRECTORY, count=raw_count)
  if raw_count eq 0 then begin
    message, 'No raw directories found starting at ' + dir, /info
    return
  endif

  ;; loop over each raw directory we've found  
  foreach r, raw do begin
    print, 'Processing ' + r
    ts = systime(/SECONDS)
    
    ;; find all FITS files in this raw directory
    files = file_search(r, '*.fts')
    if ~isa(files, /array) then begin
      message, 'No FITS files found in ' + r, /info
      continue
    endif

    ;; make sure a combo directory exists
    combo = r + '/../combo'
    if ~file_test(combo, /DIRECTORY) then begin
      if ~file_test(combo) then begin
        ;; need to make combo
        file_mkdir, combo
      endif else begin
        ;; combo exists, but isn't a directory.... problem!
        message, r + ' exists, but is apparently not a directory. Remove it or fix!', /info
        continue
      endelse
    endif

    ;; handle EXCLUDE, if passed - using IDL 8 LIST for this...
    files_list = list(files, /EXTRACT)
    if isa(exclude) then begin
      foreach e, exclude do begin
        which = where(strmatch(files_list.toArray(), '*/' + e) eq 1, wcount)
        if wcount ne 0 then files_list.Remove, which
      endforeach
    endif

    ;; Now, call makeSpect on each remaining file with output going to combo directory
    foreach f, files_list do begin
      base = file_basename(f)
      makeSpect, f, combo + '/' + base, /LOW_REJECT, _extra=extra
    endforeach

    ;; Output timing 
    dt = systime(/SECONDS) - ts
    print, 'Processed ' + strtrim(files_list.count(), 2) + ' observations in ' $
            + strtrim(dt, 2) + ' seconds (' + strtrim(files_list.count() / dt, 2) + ' obs/sec)'
    
  endforeach

end
