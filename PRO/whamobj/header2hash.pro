compile_opt idl2, logical_predicate

function header2hash, h

;  on_error, 2
  if double(!version.release) lt 8.0 then message, 'Sorry, requires IDL version 8.0 or above.'
  
  if ~ isa(h, 'STRING', /ARRAY) then $
    message, 'Input is not a string array!'
  
  split_h = strsplit(h, '=/', /extract)
  
  head_hash = hash()
  FOREACH line, split_h DO BEGIN
    ;; a non-keyword line (without an '=') will create a single element array
    if N_ELEMENTS(line) ge 2 then begin 

      ;; COMMENT or HISTORY line with an = or / in it... nothing here, keep moving
      if strmatch(line[0], 'COMMENT*') or strmatch(line[0], 'HISTORY*') then continue

      ;; special case for old-style FITS date values and one COMMENT field w/ slashes
      if N_ELEMENTS(line) ge 5 then begin
        
        if strmatch(line[0], 'EXPTYPE*') then begin
          ;; comment for EXPTYPE makes extra fields in split; treat it specially here.
          value = fix(line[1])
        endif else begin 

          ;; old-style date format; convert to something more useable
          date = fix(stregex(line[1:3], '[0-9]+', /extract))
          date[2] = (date[2] lt 50 ? date[2] + 2000 : date[2] + 1900)
          jd = julday(date[1], date[0], date[2], 0, 0, 0)
          value = string(jd, format = '(C(CYI4.4, "/", CMOI2.2, "/", CDI2.2))')

        endelse
        
      endif else begin 

        ;; try to map values to real IDL data types
        if STREGEX(line[1], '^[ E0-9.+-]+$', /BOOLEAN) then begin
          ;; looks like a number
  
          if STREGEX(line[1], '\.', /BOOLEAN) then begin
            ;; looks like a floating point number
            value = double(line[1])
  
          endif else begin
            ;; looks like an integer
            value = long(line[1])
  
          endelse
        endif else begin
        
          ;; check for boolean T/F value and convert to 1/0
          if STREGEX(line[1], '^ +T ?$', /BOOLEAN) then begin
            value = 1
          endif else if STREGEX(line[1], '^ +F ?$', /BOOLEAN) then begin
            value = 0
  
          endif else begin
            ;; probably a string; strip any quotes or leading/trailing spaces
            value = STREGEX(line[1], "^ *'?([^']+)'? *", /SUBEXPR, /EXTRACT)
            value = strtrim(value[1], 2)
  
          endelse
  
        endelse
        
      endelse 

      head_hash[strtrim(line[0], 2)] = value

    endif
  ENDFOREACH

  return, head_hash
  
 end
 