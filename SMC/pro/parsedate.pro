FUNCTION parsedate, date, fitsformat = fitsformat, wham = wham
  ;; convert yymmdd or dd/mm/yy (FITS format) to jd
  ;; (can handle arrays)
  
  ;; /WHAM returns WHAM day number instead
 
  jdcnv, 1997, 1, 1, 0, day0
  
  ;; Check for newer IAU date format; quick convert, if so...
  if strpos(date[0], 'T') ne -1 then begin
    jd = list()
    foreach d, date do begin
      jd.add, date_conv(d, 'J')
    endforeach

    return, keyword_set(wham) ? long(jd.ToArray() - day0) : jd.ToArray()  
  endif

  ;; Check for yyyy/mm/dd
  if (strlen(strtrim(date[0])) eq 10) and (strpos(date[0], '/') eq 4) then begin
    jd = list()
    foreach d, date do begin
      jd.add, date_conv(strjoin(strsplit(date, '/', /extract), '-'), 'J')
    endforeach
    
    return, keyword_set(wham) ? long(jd.ToArray() - day0) : jd.ToArray()  
  endif

  IF keyword_set(fitsformat) THEN BEGIN
      year = fix(strmid(date, 6, 2)) + 1900
      month = fix(strmid(date, 3, 2))
      day = fix(strmid(date, 0, 2))
  ENDIF ELSE BEGIN 
      year = fix(strmid(date, 0, 2)) + 1900
      month = fix(strmid(date, 2, 2))
      day = fix(strmid(date, 4, 2))
  ENDELSE 

  ;; Y2K fix
  wyear = where(year LT 1950, wcnt)
  IF wcnt NE 0 THEN year[wyear] = year[wyear] + 100
  
  jdcnv, year, month, day, 0, jd
  
  return, keyword_set(wham) ? long(jd - day0) : jd
  
END 
  
