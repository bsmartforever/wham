FUNCTION parsedate, date, fitsformat = fitsformat
  ;; convert yymmdd or dd/mm/yy (FITS format) to jd
  ;; (can handle arrays)

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
  
  return, jd
  
END 