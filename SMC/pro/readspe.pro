PRO readspe, fn, v, d, s, n, npix = npix
  command = 'wc -l "' + fn + '"'
  spawn, command, res
  ndp = fix(res)
  ndp = ndp(0)

  IF keyword_set(npix) THEN $
    alldata = fltarr(4, ndp) $
  ELSE $
    alldata = fltarr(3, ndp)

  openr, 1, fn
  readf, 1, alldata
  v = reform(alldata(0, *))
  d = reform(alldata(1, *))
  s = reform(alldata(2, *))

  IF keyword_set(npix) THEN $
    n = reform(alldata(3, *))

;  dummyv = double(1)
;  dummyd = double(1)
;  dummys = double(1)
;  FOR i = 0, ndp-1 DO BEGIN
;      readf, 1, dummyv, dummyd, dummys
;      v(i) = dummyv
;      d(i) = dummyd
;      s(i) = dummys
;  ENDFOR

  close, 1
END
