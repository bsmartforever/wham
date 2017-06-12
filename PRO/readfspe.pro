PRO readfspe, fn, v, d, s, header, ext = ext, pheader = pheader, $
              errmsg = errmsg

  IF NOT keyword_set(ext) THEN ext = 'RAWSPEC'

  ;; get around a core dump in IDL v 4 if extension is not found
  errmsg = ''
  fxbopen, unit, fn, ext, header, errmsg = errmsg
  IF errmsg NE '' THEN BEGIN
      v = fltarr(133)
      d = v
      s = v
      message, fn + ':' + errmsg, /info
      return
  ENDIF 
  fxbreadm, unit, ['VELOCITY', 'DATA', 'VARIANCE'], v, d, s

  fxbclose, unit

  pheader = headfits(fn)
END
