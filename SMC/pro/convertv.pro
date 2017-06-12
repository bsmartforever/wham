FUNCTION convertv, oldv

  p = indgen(133)
  oldnewv, ov, nv

  pix = interpol(p, ov, oldv)

  print, 'Pixel: ', pix
  
  return, interpol(nv, p, pix)
END
  
