function vtow, v, lzero

  ; v in km/s, lzero in angstroms

  c = 299792.458D
  
  return, lzero + (v/c * lzero)

end