function wtov, l, lzero

  ; output v in km/s, l and lzero in angstroms

  c = 299792.458D
  
  return, (l - lzero) / lzero * c

end