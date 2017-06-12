pro thorium_argon, lambda, spectrum

  spectrum = readfits('/d/wham/pro/filters/thar.fits', thar_head)
  delt = fxpar(thar_head, 'cdelt1')
  zero_val = fxpar(thar_head, 'crval1')
  zero_pix = fxpar(thar_head, 'crpix1')
  lambda = zero_val + (findgen(n_elements(spectrum)) + 1 - zero_pix) * delt

end
