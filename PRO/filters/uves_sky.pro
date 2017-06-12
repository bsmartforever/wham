pro uves_sky, lambda, spectrum, which

  uves_map = {$
    filename: ['346', '437', '564U', '580L', '580U', '800U', '860L', '860U'], $
    l_min: [3140, 3740, 5700, 4800, 5800, 8530, 6700, 8600], $
    l_max: [3760, 4940, 5900, 5800, 6760, 8630, 8560, 10430] }
      
  if n_elements(which) eq 0 then begin
    print, "Must specify which spectrum to use as the last argument:"
    for i = 0, n_elements(uves_map.filename)-1 do $
      print, i, uves_map.filename[i], uves_map.l_min[i], uves_map.l_max[i], $
        format = '(I1, ": ", A, " (", I5, "-", I5, ")")'
  endif else begin 
    spectrum = readfits('/d/wham/pro/filters/UVES/fluxed_sky_' + uves_map.filename[which] + '.fits', uves_head)
    delt = fxpar(uves_head, 'cdelt1')
    zero_val = fxpar(uves_head, 'crval1')
    zero_pix = fxpar(uves_head, 'crpix1')
    lambda = zero_val + (findgen(n_elements(spectrum)) + 1 - zero_pix) * delt
  endelse

end