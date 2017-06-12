@predict_vel_calib3     ; need n_non_lin, n_wave for SF6 pressure -> index

pro make_airy, lambda, sigma, inten1, inten2, inten, theta = theta, pa = pa, pb = pb

  ; input wavelengths assumed to be in air wavelengths (angstroms)
  ; theta input in degrees (converted below)
  ; sigma is wavenumber (vacuum), inten* are the transmission functions
  ; optional pressures are in cm

  if n_elements(pa) eq 0 then pa = 94.3
  if n_elements(pb) eq 0 then pb = 114.5

  vlambda = double(lambda)
  airtovac, vlambda

  ;; optical index with non-linear corrections for SF6 pressure and 
  ;; wavelength of interest

  ;; n_wave returns n for 76.0 cm at lambda
;  n_wave_corr = n_wave(vlambda) - n_non_lin(76.0) 
  
;  n1 = n_non_lin(pa) + n_wave_corr
;  n2 = n_non_lin(pb) + n_wave_corr

  n1 = n_sf6(pa, 21)
  n2 = n_sf6(pb, 21)

  ;; etalon gap spacings [cm]: emperically determined by GJM ~9/4/09
  ;l1 = 0.047077D
  ;l2 = 0.020144D 

  ; by LMH by trusting n_sf6(), 4/9/2010:

  ; tune = 94.3, 114.5 (rest H-alpha at 50th ring)
  ; wave @ theta = 0: vtow(+99.6983, 6562.8) = 6564.98
  ; vac wave = 6566.7958
  ; m(high) = 1435, m(low) = 614

  l1 = 0.047075033
  l2 = 0.020138323

  if n_elements(theta) eq 0 then theta = 0.0 else theta = theta * !dtor
  r = 0.90
  
  sigma = 1/(vlambda/1e8)  ; [cm^-1]
  
  phi1 = 4 * !dpi * n1 * l1 * cos(theta) * sigma
  phi2 = 4 * !dpi * n2 * l2 * cos(theta) * sigma

  inten1 = airy(r, phi1)
  inten2 = airy(r, phi2)
  inten = inten1 * inten2

end



