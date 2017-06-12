pro writeSpectrum, images, header, spectrum, file, ringfile, $
  params = params, low_reject = low_reject, all_images = all_images, $
  version = version, overwrite = overwrite, verbose = verbose

  ;; This routine creates a new FITS file that includes the original image exposure,
  ;; (optionally) processing history images, and the final ring-summed spectrum as
  ;; a RAWSPEC extension.

  if file_test(file) && ~keyword_set(overwrite) then begin
    message, file + " exists and OVERWRITE not specified. No output generated."
    return
  endif
  
  mwrfits, images.orig_int, file, header, /create, silent = ~keyword_set(verbose)

  ;; Optionally include intermediate processing image output
  if keyword_set(all_images) then begin
    fxaddpar, im_header, 'EXTNAME', 'BADPIX'
    mwrfits, images.bad, file, im_header, silent = ~keyword_set(verbose)
    fxaddpar, im_header, 'EXTNAME', 'CLEAN'
    mwrfits, images.clean, file, im_header, silent = ~keyword_set(verbose)
    fxaddpar, im_header, 'EXTNAME', 'REFLECTION'
    mwrfits, images.reflect, file, im_header, silent = ~keyword_set(verbose)
    fxaddpar, im_header, 'EXTNAME', 'REFLECT-SUB'
    mwrfits, images.reflectSub, file, im_header, silent = ~keyword_set(verbose)
  endif
  
  out_spectrum = replicate({velocity: spectrum.vel[0], data: spectrum.data[0], variance: spectrum.var[0]}, $
                              n_elements(spectrum.vel))

  ;; reverse if needed to have velocity increase... cor_spec expects this ordering right now.
  out_spectrum.velocity = (params.v1 gt 0 ? spectrum.vel : reverse(spectrum.vel))
  out_spectrum.data = (params.v1 gt 0 ? spectrum.data : reverse(spectrum.data))
  out_spectrum.variance = (params.v1 gt 0 ? spectrum.var : reverse(spectrum.var))

  fxaddpar, sp_header, 'TUNIT1', 'KM/S'
  fxaddpar, sp_header, 'TUNIT2', 'ADU'
  fxaddpar, sp_header, 'TUNIT3', 'ADU^2'
  
  fxaddpar, sp_header, 'XC', params.xc, 'X center for image to spectrum conversion'
  fxaddpar, sp_header, 'YC', params.yc, 'Y center for image to spectrum conversion'
  fxaddpar, sp_header, 'RMAX', params.rmax, 'Maximum pixel radius for ring-summing'
  fxaddpar, sp_header, 'AREA', params.area, 'Number of image pixels per spectral data point'
  fxaddpar, sp_header, 'XOFF', params.xoff, 'X offset of primary reflection'
  fxaddpar, sp_header, 'YOFF', params.yoff, 'Y offset of primary reflection'
  fxaddpar, sp_header, 'E', params.e, 'Reflection multiplier'
  fxaddpar, sp_header, 'BVAL', params.bval, 'Bias value subtracted from image'
  fxaddpar, sp_header, 'V0', params.v0, 'Velocity of first data point'
  fxaddpar, sp_header, 'V1', params.v1, '1st order velocity dispersion'
  fxaddpar, sp_header, 'V2', params.v2, '2nd order velocity dispersion'
  fxaddpar, sp_header, 'LOW-REJ', (keyword_set(low_reject) ? 'T' : 'F'), 'Low sigma reject applied'
  fxaddpar, sp_header, 'DATE', date_conv(systime(/jul),'F')
  fxaddpar, sp_header, 'EXTNAME', 'RAWSPEC'

  sxaddhist, [' ', ' *** Column units ***', ' '], sp_header, /comment, location = 'TUNIT1'
  sxaddhist, [' ', ' *** Ring-sum parameters ***', ' '], sp_header, /comment, location = 'XC'

  sxaddhist, string(version, ringfile, format = '("(makeSpectPro v. ", F4.2, ") Spectrum created from ", A)'), sp_header
  
  mwrfits, out_spectrum, file, sp_header, silent = ~keyword_set(verbose)
  
end