pro predict_sky, filter, uves, focus = focus, show_growth = show_growth

  if n_params() ne 2 then begin
    message, 'Usage: predict_sky, filter_number, uves_spectrum_number', /info
    uves_sky
    return
  endif

  ; read filter information and profiles
  restore, '/d/wham/pro/filters/wham_filters_template.dat'
  filters = read_ascii('/d/wham/pro/filters/wham_filters.txt', $
    template = wham_filters_template)
  profiles = read_filters()

  f = (where(filters.number eq filter, fcount))[0]
  
  if fcount eq 0 then $
    message, 'No filter with inventory number ' + strtrim(filter, 2) + ' found.'

  fp_search = where(filters.short_name[f] eq profiles.name, fpcount)

  if fpcount eq 0 then begin 
    ;; Approximate with a 20 ang FWHM Gaussian at the recorded filter center
    fwhm = 20.0
    f_lam = findgen(2000)*0.1 + (filters.center[f] - fwhm*2.0)
    f_trans = gaussian(x, [1, filters.center[f], fwhm / (2 * sqrt(2 * alog(2)))])
  endif else if fpcount eq 1 then begin
    f_lam = profiles[fp_search[0]].lambda
    f_trans = profiles[fp_search[0]].transmission

    ;; need to get down to black outside of filter -- subtract "baseline" 
    ;;   could fit this as well, but simple for now...
    ;;   can be removed if original profiles are corrected.
    f_trans = (f_trans - avg([f_trans[0:30], f_trans[n_elements(f_trans)-30:*]])) > 0
    
  endif else begin
    message, 'Oops, more than one profile name match for "' $
      + filters.short_name[f] + '"'
  endelse 

  ;; shift filter for effective average angle through filter (~2.4 deg)
  f_lam_orig = f_lam
  f_lam = f_lam_orig * sqrt(1 - (1/2.1)^2.0 * sin( (2.4 * !dtor)^2.0 ))
  
  ;; transmission weighted center
  center = int_tabulated(f_lam, f_trans*f_lam) / int_tabulated(f_lam, f_trans)
  l_range = center + [-25, 25]
  print, center, int_tabulated(f_lam_orig, f_trans*f_lam_orig) / int_tabulated(f_lam_orig, f_trans)

  ;; plot filter profile
  if !d.name eq 'X' then wset_or_create, 0

  if !d.name eq 'PS' then begin
    !p.thick = 2
    !p.charthick = 2
  endif

  kplot, f_lam, f_trans, xrange = l_range, xstyle = 1, yrange = [0, 1], $
    xtitle = 'Wavelength [' + string("305B) + ']', $
    ytitle = 'Transmission / Relative Intensity', $
    psym = 100

  ;; grab sky spectrum, locate plotted subregion, interpolate filter transmission 
  ;;   to the higher resolution wavelength of the sky spectrum
  uves_sky, uves_lam, uves_data, uves
  uves_win = where(!x.crange[0] le uves_lam and uves_lam le !x.crange[1])
  uves_win_lam = uves_lam[uves_win]
  
  f_win = where(!x.crange[0] le f_lam and f_lam le !x.crange[1])
  f_trans_interp = interpol(f_trans[f_win], f_lam[f_win], uves_lam[uves_win], /spline)

  ;; intensity scaling for sky spectrum
  max_in_win = max(uves_data[uves_win])

  ;; overplot sky spectrum unmodified and with filter profile applied
;  oplot, uves_lam[uves_win], f_trans_interp, color = color(250)
  oplot, uves_win_lam, uves_data[uves_win]/max_in_win, color = color(1)
  oplot, uves_win_lam, uves_data[uves_win]/max_in_win * f_trans_interp, color = color(250)

  ;; make WHAM FP transmission function; need span to be more than 2x filter window
  dl = uves_win_lam[1] - uves_win_lam[0]
  fp_size = n_elements(uves_win_lam) * 2.5
  fp_start = (max(uves_win_lam) + min(uves_win_lam))/2.0 - (fp_size/2 * dl) 
  fp_lam = dindgen(fp_size) * dl + fp_start
  fp_sig = 1/fp_lam

  ;; We want the sigma sampling to be equal to that of the filter window wavelengths
  ;;  but as of this writing, we only have a good tune on the model near H-alpha.
  ;;  So we translate the target sigma vector to the range around H-alpha, which 
  ;;  keeps the sampling of the target wavelengh range.
  fp_sig_ha = fp_sig - (avg(fp_sig) - 1/6562.8)
  fp_lam_ha = 1/fp_sig_ha
  make_airy, fp_lam_ha, sigma, i_high, i_low, i_tot, theta = 0

  ;; Now we can use this i_tot with the original fp_lam since the FSR is roughly constant
  ;;  in sigma-space. The only minor assumption we are making is that n(gas) is 
  ;;  similar to that used for H-alpha, which is a pretty safe assumption since Q(sig) only 
  ;;  changes by ~0.2% over 3 atm of pressure.

;  oplot, fp_lam, i_tot

  fp_peak = (where(i_tot eq max(i_tot)))[0]

  sky_total = dblarr(n_elements(uves_win))
  filtered_sky = uves_data[uves_win] * f_trans_interp

  for i=0, n_elements(uves_win)-1 do begin
    fp_start = fp_peak - i
    fp_end = fp_start + n_elements(uves_win_lam) - 1
    sky_total[i] = total(filtered_sky * i_tot[fp_start:fp_end])/total(i_tot)
    
    if keyword_set(show_growth) and n_elements(focus) eq 3 and !d.name eq 'X' then begin 
      wset_or_create, 2
      dmin = min(filtered_sky, max = dmax)
      kplot, uves_win_lam, i_tot[fp_start:fp_end]*filtered_sky, kplot_type='io', $
        yrange = [1e-8, dmax]
      koplot, uves_win_lam[i] * [1, 1], 10^!y.crange, line = 1
      wset_or_create, 1

      kplot, wtov(uves_win_lam, focus[0]), sky_total, kplot_type='io', $
        xrange = focus[1:2], yrange = [dmin, dmax], psym=100
      koplot, wtov(uves_win_lam, focus[0]), filtered_sky, psym=100, color = color(1)

      wait, 0.1
    endif
    
  endfor
  
  oplot, uves_win_lam, sky_total/max_in_win
  
  if n_elements(focus) eq 3 then begin
    
    lmin = vtow(focus[1], focus[0])
    lmax = vtow(focus[2], focus[0])
    lsub = where(lmin le uves_win_lam and uves_win_lam le lmax) 
    dmin = min([sky_total[lsub], filtered_sky[lsub]], max = dmax)
        
    if !d.name eq 'X' then wset_or_create, 1
    kplot, wtov(uves_win_lam, focus[0]), sky_total, kplot_type='io', $
        xrange = focus[1:2], yrange = [dmin, dmax], psym=100, $
        xtitle = 'Velocity [km/s]', ytitle = 'Intensity'
    koplot, wtov(uves_win_lam, focus[0]), filtered_sky, psym=100, color = color(1)
    
    fp_shift = focus[0] - fp_lam[fp_peak]
    koplot, wtov(fp_lam + fp_shift, focus[0]), i_tot + 10^!y.crange[0], color=color(250), psym=100
  endif
    
  if !d.name eq 'PS' then begin
    !p.thick = 1
    !p.charthick = 1
  endif
  
end