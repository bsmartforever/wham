@kplot
pro plotUnroll, unroll, spectrum, $
    no_bins = no_bins, no_data_points = no_data_points, no_bad = no_bad, labels = labels, fits = fits, $
    rsqr = rsqr, bin_range = bin_range, xrange = xrange, _extra = _extra

  if n_elements(bin_range) ne 0 then begin
    if (n_elements(bin_range) lt 1) or (n_elements(bin_range) gt 2) then begin
      message, "BIN_RANGE must be one or two elements; ignoring", /info
    endif else begin
      if n_elements(bin_range) eq 1 then begin 
        xmin = bin_range eq 0 ? 0 : spectrum.rsqr[bin_range-1]
        xmax = spectrum.rsqr(bin_range)
      endif else begin
        xmin = bin_range[0] eq 0 ? 0 : spectrum.rsqr[bin_range[0]-1]
        xmax = spectrum.rsqr(bin_range[1])
      endelse
      xrange = keyword_set(rsqr) ? [xmin, xmax] : sqrt([xmin, xmax])
    endelse
  endif
  
  if n_elements(xrange) eq 0 then xrange = [0, keyword_set(rsqr) ? max(spectrum.rsqr) : max(sqrt(spectrum.rsqr))]

  kplot, keyword_set(rsqr) ? unroll.radii^2.0 : unroll.radii, unroll.data, psym = 99, symsize = 0.7, $
    xtitle = keyword_set(rsqr) ? "Radius squared [pix!u2!n]" : "Radius [pix]", $
    ytitle = "ADU", $
    xrange = xrange, _extra = _extra
  
  rsqr_range = keyword_set(rsqr) ? !x.crange : !x.crange^2.0
  bins = where((rsqr_range[0] + spectrum.rsqr[0]/2.0) le spectrum.rsqr and spectrum.rsqr le (rsqr_range[1] - spectrum.rsqr[0]/2.0))
  
  if not keyword_set(no_bins) then $
    for i=0, n_elements(spectrum.rsqr)-1 do $
      oplot, (keyword_set(rsqr) ? spectrum.rsqr[i] : sqrt(spectrum.rsqr[i]))*[1,1], !y.crange, color=color(50)
    
  if not keyword_set(no_data_points)  then begin
    bin_center = spectrum.rsqr - spectrum.rsqr[0]/2.0
    koplot, keyword_set(rsqr) ? bin_center : sqrt(bin_center), spectrum.data, yerror=sqrt(spectrum.var), $
      psym = 6, color=color(200), symsize = 0.7
  endif
  
  if not keyword_set(no_bad) then begin
    bad_1 = where(unroll.bad eq 1)
    bad_2 = where(unroll.bad eq 2)
    
    oplot, keyword_set(rsqr) ? unroll.radii[bad_1]^2.0 : unroll.radii[bad_1], unroll.data[bad_1], psym=7, $
      symsize = 0.7, color=color(128)
    oplot, keyword_set(rsqr) ? unroll.radii[bad_2]^2.0 : unroll.radii[bad_2], unroll.data[bad_2], psym=7, $
      symsize = 0.7, color=color(250)
  endif
  
  if keyword_set(labels) then begin
    xyouts, !x.crange[0] - 0.10*(!x.crange[1]-!x.crange[0]), !y.crange[1] + 0.02*(!y.crange[1] - !y.crange[0]), $
      '# (N)'
    
    for i=0, n_elements(bins)-1 do begin
      bin_center = spectrum.rsqr[bins[i]] - spectrum.rsqr[0]/2.0
      xyouts, keyword_set(rsqr) ? bin_center : sqrt(bin_center), !y.crange[1] + 0.02*(!y.crange[1] - !y.crange[0]), $
        strtrim(bins[i], 2) + ' (' + strtrim(spectrum.n[bins[i]], 2) + ')', $
        align = 0.5
    endfor
  endif
  
  if keyword_set(fits) then begin
    for i=0, n_elements(bins)-1 do begin
      rmin = bins[i] eq 0 ? 0 : spectrum.rsqr[bins[i] - 1]
      rmax = spectrum.rsqr(bins[i])
      pix = where(rmin le unroll.radii^2.0 and unroll.radii^2.0 le rmax)

      koplot, keyword_set(rsqr) ? unroll.radii[pix]^2.0 : unroll.radii[pix], $
        spectrum.fit[bins[i], 0] + spectrum.fit[bins[i], 1]*unroll.radii[pix], $
        color=color(100), psym=100
    endfor
       
    bin_center = spectrum.rsqr - spectrum.rsqr[0]/2.0
    koplot, keyword_set(rsqr) ? bin_center : sqrt(bin_center), spectrum.data, $
      yerror=sqrt(spectrum.fit[*, 2]/(spectrum.n - 2)), $
      psym = 17, color=color(100)
  endif 

end
    