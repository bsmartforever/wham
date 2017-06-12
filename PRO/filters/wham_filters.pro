;+
; :Description:
;    Plot WHAM filter curves.
;
;
;
; :Keywords:
;    lmin : in, optional, type=float
;       minimum wavelength in angstroms
;    lmax : in, optional, type=float
;       maximum wavelength in angstroms
;    lzero : in, optional, type=float
;       reference wavelength for optional velocity axis
;    labels : in, optional, type=int
;       label type : 0=none, 1=short, 2=long
;    thar : in, optional, type=int
;       overplot ThAr lamp spectrum
;    uves : in, optional, type=int
;       overplot UVES sky spectrum N [0-7]
;    shift : in, optional, type=int
;       apply off-axis angle shift
;       0 : no shift
;       1 : effective average shift (~2.4 deg) of full cone [default]
;       2 : maximum angle shift (4.159 deg) seen by furthest off-axis rays
;       
; :Author: LMH
; 
; :History:
;   
;   2/24/2010: Initial version
;   3/30/2010: Added ThAr and UVES spectral overplotting
;   4/21/2010: Add wavelength shift for off-axis rays
;   
;-
pro wham_filters, lmin = lmin, lmax = lmax, lzero = lzero, labels = labels, $
                  thar = thar, uves = uves, shift = shift, only_filters = only_filters,$
		  ps=ps

if keyword_set(ps) then set_plot,'ps' else set_plot,'x'
  
  ; read filter information and profiles
  restore, '/d/wham/pro/filters/wham_filters_template.dat'
  filters = read_ascii('/d/wham/pro/filters/wham_filters.txt', $
    template = wham_filters_template)
  profiles = read_filters()
;  profiles[5].lambda += 21
;  profiles[10].lambda += 22.78

  if N_ELEMENTS(only_filters) ne 0 then begin
    good = []
    foreach f, only_filters do begin
      w = where(filters.short_name eq f, count)
      if count ne 1 then begin
        message, 'No filter found for "' + f + '"', /info
      endif else begin
        good = [good, w]
      endelse
    endforeach
  endif else begin 
    ; ignore the 400 series "filters" (open, etc.)
    good = where(filters.number lt 400 or filters.number ge 500)
    gs = sort(filters.center[good])
    good = good[gs]
  endelse
  
  ; plot filter profiles with user limits
  if n_elements(lmin) eq 0 then lmin = 4500.0
  if n_elements(lmax) eq 0 then lmax = 7500.0

  ; check labels keyword
  if (n_elements(labels) ne 0) then begin
    if (labels lt 0 or labels gt 2) then begin 
      message, 'Bad LABELS keyword value: should be 0, 1, or 2', /info
      labels = 0
    endif
  endif else labels = 0

  ; set up plot
  
  if keyword_set(ps) then begin
    !p.thick = 2
    !p.charthick = 2
  endif
  
  plot, [lmin, lmax], [0, 1.1], /nodata, $
    xstyle = 9, ystyle = 1, ymargin = [4, 4], $
    xtitle = 'Wavelength [' + string("305B) + ']', $
    ytitle = 'Transmission' 

  ; check for velocity frame
  if n_elements(lzero) ne 0 then begin
    vrange = (!x.crange - lzero)/lzero * 299792.458 
    axis, xaxis = 1, xstyle = 1, $
      xrange = vrange, xtitle = 'Velocity [km/s]'

    ; Add velocity grid lines
    v_grid = ceil(vrange[0]/200) * 200
    while v_grid le vrange[1] do begin 
      oplot, v_grid / 299792.458 * lzero + lzero * [1, 1], !y.crange, linestyle=1
      v_grid += 200
    endwhile

  endif else begin
    axis, xaxis = 1, xstyle = 1, $
      xtitle = 'Wavelength [' + string("305B) + ']'
  endelse

  ; label y-stepping
  ylab = 0
  
  ; angle shift setup
  if n_elements(shift) eq 0 then shift = 1
  if shift ne 0 then begin
    shift_angle = (shift eq 1) ? 2.4 : 4.159
    xyouts, !x.window[0] + 0.03, !y.window[1] - 0.03, $
      string(shift_angle, $
        format = '("Filters shifted by ", F4.2, " deg")'),  /norm
  endif
  
  for i = 0L, n_elements(good)-1 do begin  
    ; plot a filter
    f_search = where(filters.short_name[good[i]] eq profiles.name, fcount)
    
    if fcount eq 0 then begin 
      ;; Approximate with a 20 ang FWHM Gaussian at the recorded filter center
      fwhm = 20.0
      x = findgen(2000)*0.1 + (filters.center[good[i]] - fwhm*2.0)
      y = gaussian(x, [1, filters.center[good[i]], fwhm / (2 * sqrt(2 * alog(2)))])
    endif else if fcount eq 1 then begin
      x = profiles[f_search[0]].lambda
      y = profiles[f_search[0]].transmission
    endif else begin
      message, 'Oops, more than one name match for "' $
        + filters.short_name[good[i]] + '"'
    endelse 

    if shift ne 0 then begin
      ;; apply wavelength shift to filter due to off axis rays
      ;; assume a cavity index of refraction of 2.1 (emp. fit to Barr H-alpha)

      x_eff = x * sqrt(1 - (1/2.1)^2.0 * sin( (shift_angle * !dtor)^2.0 ))
;      print, filters.short_name[good[i]] + ' shifted by ' + strtrim(avg(x_eff - x), 2)

    endif else x_eff = x    ; No shift applied
      
    oplot, x_eff, y

    ; add an optional label
    ylab = (ylab + 1) mod 4
  
    x_shift = avg(x_eff - x)
    if (labels eq 1) then begin
      xyouts, filters.center[good[i]] + x_shift, 1.0 + 0.02*ylab, filters.short_name[good[i]], $
        align = 0.5
    endif else if (labels eq 2) then begin 
      xyouts, filters.center[good[i]] + x_shift, 1.0 + 0.02*ylab, filters.long_name[good[i]], $
        align = 0.5
    endif
    
  endfor
  
  ; optionally plot lines, deal with scaling
  if keyword_set(thar) then begin
    thorium_argon, thar_l, thar_s
    thar_win = where(!x.crange[0] le thar_l and thar_l le !x.crange[1])
    max_in_win = max(thar_s[thar_win])
    oplot, thar_l, thar_s/max_in_win, color='FF0000'
  endif
  
  if n_elements(uves) ne 0 then begin
    uves_sky, uves_l, uves_s, uves
    uves_win = where(!x.crange[0] le uves_l and uves_l le !x.crange[1])
    max_in_win = max(uves_s[uves_win])
    oplot, uves_l, uves_s/max_in_win, color='red'
  endif
  
  if keyword_set(ps) then begin
    !p.thick = 1
    !p.charthick = 1
  endif

  set_plot,'x'

end
