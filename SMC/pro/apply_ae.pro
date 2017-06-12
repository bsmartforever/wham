FUNCTION apply_ae, inmap, $
  no_degrad = no_degrad, reverse = reverse, no_lookup = no_lookup, $
  ta = ta, da = da, da0 = da0, extra_days = extra_days

  ;; NOTE: only handles CTIO data right now. Needs some tweaks to handle KPNO transmissions.

  ;; Apply intensity corrections to a map. It would be nice to do this
  ;; in log space until the end but the seasonal transmission curve is
  ;; much nicer in non-log space.

  ;; seasonal ext, degrad curve, tune curve parameters

  ;  ta = [exp(-0.0795), 0, 0]
  ;  da = -3.65e-5
  ;  da0 = 0
  ;  da0 = parsedate('090101') - day0

  ;;  early CITO 09/30/15 anaylsis:
  ;  ta = [exp(-0.0704), 0.01, 40.0]  ;; Peak on Feb 10
  ;  if keyword_set(no_degrad) then da = [0.0, 0.0] else da = [-8.0e-3, -1.1e-4]
  ;  da0_days = [parsedate('090424'), parsedate('091110')] - day0 ;; range of days where da[0] is valid

  ;; KPNO values
  ; ta = [0.924, 0.026, 0.12]
  ;  da = -1.03e-4
  ;  pa = [13.258, 0.002407, 12.896955]


  ;; 04/27/16 & 09/14/16 CTIO analysis *CURRENT*
  ;;    - no global seasonal variation seen in these iterations
  ;;    - slightly higher than average transmisison around 12/2009 and 12/2015
  ;;      may be related to el Niño; not accounted for (yet?)

  if ~isa(ta) then ta = [exp(-0.0795), 0, 0]
  if ~isa(da) then da = -3.65e-5
  if ~isa(da0) then da0 = 0
  if isa(extra_days) then begin
    if ((extra_days.dim)[0] ne 2) || (extra_days.ndim ne 2 && extra_days.ndim ne 1) then begin
      message, 'EXTRA_DAYS must be a [2, N] array: [[d1, slp], [d2, slp], [d3, slp] ...]; nothing done.', /info
      return, !null
    endif
  endif

  ;; a transmission without a seasonal component was passed in
  if ta.length eq 1 then ta = [ta, 0, 0]

  jdcnv, 1997, 1, 1, 0, day0

  days = inmap.day

  ;; any observations with UTC near and before midnight should be bumped to the next day.
  close = where(inmap.time ge 22.0)
  days[close]++

  if where(days le 4442, /null) ne !null then begin
    message, 'Corrections only for CTIO right now; no map returned', /info
    return, !null
  endif

  ;; make a copy of the input map before making changes
  map = inmap
  am = zd2airmass(map.zd)

  unity = intarr(n_elements(map[0].data)) + 1.0

  if keyword_set(no_lookup) then begin

    ;; User wants a functional transmission applied across the whole map; don't need 
    ;;   to look up daily transmission parameters.

    t = ta[0] + ta[1] * cos(2*!pi / 365.0 * (days - ta[2]))
    d = keyword_set(no_degrad) ? 1.0 : exp((days - da0) * da)

    print, '   Avg. Transmission (calculated) ', t.mean()
    if ~keyword_set(no_degrad) then  print, '   Min/Max Degradation is ', d.min(), d.max()

    ;    print, '   First Tune Corr (ln) is ', alog(tune[0])

    corr = (t^(-am))/d

    if keyword_set(reverse) then begin
      print, 'Transmission corrections removed.'
      map.data = map.data / (unity # corr)
      map.var = map.var / (unity # (corr^2.0))
    endif else begin
      map.data = map.data * (unity # corr)
      map.var = map.var * (unity # (corr^2.0))
    endelse

  endif else begin

    ;; lookup/compute transmission for each unique day in map
    restore, '/d/wham/pro/calibration/transmission/transmission.sav'
    days = days(uniq(days, sort(days)))

    FOR i = 0, n_elements(days)-1 DO BEGIN

      ;; select all pointings with this day
      p = where(map.day EQ days[i] or ((map.day eq days[i]-1) and (map.time ge 22.0)))

      ;; check extra_days for a user-supplied correction first
      if isa(extra_days) then begin
        e_day = where(extra_days[0, *] eq days[i], edcount)
        if edcount ne 0 then begin
          t = exp(extra_days[1, e_day[0]])
        endif
      endif else edcount = 0

      ;; If nothing from extra_days, check the transmission table
      if edcount eq 0 then begin
        e = where(ext.day EQ days[i], ecount)

        ;    dp = map[p].pbcmd - map[p].pacmd

        IF ecount ne 0 THEN BEGIN
          ;; found an explicit transmission correction
          t = exp(ext.nslp[e[0]])
        ENDIF ELSE BEGIN
          ;; use average behavior to estimate transmission
          t = ta[0] + ta[1] * cos(2*!pi/365.0*(days[i] - ta[2]))
        ENDELSE

        ;    tune = exp(pa[0] - (pa[1]*dp+pa[2]))
      endif

      ;; 04/27/16 degrad analysis
      d = keyword_set(no_degrad) ? 1.0 : exp((days[i]-da0)*da)

      ;; Needed for 09/30/15 analysis
      ;    if (da0_days[0] le days[i]) and (days[i] le da0_days[1]) then begin
      ;      d = exp(da[0])  ;; the degradation shows no appreciable slope in this 2009 window
      ;    endif else begin
      ;      d = exp((days[i]-da0_days[1])*da[1])
      ;    endelse

      print, 'Day', days[i]
      print, '   Transmission ', (e[0] EQ -1) ? '(calculated) ' : '', t
      if ~keyword_set(no_degrad) then  print, '   Degradation is ', d

      ;    print, '   First Tune Corr (ln) is ', alog(tune[0])

      corr = (t^(-am[p]))/d
      ;   map[p].data *= corr

      if keyword_set(reverse) then begin
        print, 'Transmission corrections removed.'
        map[p].data = map[p].data / (unity # corr)
        map[p].var = map[p].var / (unity # (corr^2.0))
      endif else begin
        map[p].data = map[p].data * (unity # corr)
        map[p].var = map[p].var * (unity # (corr^2.0))
      endelse

    ENDFOR
  endelse

  return, map

END
