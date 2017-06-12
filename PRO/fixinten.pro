PRO fixinten, map
  ;; Apply intensity corrections to a map. It would be nice to do this
  ;; in log space until the end but the seasonal transmission curve is
  ;; much nicer in non-log space. Note that this routine modifies the
  ;; original data!

  restore, '/d/wham/pro/data/transmission.dat'

  days = map.day
  days = days(uniq(days, sort(days)))

  am = 1/cos(map.zd*!dtor)

  ;; seasonal ext, degrad curve, tune curve
  ta = [0.924, 0.026, 0.12]
  da = -1.03e-4
  pa = [13.258, 0.002407, 12.896955]

  FOR i = 0, n_elements(days)-1 DO BEGIN 
    
    p = where(map.day EQ days[i])
    e = where(ext.day EQ days[i])
    dp = map[p].pbcmd - map[p].pacmd

    IF e[0] NE -1 THEN BEGIN 
      ;; found an explicit correction
      t = exp(ext[e[0]].nslp)
      d = exp(ext[e[0]].degrad)
    ENDIF ELSE BEGIN 
      ;; make it up
      t = ta[0] + ta[1] * cos(2*!pi/365.0*(days[i] - ta[2]))
      d = exp(days[i]*da)
    ENDELSE 
    tune = exp(pa[0] - (pa[1]*dp+pa[2]))

    print, 'Day', days[i]
    print, '   Transmission ', (e[0] EQ -1) ? '(calculated) ' : '', t
    print, '   Degredation (ln) is ',  alog(d)
    print, '   First Tune Corr (ln) is ', alog(tune[0])
    
    corr = (t^(-am[p]))*tune/d
    map[p].data = map[p].data * ((intarr(133)+1.0)#corr)

  ENDFOR  

END 
