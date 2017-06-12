PRO calclc, inmap, dist, center = center, radius = radius, $
            background = background, vmin = vmin, vmax = vmax, $
            iscale = iscale, oplot = oplot

  r2ph = 1e6/(4*!pi)
  alpha = 3.1e-13 ;; 8000K case B
  IF n_elements(background) EQ 0 THEN background = 0 ;; WIM background in R
  IF n_elements(vmin) EQ 0 THEN vmin = -100
  IF n_elements(vmax) EQ 0 THEN vmax = +100
  IF n_elements(iscale) EQ 0 THEN iscale = 1.00 ;; arbitrary intensity scaling
  IF n_elements(radius) EQ 0 THEN radius = 0

  IF radius NE 0 THEN BEGIN 
    dvect = sphdist(inmap.glon, inmap.glat, center[0], center[1], /deg)
    sel = where(dvect LE radius)
    maxdvect = max(dvect[sel])
    map = inmap[sel]
  ENDIF ELSE BEGIN 
    map = inmap
  ENDELSE 

  IF keyword_set(oplot) THEN $
    plots, -map.glon, map.glat, psym = 1
  
  imap = intmap(map, vmin = vmin, vmax = vmax, moment=1) - background
  sum = total(imap)*iscale

  print, 'Total integrated intensity: ', sum, ' R'

  wham_sr = 2 * !pi * (1 - cos(0.5 * !dtor))
  lc = 4.0 * !pi * (dist*3.086d18)^2.0 / 0.47 * sum * r2ph * wham_sr

  print, 'L_c: ', lc, ' ph s^-1       log L_c: ', alog10(lc)

;  print, 'L_c: ', 0.48*sum*diam^2.0, ' x 10^46 ph s^-1'
;  print, 'n_e: ', sqrt(2.32*sum/diam), ' cm^-3'

  IF radius NE 0 THEN BEGIN 
    diam = 2*tan(maxdvect*!dtor)*dist ;; diameter of region
    nelec = sqrt(lc/alpha * (6/!pi * 1.0/(diam*3.086d18)^3.0))
    
    print, 'n_e: ', nelec, ' cm^-3'

    covcorr = (2*!pi*(1.0 - cos(maxdvect * !dtor))) / (n_elements(sel) * wham_sr)

    print, 'Coverage correction for ', strtrim(n_elements(sel),2), ' WHAM beams: ', covcorr

    print, 'L_c (c): ', lc*covcorr, ' ph s^-1       log L_c: ', alog10(lc*covcorr)
    print, 'n_e (c): ', nelec*sqrt(covcorr), ' cm^-3'
  ENDIF     

END
  
