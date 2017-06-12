PRO plotspectrum, vel, data, chop = chop, adu = adu, rey = rey, over = over, $
                  errors = errors, hi = hi, _extra = e

  ;; a nice plot of a WHAM spectrum
  IF NOT KEYWORD_SET(over) THEN over = 0

  IF KEYWORD_SET(chop) THEN $
    mask = indgen(101)+32 $
  ELSE $
    mask = indgen(n_elements(vel))

  IF keyword_set(rey) THEN BEGIN
    IF keyword_set(adu) THEN BEGIN 
      plotdata = data/684.1
    ENDIF ELSE BEGIN 
      plotdata = data/22.8
    ENDELSE 
    yt = 'Intensity [R (km s!u-1!n)!u-1!n]'
  ENDIF ELSE IF KEYWORD_SET(adu) THEN BEGIN
    plotdata = data
    yt = 'Intensity [ADU]'
  ENDIF ELSE IF keyword_set(hi) THEN BEGIN
    plotdata = data
    yt = 'Intensity [deg K]'
  ENDIF ELSE BEGIN 
    plotdata = data
    yt = 'Intensity [ADU s!u-1!n]'
  ENDELSE

;;  IF NOT keyword_set(title) THEN title = ''
;;  IF n_elements(xr) EQ 0 THEN xr = [0, 0]

  IF !d.name EQ 'PS' THEN BEGIN
      thick = 4.0
      xthick = 2.0
      ythick = 2.0
  ENDIF ELSE BEGIN
      thick = 2.0
      xthick = 2.0
      ythick = 2.0
  ENDELSE 
  
  IF KEYWORD_SET(over) THEN BEGIN 
    oplot, vel[mask], plotdata[mask], thick = thick, _extra = e
  ENDIF ELSE BEGIN 
    plot, vel[mask], plotdata[mask], $
          xstyle = 3, ystyle = 3, $
          thick = thick, xthick = xthick, ythick = ythick, charthick = 2.0, $
          xtitle = 'Velocity [km s!u-1!n]', $
          ytitle = yt, _extra = e
  ENDELSE 
 
  IF n_elements(errors) NE 0 THEN BEGIN 
    IF keyword_set(rey) THEN BEGIN 
      IF KEYWORD_SET(adu) THEN BEGIN 
        ploterrors = errors/648.1
      ENDIF ELSE BEGIN 
        ploterrors = errors/22.8
      ENDELSE 
    ENDIF ELSE BEGIN 
      ploterrors = errors
    ENDELSE 

    errplot, vel[mask], $
             plotdata[mask]-ploterrors[mask], plotdata[mask]+ploterrors[mask]
  ENDIF 

END   
