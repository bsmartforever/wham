PRO plotpointing, pointing, rey = rey, adu = adu, over = over, $
                  font = font, intlabel = intlabel, $
                  title = title, loclabel = loclabel, chop = chop, $
                  scale = scale, hi = hi, errbars = errbars, _extra = e

  IF NOT keyword_set(font) THEN font = '!3'
  IF NOT KEYWORD_SET(rey) THEN rey = 0
  IF NOT KEYWORD_SET(hi) THEN hi = 0
  IF NOT KEYWORD_SET(over) THEN over = 0
  IF KEYWORD_SET(chop) THEN $
    mask = indgen(101)+32 $
  ELSE $
    mask = indgen(n_elements(pointing.vel))
  if keyword_set(errbars) then $
    errors = sqrt(pointing.var[mask])
  
  xyouts, 0, 0, font
  
  IF NOT keyword_set(title) THEN $
    title = font+'!12l'+font+' = ' + $
            string(pointing.glon, format = '(F6.2)') + $
            ',  !18b'+font+' = ' + $
            string(pointing.glat, format = '(F6.2)')
  
  plotspectrum, pointing.vel[mask], pointing.data[mask], over = over, scale = scale, $
                adu = adu, rey = rey, hi = hi, title = title, errors = errors, _extra = e
 
  IF NOT over THEN BEGIN 
    xr = !x.crange[1]-!x.crange[0]
    yr = !y.crange[1]-!y.crange[0]
    IF NOT keyword_set(scale) THEN $
      scale = 1/float((1 > (!p.multi(1) < !p.multi(2))))

    IF keyword_set(intlabel) THEN BEGIN
      IF keyword_set(hi) THEN BEGIN 
        i = intspect(pointing, pointing.vel[0], $
                     pointing.vel[n_elements(pointing.vel)-1], /hi) $
            * 1.8224e18
        ifmt = '("I = ",E10.2," cm!u-2")'
      ENDIF ELSE BEGIN 
        i = intspect(pointing, pointing.vel[mask[0]], $
                     pointing.vel[mask[N_ELEMENTS(mask)-1]])
        
        IF keyword_set(rey) THEN BEGIN
          IF KEYWORD_SET(adu) THEN BEGIN 
            i = i/684.1
          ENDIF ELSE BEGIN 
            i = i/22.8
          ENDELSE 
          ifmt = '("I = ",F6.2," R")'
        ENDIF ELSE BEGIN
          IF KEYWORD_SET(adu) THEN BEGIN 
            ifmt = '("I = ",F10.2," ADU x km s!u-1!n")'
          ENDIF ELSE BEGIN 
            ifmt = '("I = ",F10.2," ADU s!u-1!n x km s!u-1!n")'
          ENDELSE 
        ENDELSE 
        
      ENDELSE 

      xyouts, xr*0.05+!x.crange[0], yr*0.95+!y.crange[0], $
              string(i, format = ifmt), charsize = scale, charthick = 2.0
    ENDIF 

    IF keyword_set(loclabel) THEN BEGIN
      xyouts, xr*0.05+!x.crange[0], yr*0.95+!y.crange[0], $
              (n_elements(intlabel) EQ 1 ? '!c' : '') + $
              font+'!12l'+font+'!d !n = ' + $
              string(pointing.glon, format = '(F6.2)') + string(176B) + $
              '!c!18b'+font+' = ' + $
              string(pointing.glat, format = '(F6.2)') + string(176B), $
              charsize = scale, charthick = 2.0
    ENDIF
  ENDIF 
END
   
