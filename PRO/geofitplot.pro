PRO geofitplot, block, noline = noline, printer = printer, ftsext = ftsext

  IF keyword_set(noline) THEN line = 1 ELSE line = -1
  IF n_elements(printer) NE 0 THEN BEGIN
      prevdev = !d.name
      set_plot, 'ps'
      device, /land
  ENDIF 

  if ~isa(ftsext) then ftsext = 'ATMSUB'
  readblock, block, map, /extended, ftsext = ftsext
  p = indgen(n_elements(map))+1
  gridpts = indgen(n_elements(map)/5)*5+5
  
  xs = !x.style
  ys = !y.style
  cs = !p.charsize
  !x.style = 3
  !y.style = 3
  !p.charsize = 0.75
  
  x0 = 0.05
  x1 = 0.45
  dy = 0.9/6.0
  y0 = 0.05
  
  plot, p, map.chi2, psym = 1*line, ytitle = 'Chi Sq', $
    xtitle = 'Pointing', $
    position = [x0, y0, x1, y0+dy*1]

  plot, p, map.bkg[0], psym = 1*line, /noerase, ytitle = 'Bkg Constant', $
    xtickformat = '(A1)', $
    position = [x0, y0+dy*1, x1, y0+dy*2]
  plot, p, map.bkg[1], psym = 1*line, /noerase, ytitle = 'Bkg Slope', $
    xtickformat = '(A1)', $
    position = [x0, y0+dy*2, x1, y0+dy*3]
  
  plot, p, map.mean[0], psym = 1*line, /noerase, ytitle = 'Geo Mean', $
    xtickformat = '(A1)', $
    position = [x0, y0+dy*3, x1, y0+dy*4]
  plot, p, abs(map.width[0]), psym = 1*line, /noerase, ytitle = 'Geo Width', $
    xtickformat = '(A1)', $
    position = [x0, y0+dy*4, x1, y0+dy*5]
  oplot, !x.crange, [3.0, 3.0], line = 1
  oplot, !x.crange, [7.0, 7.0], line = 1
  plot, p, map.area[0], psym = 1*line, /noerase, ytitle = 'Geo Area', $
    xtickformat = '(A1)', $
    position = [x0, y0+dy*5, x1, y0+dy*6]

  gridnorm = (convert_coord(gridpts, gridpts*0, /to_normal))[0, *]
  FOR i = 0, n_elements(gridnorm)-1 DO BEGIN $
    plots, [gridnorm[i], gridnorm[i]], [y0, y0+dy*6], /norm, line = 1
  ENDFOR 
  
  x0 = 0.55
  x1 = 0.95
  dy = 0.9/5.0
  y0 = 0.05

  gpsym = [1, 7, 4, 6, 7]

  blank = map.area[1:*]*0.0
  plwhich = where(map.area[1:*] NE blank, howmany)

  IF howmany NE 0 THEN BEGIN
      yr = [min((map.mean[1:*])[plwhich]), $
            max((map.mean[1:*])[plwhich])]
      plot, p, map.mean[1], psym = gpsym[0], /noerase, $
        ytitle = 'Gal Mean', xtitle = 'Pointing', $
        position = [x0, y0, x1, y0+dy*1], yrange = yr, symsize = 0.5
      xmean = !x
      ymean = !y
      
      yr = [min((abs(map.width[1:*]))[plwhich]), $
            max((abs(map.width[1:*]))[plwhich])]
      plot, p, abs(map.width[1]), psym = gpsym[0], /noerase, $
        ytitle = 'Gal Width', $
        xtickformat = '(A1)', $
        position = [x0, y0+dy*1, x1, y0+dy*2], yrange = yr, symsize = 0.5
      xwidth = !x
      ywidth = !y
      
      yr = [min((map.area[1:*])[plwhich]), $
            max((map.area[1:*])[plwhich])]
      plot, p, map.area[1], psym = gpsym[0], /noerase, $
        ytitle = 'Gal Area', $
        xtickformat = '(A1)', $
        position = [x0, y0+dy*2, x1, y0+dy*3], yrange = yr, symsize = 0.5
      xarea = !x
      yarea = !y
      
      blank = fltarr(n_elements(map))
      
      FOR i = 2, n_elements(gpsym) DO BEGIN
          plwhich = where(map.area[i] NE blank, howmany)
          IF howmany NE 0 THEN BEGIN
              !x = xmean
              !y = ymean
              plots, p[plwhich], map[plwhich].mean[i], $
                psym = gpsym[i-1], symsize = 0.5
              !x = xwidth
              !y = ywidth
              plots, p[plwhich], abs(map[plwhich].width[i]), $
                psym = gpsym[i-1], symsize = 0.5
              !x = xarea
              !y = yarea
              plots, p[plwhich], map[plwhich].area[i], $
                psym = gpsym[i-1], symsize = 0.5
          ENDIF 
      ENDFOR
  ENDIF
  
  FOR i = 0, n_elements(gpsym)-1 DO BEGIN
      plots, 0.47, y0+dy*1.7-0.02*i, psym = gpsym[i], /norm
      xyouts, 0.48, y0+dy*1.7-0.02*i-0.003, '= ' + strtrim(i+1, 2), $
        /norm, align = 0
  ENDFOR 

  gridnorm = (convert_coord(gridpts, gridpts*0, /to_normal))[0, *]
  FOR i = 0, n_elements(gridnorm)-1 DO BEGIN $
    plots, [gridnorm[i], gridnorm[i]], [y0, y0+dy*3], /norm, line = 1
  ENDFOR 

  plot, map[0].vel, avg(map.data, 1), /noerase, $
    xtitle = 'Velocity', ytitle = 'ADU', $
    position = [x0, y0+dy*3 + 0.05, x1, y0+dy*5]

  date = (stregex(map[0].date, '([0-9]+-[0-9]+-[0-9]+)T', /extr, /subexp))[1]
  blockname = (stregex(map[0].name, 'b([0-9]+)_', /extr, /subexp))[1]
  
  title = 'Block #' + blockname + ' on ' + date + '   (' + $
    string(avg(map.glon), format = '(F6.2)') + ',' + $
    string(avg(map.glat), format = '(F6.2)') + ')'
  xyouts, 0.5, 0.98, title, /norm, charsize = 1.0, align = 0.5

  !x.style = xs
  !y.style = ys
  !p.charsize = cs

  IF n_elements(printer) NE 0 THEN BEGIN
      device, /close
      set_plot, prevdev
      spawn, 'lpr -P'+printer+' idl.ps'
      print, 'Output sent to printer ' + printer
  ENDIF 
  
END
  
