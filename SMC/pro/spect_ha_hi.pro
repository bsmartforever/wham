pro spect_ha_hi, ha, hi, which
  if n_elements(xs) eq 0 then xs = 400
  if n_elements(ys) eq 0 then xs = 400
  
  if n_elements(png) ne 0 then begin
    curwin = !d.window
    window, xsize = xs, ysize = xs, 5
  endif
  
  @mycmap
  
  !p.charsize = 1.2
  xrange = [min(ha.vel), max(ha.vel)]
  xmargin = [10, 10]
  hi_color = 50
  
  hi_pt = spectnear(hi, ha[which].glon, ha[which].glat, 0.5, hi_count)
  hi_avg_vel = hi[hi_pt[0]].vel
  hi_avg_data = avg(hi[hi_pt].data, 1)

   scale = max(ha[which].data[where(xrange[0] le ha[which].vel and ha[which].vel le xrange[1])]) $
    / max(hi_avg_data[where(xrange[0] le hi_avg_vel and hi_avg_vel le xrange[1])])

  yrange = [0, max(hi_avg_data)]

  plot, ha[which].vel, ha[which].data/22.8, xmargin = xmargin, $
    xrange = xrange, xstyle = 1, ystyle = 11, yrange =yrange*scale/4
  oplot, !x.crange, [0, 0], line = 2


  oplot, hi_avg_vel, hi_avg_data*scale, color = color(hi_color)
  axis, !x.crange[1], yaxis = 1, yrange = !y.crange/scale, color = color(hi_color), $
    ystyle = 3
  
  xyouts, 0.20, 0.9, $
    '!12l!3 = ' + string(ha[which].glon, format = '(F6.2)') + $
    ', !18b!3 = ' + string(ha[which].glat, format = '(F6.2)'), $
    align = 0.0, /norm
  xyouts, 0.05, 0.5, 'ADU/sec', align = 0.5, orient = 90, /norm
  xyouts, 0.95, 0.5, 'T [K]', align = 0.5, orient = -90, /norm, color = color(hi_color)
  
  !p.charthick = 10
  !p.charsize = 10
  !p.thick = 10
  
  if n_elements(png) ne 0 then begin
    write_png, png, tvrd(/true)
    wset, curwin
  endif
  
end
