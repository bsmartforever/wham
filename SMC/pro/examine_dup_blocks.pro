pro plot_dup_blocks, map1, map2

  both = [map1, map2]

  xrange = [min(both.vel), max(both.vel)]

  names = (both.name).uniq()
  p_numbers = names.extract('_([0-9]+)', /sub)
  pn_sort = sort((p_numbers[1, *]).toInteger())
  names = names[pn_sort]

  foreach p_name, names do begin
    
    p1_i = where((map1.name).matches('^' + p_name + '$'), c1)
    p2_i = where((map2.name).matches('^' + p_name + '$'), c2)

    if c1 eq 0 then begin
      message, p_name + ' is missing in first map; skipping.', /info
      continue
    endif

    if c2 eq 0 then begin
      message, p_name + ' is missing in second map; skipping.', /info
      continue
    endif

    p1 = map1[p1_i]
    p2 = map2[p2_i]
    
    IF abs(p1.vel[0]-p2.vel[0]) GE 2 THEN BEGIN
      print, 'Spectra offset by ', p1.vel[0]-p2.vel[0]
    ENDIF

    yrange = [min([p1.data, p2.data]), max([p1.data, p2.data])]
    plot, p1.vel, p1.data, xrange = xrange, yrange = yrange, $
      position = [0.1, 0.3, 0.9, 0.9], /nodata, $
      xstyle = 7, title = file_basename(p1.name, '.fts')
    axis, xaxis = 0, xticks = 1, xminor = 1, xtickname = [' ', ' ']
    axis, xaxis = 1, xstyle = 3, xrange = xrange
    oplot, p1.vel, p1.data, color = color(128)
    oplot, -([p1.vlsr, p1.vlsr] + 2.33), !y.crange, line = 1, color = color(128)
    oplot, p2.vel, p2.data, color = color(150)
    oplot, -([p2.vlsr, p2.vlsr] + 2.33), !y.crange, line = 1, color = color(150)
    xyouts, 0.12, 0.88, (strsplit(p1.date, "T", /extract))[0], $
      align = 0, color = color(128), /norm
    xyouts, 0.88, 0.88, (strsplit(p2.date, "T", /extract))[0], $
      align = 1, color = color(150), /norm

    sp_avg = sparith([p1, p2], /average)
    oplot, sp_avg.vel, sp_avg.data, thick = 2
    oplot, sp_avg.vel, sp_avg.vel*0, line = 1

    sp_diff = sparith(p1, p2, /sub)
    plot, sp_diff.vel, sp_diff.data, xrange = xrange, $
      position = [0.1, 0.1, 0.9, 0.3], $
      xstyle = 3, ystyle = 7, /nodata, /noerase
    axis, yaxis = 0, yticks = 1, yminor = 1, ytickname = [' ', ' ']
    axis, yaxis = 1, ystyle = 3
    oplot, sp_diff.vel, sp_diff.data, color = color(200)
    oplot, sp_avg.vel, sp_avg.vel*0, line = 1, color = color(200)

    garb = get_kbrd(1)

    if garb eq 'Q' or garb eq 'q' then retall 
    if garb eq 'n' or garb eq 'N' then break
  endforeach

end

pro examine_dup_blocks, map

  @mycmap
  founddup = 0

  blocks = where(strpos(map.name, '_1.fts') ne -1)

  foreach b, blocks, i do begin
    w = where(map[b].name eq map[blocks[i:*]].name, c)

    if c gt 1 then begin
      founddup++
      block_name = file_basename(map[blocks[w+i]].name, '1.fts')
      block_day = map[blocks[w+i]].day
      
      print, block_name[0], map[blocks[w[0]+i]].date, $
        format = '(%"%8s : %24s")'
      print, block_name[1], map[blocks[w[1]+i]].date, $
        format = '(%"%8s : %24s")'
      
      b1 = map[where(strpos(map.name, block_name[0]) ne -1 and map.day eq block_day[0])]
      b2 = map[where(strpos(map.name, block_name[1]) ne -1 and map.day eq block_day[1])]
      
;      if b1.length ne b2.length then begin
;        message, 'Blocks are not equal length (missing pointing(s)); skipping.', /info
;        continue
;      endif
      
      plot_dup_blocks, b1, b2
      
      print

    endif

  endforeach

  if founddup eq 0 then begin
    message, 'No duplicate blocks found in map.', /info
    return
  endif

end