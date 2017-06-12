pro read_ms, ms, save = save

  ms = readblocklist('blocks.ha.end.txt', ext='ATMSUB')
  if KEYWORD_SET(save) then save, ms, filename = '/d/wham/bsmart/MS/ms_end.dat', /compress

    min_l = min(ms.glon, max = max_l)
    min_b = min(ms.glat, max = max_b)
    ms_hi = exthi(min_l, max_l, min_b, max_b, $
      vmin = -400, vmax = -200, DATADIR = '/d/data/hi/LAB', /wham)
    if KEYWORD_SET(save) then save, ms_hi, filename = '/d/wham/bsmart/MS/ms_end_hi.dat', /compress
  
end
