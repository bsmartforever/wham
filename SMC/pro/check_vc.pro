pro check_vc, dir

  if n_elements(dir) eq 0 then dir = '.'

  ;; find blocks for this date
  blocks = findfile(dir + '/combo/b*_1.fts', count = bcount)
  
  IF (bcount EQ 0) THEN BEGIN
    print, '  No blocks'
  ENDIF ELSE BEGIN
    
    FOR k = 0, n_elements(blocks)-1 DO BEGIN
      
      ;; extract the blockname
      last_slash = rstrpos(blocks[k], "/")
      last_pos = rstrpos(blocks[k], "_")
      
      base = strmid(blocks[k], last_slash+1, last_pos-last_slash-1)
      
      print, '  Checking ' + base

      readfspe, dir + '/combo/' + base + '_1.fts', vr, dr, sr, ext = 'rawspec'
      readfspe, dir + '/combo/' + base + '_1.fts', vp, dp, sp, ext = 'procspec'

      if (vr[0]-vp[0]) ge 10.0 then print, 'BAD!!!'
 
    endfor 
  endelse 
end 
