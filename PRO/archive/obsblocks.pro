@predict_vel_calib3

pro obsBlocks, dir, filter
  compile_opt idl2, logical_predicate
  
  ; NOTE: tailered for CTIO right now.
  
;  on_error, 2
  
  if ~dir then message, 'Usage: obsBlocks, directory, [, filter]'
  if ~file_test(dir, /dir) then message, dir + ' is not a directory'
  
  dir_base = file_basename(dir)
  if ~stregex(dir_base, '[0-9]{6}', /boolean) then begin
    date_dirs = file_search(dir, '[0-9][0-9][0-9][0-9][0-9][0-9]', /test_dir)
  endif else begin
    date_dirs = [dir]
  endelse
  
  foreach d, date_dirs do begin
    if ~file_test(d + '/' + filter, /dir) then continue

    blocks = file_search(d + '/' + filter + '/raw', 'b*_1.fts', count = blocks_count)
    if blocks_count eq 0 then continue
    
    print, 'Directory: ' + d + '/' + filter
    print, 'Block', 'Time', 'Long', 'Lat', 'ZD', 'PA', 'PB', 'LSR Vel', 'V_term', $
      format = '(-A6, A5, 2A8, A7, 2A8, A14, A7)'
    print, replicate('=', 71), format = '(80A)'
      
    foreach b, blocks do begin
      h = headfits(b)
      
      exptime = sxpar(h, 'EXPTIME')
      glon = sxpar(h, 'GAL-LON')
      glat = sxpar(h, 'GAL-LAT')
      pacmd = sxpar(h, 'PACMD')/10.0
      pbcmd = sxpar(h, 'PBCMD')/10.0
      vlsr = sxpar(h, 'VLSR')
      zenith_d = sxpar(h, 'ZENITH_D')

      zero = predict_vel_calib3([pacmd, pbcmd], filter, /silent)
      
      print, file_basename(b, '_1.fts'), exptime, glon, glat, zenith_d, pacmd, pbcmd, [-50, 150] - zero - vlsr, term_vel(glon, glat), $
        format = '(-A6, I5, 2F8.2, F7.2, 2F8.2, "  [", I+4, ", ", I+4, "]", F7.1)' 

    endforeach

    print, ' '
      
  endforeach

end
