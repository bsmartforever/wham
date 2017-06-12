pro print_span, span
  num = n_elements(span)
  print, span[0].sysclock, (span[num-1].sysclock_s - span[0].sysclock_s) + 1, $
    span.fw_valid, span.fw_pos, $
    format = '("Start: ", C(), "/ Duration:", I4, " s / Valid: ", ' $
         + strtrim(num,2) + '(I1) / "  Position: ", ' $
         + strtrim(num,2) + '(I3))'
end 
  
pro fw_move_analysis, logfile

  ics = read_icslog(logfile)
  
  n = n_elements(ics)
  
  moving_cw = where(ics.fw_moving_cw eq 1, cw_count)
  moving_ccw = where(ics.fw_moving_ccw eq 1, ccw_count)
  
 if cw_count ne 0 then begin
    print, "Clockwise Motion:"
    start = moving_cw[0]
    last = start
    for i=1, n_elements(moving_cw)-1 do begin
      cur = moving_cw[i]
      if (cur - last) gt 1 then begin
        ;; end a move span
        print_span, ics[start:last]
        start = cur
      endif
      
      last = cur
    endfor
    
    ;; print the last span
    print_span, ics[start:last]
  endif
  
  if ccw_count ne 0 then begin
    print, "Counterclockwise Motion:"
    start = moving_ccw[0]
    last = start
    for i=1, n_elements(moving_ccw)-1 do begin
      cur = moving_ccw[i]
      if (cur - last) gt 1 then begin
        ;; end a move span
        print_span, ics[start:last]
        start = cur
      endif
      
      last = cur
    endfor
    
    ;; print the last span
    print_span, ics[start:last]
  endif

end  
