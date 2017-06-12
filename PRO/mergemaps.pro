function mergemaps, a, b
  compile_opt idl2

  ;; figure out which map has bigger arrays
  if n_elements(a[0].vel) gt n_elements(b[0].vel) then abigger = 1 else abigger = 0
  
  if abigger then new_element = a[0] else new_element = b[0]
  
  new_map = replicate(new_element, n_elements(a) + n_elements(b)) 
  
  if abigger then begin
    bstart = n_elements(a)
    new_map[0:bstart-1] = a
    for i=0, n_elements(b)-1 do begin
      struct_assign, b[i], new_element 
      new_map[bstart + i] = new_element
    endfor
  endif else begin
    astart = n_elements(b)
    new_map[0:astart-1] = b
    for i=0, n_elements(a)-1 do begin
      struct_assign, a[i], new_element 
      new_map[astart + i] = new_element
    endfor
  endelse
  
  for i=0, n_elements(new_map)-1 do begin
    w = where(new_map[i].vel eq 0 and new_map[i].data eq 0, cnt)
    if cnt ne 0 then begin
      new_map[i].vel[w] = !values.f_nan
      new_map[i].data[w] = !values.f_nan
      new_map[i].var[w] = !values.f_nan
    endif
  endfor

  return, new_map

end
