function read_filters

  files = file_search('/d/wham/pro/filters/profiles/*.txt')
  
  if n_elements(files) eq 0 then $
    message, "No filter profiles found!"

  a_profile = { $
    name: '', $
    lambda: fltarr(246), $
    transmission: fltarr(246) $
  }
    
  profiles = replicate(a_profile, n_elements(files))
  
  for i = 0, n_elements(files)-1 do begin
    p = read_ascii(files[i], data_start=1)
    
    profiles[i].name = file_basename(files[i], '.txt')
    profiles[i].lambda = p.field1[0, 0:245]
    profiles[i].transmission = p.field1[1, 0:245]
  endfor
  
  return, profiles
end