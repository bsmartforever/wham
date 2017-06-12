pro wset_or_create, win, _extra = extra
  on_error, 2

  ;; get list of open windows
  device, window_state = state
  
  if win ge n_elements(state) then $
    message, 'Window number too large for device (max ' $
      + strtrim(n_elements(state)-1, 2) + ')'
      
  if state[win] then $
    wset, win $
  else $
    window, win, _extra = extra

end