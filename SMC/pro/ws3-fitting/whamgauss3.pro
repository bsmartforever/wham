FUNCTION whamgauss3, vel, params, nbk, ip = ip, dir = dir

  IF NOT keyword_set(ip) THEN ip = 'kpno_3gauss.txt'
  if not keyword_set(dir) then dir = '/d/wham/pro/calibration/ip/'
  
  forward_function fit_helper
  
  get_instr_prof3, dir + ip, num_ip, mean_ip, width_ip, height_ip

  params = fit_helper(params, nbk = nbk, mask = 2L^(n_elements(params))-1, $
                      num_ip = num_ip, mean_ip = mean_ip, $
                      width_ip = width_ip, height_ip=height_ip, /setup)

  gauss_ip, vel, params, fit

  return, fit
END 
