FUNCTION whamgauss, vel, params, nbk, ip = ip

  IF NOT keyword_set(ip) THEN ip = '3gauss_ip.dat'

  IF strpos(!path, "$HOME/WHAM/Bridge/data/ha/110105/") EQ -1 THEN $
    !path = "$HOME/WHAM/Bridge/data/ha/110105/:" + !path
  forward_function fit_helper
  
;  IF strpos(!path, "$HOME/WHAM/Bridge/data/ha/101030/") EQ -1 THEN $
;    !path = "$HOME/WHAM/Bridge/data/ha/101030/:" + !path
;  forward_function fit_helper
  
;  get_instr_prof, '$HOME/WHAM/Bridge/data/ha/101030/' + ip, $
;    num_ip, mean_ip, width_ip, height_ip

  get_instr_prof, '$HOME/WHAM/Bridge/data/ha/110105/' + ip, $
    num_ip, mean_ip, width_ip, height_ip


  params = fit_helper(params, nbk = nbk, mask = 2L^(n_elements(params))-1, $
                      num_ip = num_ip, mean_ip = mean_ip, $
                      width_ip = width_ip, height_ip=height_ip, /setup)

  gauss_ip, vel, params, fit

  return, fit
END 
