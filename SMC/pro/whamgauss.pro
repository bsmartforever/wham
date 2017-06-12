FUNCTION whamgauss, vel, params, nbk, ip = ip, wsdir = wsdir

  IF NOT keyword_set(ip) THEN ip = '3gauss_ip.dat'
  
  if N_ELEMENTS(wsdir) eq 0 then wsdir = "/d/wham/lib/whamspect2"

  IF strpos(!path, wsdir) EQ -1 THEN $
    !path = wsdir + "/idl:" + !path
  forward_function fit_helper
  
  get_instr_prof, wsdir + '/ip/' + ip, $
    num_ip, mean_ip, width_ip, height_ip

  params = fit_helper(params, nbk = nbk, mask = 2L^(n_elements(params))-1, $
                      num_ip = num_ip, mean_ip = mean_ip, $
                      width_ip = width_ip, height_ip=height_ip, /setup)

  gauss_ip, vel, params, fit

  return, fit
END 
