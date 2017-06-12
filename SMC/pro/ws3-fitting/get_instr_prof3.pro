PRO get_instr_prof3, file, n_ip, m_ip, w_ip, h_ip

  ; Function to read in parameters from the new instrument profile file format.

  restore, '/d/wham/pro/calibration/ip/ipfile_template.sav'
  ip = read_ascii(file, template=ipfile_template)
  n_ip = n_elements(ip.mean)
  m_ip = ip.mean
  w_ip = ip.fwhm
  h_ip = ip.height

END
