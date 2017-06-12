FUNCTION mk_wham_gauss, X, Y_GAUSS=Y_GAUSS, p=p
  
  ;; NOTE that this function isn't actually used in multi_gauss.pro, but 
  ;; I'm keeping it here as more of a note to self on how to extract the 
  ;; WHAM IP then convolve it with a Gaussian so that it matches the 
  ;; velocity resolution of WHAM.

  ;; A Gaussian function convolved with WHAM IP
  ;; This well samples the ~12 km/s width
  ;; Have to convert width to sigma, height to area

  ; p = [constant, center, width, area]

  ip_name = '/d/wham/lib/whamspect2/ip/standard_ip.dat'
  Get_Instr_Prof, ip_name, num_ip, center_ip, width_ip, height_ip
  ; ip_name = '/d/wham/lib/whamspect2/ip/smorn_ip.dat'
  ; Choose 100. unless 100. gt n_elements(x)
  ; The kernal must have n_elements(x) or smaller
  num=(100. gt n_elements(x)) ? n_elements(x) : 100.
  Kern = DBLARR(num)
  ; The spacing makes sure that the Gaussian has the same sampling spacing as X
  spacing=(max(x)-min(x))/n_elements(x)
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(num+1)-num/2., [Center_Ip[i], (Width_Ip[i]/2.3548)/spacing, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR

  if (NOT keyword_set(Y_GAUSS)) then $
    RETURN,  CONVOL(P[0]+GAUSS1(X, P[1:3]), Kern, /CENTER, /EDGE_TRUNCATE) $
  else RETURN,  CONVOL(Y_GAUSS, Kern, /CENTER, /EDGE_TRUNCATE)
  
END