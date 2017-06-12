PRO WHAM_MPFIT, vel, data, var, Calib, Guess, func_str, parinfo = parinfo
  
;E.g.,  WHAM_MPFIT, vel, data, var, 1., [0.0025,150.,30,1.], 'MYFUNC_IP', Parinfo = parinfo

  KPLOT, vel, data, yerr = SQRT(var), PSYM = 2
  
  Fit = MPFITFUN(func_str, vel, data, SQRT(var), Guess, PARINFO = parinfo, $
                 BestNorm = BestNorm, Covar = Covar, Dof = Dof, $
                 PError = PError, YFit = YFit, /quiet)
  ;; GAUSS1 uses Sigma rather than FWHM
  ;; FWHM = 2*SQRT(2*ln(2)) = 2.3548*sigma

  ;; Gives 99% confidence intervals
  PRINT, 'V: ', Fit[1], '  +/-', T_CVF((1-.99)/2, DoF)*PError[1]*SQRT(BestNorm/DOF), $
         FORMAT = '(A,I7,A,I4)'
  PRINT, 'W: ', Fit[2]*2.3548, '  +/-', T_CVF((1-.99)/2, DoF)*PError[2]*SQRT(BestNorm/DOF)*2.3548, $
         FORMAT = '(A,F7.1,A,F10.1)'
  PRINT, 'A: ', Fit[3]/Calib, '  +/-', T_CVF((1-.99)/2, DoF)*PError[3]*SQRT(BestNorm/DOF)/Calib, $
         FORMAT = '(A,F7.3,A,F10.3)'

  ;PRINT, 'V: ', Fit[4], '  +/-', T_CVF((1-.99)/2, DoF)*PError[4]*SQRT(BestNorm/DOF), $
  ;       FORMAT = '(A,I7,A,I4)'
  ;PRINT, 'W: ', Fit[5]*2.3548, '  +/-', T_CVF((1-.99)/2, DoF)*PError[5]*SQRT(BestNorm/DOF)*2.3548, $
  ;       FORMAT = '(A,F7.1,A,F10.1)'
  ;PRINT, 'A: ', Fit[6]/Calib, '  +/-', T_CVF((1-.99)/2, DoF)*PError[6]*SQRT(BestNorm/DOF)/Calib, $
  ;       FORMAT = '(A,F7.3,A,F10.3)'
  
  PRINT, 'Chi^2 = ', SQRT(BestNorm/DOF)
  
  OPLOT, vel, MYFUNC_IP(vel, Fit)
  
END


FUNCTION MYFUNC, X, P
  
  RETURN,  P[0]+GAUSS1(X, P[1:3])
  
END
FUNCTION MYFUNC2, X, P
  
  RETURN,  P[0]+GAUSS1(X, P[1:3])+GAUSS1(X, P[4:6])
  
END

FUNCTION MYFUNC_IP, X, P
  
  ;; A MYFUNC with WHAM IP
  ;; Fixes the Kernel x-array to have total width of 50 km/s
  ;; This well samples the ~12 km/s width
  ;; Have to convert width to sigma, height to area
  ip_name = '/d/wham/lib/whamspect2/ip/smorn_ip.dat'
  Get_Instr_Prof, ip_name, num_ip, mean_ip, width_ip, height_ip
  num=(100. gt n_elements(x)) ? n_elements(x) : 100.
  Kern = DBLARR(num)
  spacing=(max(x)-min(x))/n_elements(x)
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(num+1)-num/2., [Mean_Ip[i], (Width_Ip[i]/2.3548)/spacing, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR
  
  RETURN,  CONVOL(P[0]+GAUSS1(X, P[1:3]), Kern, /CENTER, /EDGE_TRUNCATE)
  
END

FUNCTION MYFUNC2_IP, X, P
  
  ;; A MYFUNC with WHAM IP
  ;; Fixes the Kernel x-array to have total width of 50 km/s
  ;; This well samples the ~12 km/s width
  ;; Have to convert width to sigma, height to area
  ip_name = '/d/wham/lib/whamspect2/ip/smorn_ip.dat'
  Get_Instr_Prof, ip_name, num_ip, mean_ip, width_ip, height_ip
  num=(100. gt n_elements(x)) ? n_elements(x) : 100.
  Kern = DBLARR(num)
  spacing=(max(x)-min(x))/n_elements(x)
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(num+1)-num/2., [Mean_Ip[i], (Width_Ip[i]/2.3548)/spacing, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR
  
  RETURN,  CONVOL(P[0]+GAUSS1(X, P[1:3])+GAUSS1(X, P[4:6]), Kern, /CENTER, /EDGE_TRUNCATE)
  
END
