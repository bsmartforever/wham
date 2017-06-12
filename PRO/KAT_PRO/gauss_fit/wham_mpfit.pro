@~/wham/pro/mpfit
@~/wham/pro/mpfitfun
@~/wham/pro/mpfitexpr
@~/wham/pro/gauss1
@~/wham/pro/kplot
@~/wham/pro/magstream
PRO FIT_STREAM
  
  dir = '/Users/greg/wham/Mag_Stream/Results/'
  
  READCOL, dir+'list.txt', fn_list, format = 'A'  ;; list of WHAM spectra
  READCOL, dir+'hi_list.txt', hi_fn_list, format = 'A'  ;; ;list of HI spectra
  
  Index = 0 &  Calib = 22.81
  Guess = [0D, -110, 3, 5]
  VMax = -60 &  VMin = -1.0e3
  
  Index = 1 &  Calib = 23.99
  Guess = [0D, -92, 55, .2*22]
  VMax = 1.0e3 &  VMin = -1.0e3
  
  ;; No line here
  ;Index = 2 &  Calib = 22.81  
  ;Guess = [0D, -92, 55, .2*22]
  ;VMax = 1.0e3 &  VMin = -1.0e3
  
  Index = 3 &  Calib = 22.28
  Guess = [0D, -110, 55, .2*22]
  VMax = 1.0e3 &  VMin = -1.0e3
  
  Index = 4 &  Calib = 23.99
  Guess = [0D, -130, 80, .2*22]
  VMax = 1.0e3 &  VMin = -1.0e3
  
  Index = 5 &  Calib = 23.67 ;; same as index=4, but second day
  Guess = [0D, -127, 40, .2*22]
  VMax = -50 &  VMin = -1.0e3
  
  Index = 6 &  Calib = 23.71
  Guess = [0D, -180, 50, .2*22]
  VMax = -70 &  VMin = -1.0e3
  
  Index = 7 &  Calib = 22.28
  Guess = [0D, -180, 50, .2*22]
  VMax = 1.0e3 &  VMin = -1.0e3
  
  Index = 8 &  Calib = 22.81
  Guess = [0D, -180, 50, .2*22]
  VMax = -50 &  VMin = -1.0e3
  
  ; No line here, Off1-Off3, chi^2 = 0.94
  ;Index = 9 &  Calib = 22.81 
  ;Guess = [0D, -180, 50, .2*22]
  ;VMax = -50 &  VMin = -400
  
  Index = 10 &  Calib = 23.96
  Guess = [0D, -195, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 11 &  Calib = 23.96
  Guess = [0D, -197, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 12 &  Calib = 23.96
  Guess = [0D, -197, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 13 &  Calib = 23.96
  Guess = [0D, -197, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 14 &  Calib = 23.99
  Guess = [0D, -197, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 15 &  Calib = 23.99
  Guess = [0D, -230, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 16 &  Calib = 22.28
  Guess = [0D, -250, 50, .2*22]
  VMax = 500 &  VMin = -500
  
  Index = 17 &  Calib = 24.21
  Guess = [0D, -320, 50, .2*22]
  VMax = 500 &  VMin = -400
  
  ; No line, 89.5_-55
  ;Index = 18 &  Calib = 24.21
  ;Guess = [0D, -320, 50, .2*22]
  ;VMax = 500 &  VMin = -400
  
  Index = 19 &  Calib = 24.21
  Guess = [0D, -330, 50, .2*22]
  VMax = 500 &  VMin = -400
  
  Index = 20 &  Calib = 24.21
  Guess = [0D, -330, 50, .2*22]
  VMax = 500 &  VMin = -400
  
  ;; Big error bars, redo
  Index = 21 &  Calib = 23.67
  Guess = [0D, -320, 10, .2*22]
  VMax = -310 &  VMin = -410
  
  ;; Big error bars, redo
  Index = 22 &  Calib = 23.67
  Guess = [0D, -330, 10, .2*22]
  VMax = 0 &  VMin = -400
  
  ;; Gives big error bars, redo
  Index = 23 &  Calib = 23.67
  Guess = [0D, -340, 10, .2*22]
  VMax = 0 &  VMin = -410
  
  ;; No line, redo
  Index = 24 &  Calib = 23.67
  Guess = [0D, -330, 10, .2*22]
  VMax = 0 &  VMin = -410
  
  Index = 25 &  Calib = 24.21
  Guess = [0D, -360, 10, .2*22]
  VMax = -290 &  VMin = -410
  
  ;; No line here
  Index = 2 &  Calib = 22.81  
  VMax = 1.0e3 &  VMin = -1.0e3
  Guess = [0D, -110, 36/2.3548, .2/22]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1
  
  ;; No line, 89.5_-55
  Index = 18 &  Calib = 24.21
  Guess = [0D, -299, 30/2.3548, .03*Calib]
  VMax = 500 &  VMin = -500
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1 &  Parinfo[3].fixed = 1
  
  ;; Big error bars, redo with fixed parms
  Index = 21 &  Calib = 23.67
  Guess = [0D, -322, 23/2.3548, .05*Calib]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1 &  Parinfo[3].fixed = 0
  VMax = -290 &  VMin = -410
  
  ;; Big error bars, redo with fixed parms
  Index = 22 &  Calib = 23.67
  Guess = [0D, -319, 22/2.3548, .05*Calib]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1 &  Parinfo[3].fixed = 1
  VMax = 0 &  VMin = -400
  
  ;; Big error bars, redo with fixed parms
  Index = 23 &  Calib = 23.67
  Guess = [0D, -321, 24/2.3548, .05*Calib]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 0 &  Parinfo[2].fixed = 0 &  Parinfo[3].fixed = 0
  VMax = -290 &  VMin = -410
  
  ;; Big error bars, redo with fixed parms
  Index = 24 &  Calib = 23.67
  Guess = [0D, -340, 30/2.3548, .05*Calib]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1 &  Parinfo[3].fixed = 1
  VMax = 0 &  VMin = -410
  
  
  ;; Multi-wavelength
  
  Index = 30 &  Calib = 23.83
  Guess = [0D, -126, 20/2.3548, .05*Calib]
  PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
  parinfo[1].fixed = 1 &  parinfo[2].fixed = 1 &  parinfo[3].fixed = 0
  VMax = -70 &  VMin = -170
  
  
  DO_Ha = 1
  IF DO_HA THEN BEGIN
     fn = dir+fn_list[Index]
     READFSPE, fn, vel, data, var, ext = 'VELSUB', phead = phead
     Ind = WHERE(vel LE VMax AND vel GE VMin)
     vel = vel[Ind]
     data = data[Ind]
     var = var[Ind]
     PRINT, fn
     MagStream, SXPAR(phead, 'DGAL-LON'), SXPAR(phead, 'DGAL-LAT'), MSl, MSb
     PRINT, 'Coords: (', MSl+360, ',', MSb, ')', FORMAT = '(A,F5.1,A,F5.1,A)'
     WHAM_MPFIT, vel, data, var, Calib, Guess, 'MYFUNC_IP', Parinfo = parinfo
  ENDIF
  
  ;; HI
  
  DO_HI = 0
  IF DO_HI THEN BEGIN
     VMax = 100 &  VMin = -400
     Calib = 1.0/0.1823
     Guess = [0D, -360, 25/2.3548, 1.0*Calib]
     PARINFO = REPLICATE({fixed:0}, N_ELEMENTS(Guess)) 
     Parinfo[1].fixed = 1 &  Parinfo[2].fixed = 1 &  Parinfo[3].fixed = 1
     func_str = 'MYFUNC'
     
     ;; manually looping through
     ;; No ha line in table for 2,9,18,21-24
     ;; No hi line in table for 9 (off1-off2)
     Y = 24
     FOR i = Y, Y DO BEGIN
        fn = dir+hi_fn_list[i]
        PRINT, fn
        READSPE, fn, vel, data, var
        Ind = WHERE(vel LE VMax AND vel GE VMin)
        vel = vel[Ind]
        data = data[Ind]
        var = var[Ind]/2
        
        WHAM_MPFIT, vel, data, var, Calib, Guess, func_str, parinfo = parinfo
     ENDFOR
  ENDIF
  

  
END



PRO WHAM_MPFIT, vel, data, var, Calib, Guess, func_str, parinfo = parinfo
  
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
  Kern = DBLARR(N_ELEMENTS(X))
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(51)-50/2., [Mean_Ip[i], Width_Ip[i]/2.3548, $
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
  Kern = DBLARR(N_ELEMENTS(X))
  FOR i = 0, Num_Ip-1 DO BEGIN
     Kern = Kern + GAUSS1(DINDGEN(51)-50/2., [Mean_Ip[i], Width_Ip[i]/2.3548, $
                              Height_Ip[i]*Width_Ip[i]*SQRT(2*!pi)/2.3548])
  ENDFOR
  
  RETURN,  CONVOL(P[0]+GAUSS1(X, P[1:3])+GAUSS1(X, P[4:6]), Kern, /CENTER, /EDGE_TRUNCATE)
  
END

