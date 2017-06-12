@kplot

PRO width_decomp, W_obs = W_Obs, S_obs = S_obs, xrange = xrange, yrange = yrange

  ;; Analyze the widths of Ha,NII,SII lines toward l=180
  ;; to determine T_e, v_turb
  
  ;; Formulae from Reynolds 85 ApJ294,255 and my notes
  ;; Indexing: H, N, S
  
  ON_ERROR, 2
  ;; Help/error
  IF NOT(KEYWORD_SET(W_obs)) OR N_ELEMENTS(W_Obs) NE 3 THEN BEGIN
     
     PRINT, ' Decomposition of line widths into T, and v_turb'
     PRINT, 'Usage:'
     PRINT, '  Width_Decomp, W_Obs=W_Obs, S_Obs=S_Obs, xrange=xrange, yrange=yrange'
     PRINT, '     W_Obs is 3-element vector of observed line widths (FWHM), with '
     PRINT, '       elements 0,1,2 for H,NII,and SII, respectively'
     PRINT, '     S_Obs is optional 3-element vector of corresponding uncertainties in W_Obs'
     PRINT, '     Xrange is optional x-range for the plot'
     PRINT, '     YRange is optional y-range for the plot'
     GOTO, End_Label
     
  ENDIF
  ;; Coefficients from formulas
  A = [21.4, 5.72, 3.78]
  B = [12.8, 3.42, 2.26]
  
  ;; V_obs is observed line width of [H,N,S]
  ;; i.e. V_obs = [32.4, 35.1, 29.2]
  ;; MUST BE 3-element vector!!
  
  ;; S_obs is sigma/uncertainty in W_obs (also [H,N,S])
  ;; i.e. S_th = [2.0, 2.0, 2.0]
  
  V_non_therm = FINDGEN(10000)*0.005
  
  IF NOT(KEYWORD_SET(xrange)) THEN xrange = [0, 30]
  IF NOT(KEYWORD_SET(yrange)) THEN yrange = [0, 5.0e4]
  IF NOT(KEYWORD_SET(S_Obs)) THEN S_Obs = [0, 0, 0]
  
  FOR i = 0, 2 DO BEGIN

    T_low = 1.0e4 * ( ((W_obs[i]-S_Obs[i])/(A[i]))^2.0 - (V_non_therm/B[i])^2 )
    T_hi = 1.0e4 * ( ((W_obs[i]+S_Obs[i])/(A[i]))^2.0 - (V_non_therm/B[i])^2 )
    IF i EQ 0 THEN BEGIN
       KPLOT, V_non_therm, T_low, $
          yrange = yrange, xrange = xrange, $
          YTITLE = "T!De!N [K]", $
          XTITLE = "V!DNon-Thermal!N [km s!U-1!N]", $
          TITLE = "Temp. and Turb. Velocity Decomposition", $
          LINESTYLE = i+1
       KOPLOT, V_non_therm, T_hi, LINESTYLE = i+1
    ENDIF ELSE BEGIN
       KOPLOT, V_non_therm, T_low, LINESTYLE = i+1
       KOPLOT, V_non_therm, T_hi, LINESTYLE = i+1
    ENDELSE
  ENDFOR

  KLEGEND, [0.65, 0.9], [[-1,0], [-2, 0], [-3, 0]], $
     ["H!7a!3, !4C!3="+STRING(W_Obs[0], STRING(177B), S_Obs[0], FORMAT = '(F4.1,A,F3.1)'), $
      "[NII], !4C!3="+STRING(W_Obs[1], STRING(177B), S_Obs[1], FORMAT = '(F4.1,A,F3.1)'), $
      "[SII], !4C!3="+STRING(W_Obs[2], STRING(177B), S_Obs[2], FORMAT = '(F4.1,A,F3.1)')], $
     CHARSIZE = 1.5

  End_Label:
  
END
 
