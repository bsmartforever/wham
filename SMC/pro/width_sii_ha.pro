PRO WIDTH_SII_HA, wha, WSII, V_NON_THERM, TEMP, wha_sd, wsii_sd, v_nt_sd, temp_sd

;    V_NON_THERM = [ [ (wha/21.4D)^2.0D - (WSII/3.78D)^2.0D - 0.070D ] / [ (1/12.8D)^2.0D - (1/2.27D)^2.0D ] ]^(.5D)

;    TEMP = 1.0E4 * [ (wha/21.4D )^2.0D - (V_NON_THERM/12.8D)^2.0D - 0.070D  ]

  ;; constants version & t/v expressions only depend on widths
  
  a = 21.4D
  b = 12.8D
  c = 3.78D
  d = 2.27D
  f = 0.07D
  
  v_non_therm = b * sqrt((d^2 * (a^2 * wsii^2 - c^2 * wha^2 + a^2 * c^2 * f)) / (a^2 * c^2 * (b^2 - d^2)))

  temp = 1e4 * (b^2 * c^2 * wha^2 - a^2 * d^2 * wsii^2 - a^2 * b^2 * c^2 * f) / (a^2 * c^2 * (b^2 - d^2))
  
  ;; propagation of errors, if width errors supplied
  if isa(wha_sd) and isa(wsii_sd) then begin
    
    v_nt_sd = sqrt(((b^2 * d^2 * wha) / (a^2 * (b^2 - d^2) * v_non_therm))^2 * wha_sd^2 + $
                   ((b^2 * d^2 * wsii) / (c^2 * (b^2 - d^2) * v_non_therm))^2 * wsii_sd^2)
  
    temp_sd = sqrt(((2e4 * b^2 * wha) / (a^2 * (b^2 - d^2)))^2 * wha_sd^2 + $
                   ((2e4 * d^2 * wsii) / (c^2 * (b^2 - d^2)))^2 * wsii_sd^2)

  endif

END
 
