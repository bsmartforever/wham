FUNCTION VLSR, ra = ra, dec = dec, lon = lon, lat = lat, utdate = utdate, $
                    utc = utc, jd = jd, verbose = verbose
  
  ;; This function returns the velocity of a given direction in
  ;; the sky at Kitt Peak, corrected for orbital and rotation motion
  ;; of the Earth, plus the LSR correction 
  
  ObsLat = 31.98 
  
  IF (SIZE(Ra, /N_ELEMENTS) NE 0 AND SIZE(Dec, /N_ELEMENTS ) NE 0) THEN BEGIN
     Alpha = ra[0]*15+ra[1]/60.+ra[2]/3600.
     Delta = Dec[0]+Dec[1]/60.+Dec[2]/3600.
  ENDIF ELSE IF (SIZE(lon, /N_ELEMENTS ) NE 0 AND SIZE(lat, /N_ELEMENTS) NE 0) THEN BEGIN
     Euler, lon, lat, Alpha, Delta, 2
  ENDIF
  
  
  ;; UTDate=[Month,Day,Year]
  ;; UTC=[Hour,Minute,Second]
  
  IF NOT(KEYWORD_SET(JD)) THEN $
     JDCnv, UTDate[2], UTDate[0], UTDate[1], UTC[0]+UTC[1]/60.+UTC[2]/3600., JD
  SunPos, JD, Alpha_Sun, Delta_Sun
  
  Euler, Alpha_Sun, Delta_Sun, Lambda_Sun, Beta_Sun, 3 ;; Sun
  Euler, Alpha, Delta, Lambda, Beta, 3   ;; Observation
  
  ;; Get Hour Angle of Observation
  
  EQ2HOR, Alpha, Delta, JD, Alt, Az, Hour_Angle, ObsName = 'kpno'
  
  
  ;; Compute Gamma (location of Equinox?)
  IF SIZE(UTDate, /N_ELEMENTS) NE 0 THEN BEGIN 
     YR = UTDate[2]
     FYR = UTDate[0]/12.
  ENDIF ELSE BEGIN
     CALDAT, JD, Month, Day, YR
     FYR = Month/12.
  ENDELSE
     
  T = (YR+FYR)/100.             ; NUMBER TROPICAL CENTURIES SINCE 1900.0
  Gamma = 281.2208D0+1.7192D0*T+.000453D0*T*T+3.33333D-6*T*T*T
  
  ;; All angles in degrees at this point, convert to radians
  Beta = Beta*!DtoR
  Lambda = Lambda*!DtoR
  
  Lambda_Sun = Lambda_Sun*!DtoR

  Hour_Angle = Hour_Angle * !DtoR
  
  Alpha = Alpha * !DtoR
  Delta = Delta * !DtoR
  
  ObsLat = ObsLat * !DtoR
  
  Gamma = Gamma*!DtoR
  
  ;; Calculate the Velocity
  
  V0=29.974D0                   ;km/s
  ecc = 0.0167D0                ;orbital eccentricity
  
  V_Orb = v0*cos(beta)*(ecc*sin(Gamma-Lambda)-sin(Lambda_Sun-Lambda))
  V_Rot = 0.465*sin(Hour_Angle)*cos(delta)*cos(obslat)
  
  ;; correction to local standard of rest
  ;; Note that the Sa0 and Sd0, and the 19.5 is probably
  ;;  wrong, but close enough!
  
  sa0 = 18.*15./!radeg          ;apex of solar motion to RA=18h
  sd0 = 30./!radeg              ;apex of solar motion to DEC=+30deg
  V_Sun = cos(sa0)*cos(sd0)*cos(alpha)*cos(delta)
  V_Sun = V_Sun+sin(sa0)*cos(sd0)*sin(alpha)*cos(delta)+sin(sd0)*sin(delta)
  V_Sun = -1.0 * 19.5*V_Sun            ; at a vel of 19.5 km/s
  
  IF KEYWORD_SET(Verbose) THEN BEGIN
     PRINT, 'V_LSR = ', V_Orb+V_Rot+V_Sun, FORMAT = '(A,F6.1)'
     PRINT, 'V_Sun = ', V_Sun, FORMAT = '(A,F6.1)'
     PRINT, 'V_Orb = ', V_Orb, FORMAT = '(A,F6.1)'
     PRINT, 'V_Rot = ', V_Rot, FORMAT = '(A,F6.1)'
  ENDIF
  
  RETURN, V_Orb+V_Rot+V_Sun

END
