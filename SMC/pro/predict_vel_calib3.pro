@kplot

FUNCTION N_NON_LIN, P

  ;; Returns the optical index of SF6 given a pressure
  ;; P in cm Hg
  ;; is non-linear, uses Van der Waals formalism

  R = 8.314472D
  T = 25+272D
  a = .7857D
  b = 8.786D-5

  n = DINDGEN(14000)*.01  ;; provides 0.01 cm Hg prec. in pressure
  
  p_van = R*T/(1/n-b) - n^2*a
  p2 = R*T*n

  hg = p_van*76.0/101325D
  hg2 = P2*76.0/101325D
  
  m = 1.73606d-5*n ;; optical index -1

  ;; 'Solve' for n by finding the closest point to P

  m1 = (m[WHERE(ABS(hg-P) EQ MIN(ABS(hg-P)))])[0]  ;; non-linear gas P
  m2 = (m[WHERE(ABS(hg2-P) EQ MIN(ABS(hg2-P)))])[0] ;; linear gas P

  RETURN, m1+1

END

FUNCTION N_WAVE, l
  
  ;; Returns the optical index at wavelength l [Ang],
  ;; at 1 atm (76.0 cm HG) and 25 C

  A = 1.74065d28
  B = 2.5118d31
  nu = 299792.D / (l*1.0d-13)

  RETURN, 1+A/(B-nu^2)

END

FUNCTION OPT_INDEX, l, P, Space = Space

  ;; Returns optical index as a fun. of l,P
  ;; Applies translation shift 
  ;; This is Q=1/(2*n*l)
  ;; with l=0.0471 cm (spacing of chamber A)
  ;; the other l is lambda...

  ;;  Spacings from Tufte PhD
  ;;  Space = 0.0471  ;; Spacing in Chamber A [cm]
  ;;  Space = 0.0201  ;; Spacing in Chamber B [cm]

  ;; Spacings from 'best fit'
  ;; Space = 0.04707
  ;; Space = 0.02013

  Result = 1.0/(2*Space*DOUBLE(N_NON_LIN(P) + N_WAVE(l)-N_NON_LIN(76.0))) 
  RETURN, Result[0]

END

FUNCTION PREDICT_TUNE, L_ref, P_ref, L_targ, Space = Space, $
                       air = air, verbose = verbose
  
  ;; L_ref and L_targ need to be vaccum wavelenghts!!
  ;; Do conversion here
  ;; This is a lambda^2 fit to index of refraction of air 
  ;;  valid for optical wavelengths from page 10-304 in CRC
  ;; Real formula is complicated!
  
  ;; Fit is from Vac_Air below
  Fit = DBLARR(3)
  fit[0] = .130366
  fit[1] = .000244518
  fit[2] = 1.73603e-9
  
  IF KEYWORD_SET(air) THEN BEGIN
     Delta_Lambda = Fit[0] + Fit[1]*L_ref+Fit[2]*L_Ref^2
     L_Ref =  L_Ref + Delta_Lambda
     Delta_Lambda = Fit[0] + Fit[1]*L_Targ+Fit[2]*L_Targ^2
     L_Targ =  L_Targ + Delta_Lambda
     IF KEYWORD_SET(Verbose) THEN PRINT, 'Using ', L_Ref, L_Targ, ' as lambdas'
  ENDIF
  
  ;; Convert wavelength to inverse cm
  
  Sig_Ref = 1/(L_ref*1.0e-8)
  Sig_Targ = 1/(L_targ*1.0e-8)
  
  ;; Determine which direction to go
  
  Sign = (Sig_Ref GT Sig_Targ) ? 1.0 : -1.0
  
  Sig_Current = Sig_Ref
  L_Prev = L_Ref
  i = 0
  WHILE (Sig_Ref GT Sig_Targ ? Sig_Current GT Sig_Targ : Sig_Current LT Sig_Targ) DO BEGIN
     L = 1/( (Sig_Current-Sign*Opt_Index(L_Prev, P_ref, Space = Space))*1.0e-8 )
     Q = Opt_Index(L, P_ref, Space = Space)
     IF KEYWORD_SET(Verbose) THEN Print, L, Q
     Sig_Current = Sig_Current - Sign*Q
     L_Prev = L 
     i = i+1
  ENDWHILE
  IF KEYWORD_SET(Verbose) THEN BEGIN
     PRINT, 'Done!'
     PRINT, 'Line is ', i-1+(Sign*Sig_Current-Sign*Sig_Targ+Q)/Q, ' orders away', $
        FORMAT = '(A,F7.2,A)'
  ENDIF
  P_New = P_ref+Sign*((Sign*Sig_Current-Sign*Sig_Targ+Q)*3.0e5/Sig_Targ)/3.0
  
  FSR = (Opt_Index(L_targ, P_New, Space = Space)*3.0e5/Sig_Targ)/3.0
  IF KEYWORD_SET(Verbose) THEN PRINT, P_New, FSR, P_New-Sign*FSR
  
  P1 = P_New
  P2 = P_New - Sign*FSR
  
  ;; Determine which pressure to return
  ;; the one closest to 100
  ;RETURN, ABS(P1-100) LE ABS(P2-100) ? P1 : P2
  
  ;; Changed my mind, return the one greater than atmospheric pressure
  ;; of 70 
  RETURN, (P1 LE 70) ? P2 : P1
END

PRO FIND_FSR

  Space = FINDGEN(100)*0.000001 + 0.0202-50*0.000001

  L_ref = [4861.3, 5006.9, 5754.6, 5875.6, 6562.8, 6583.4, 6716.4,7319.10,6300.30]  ;; Air
  L_ref = [4862.6, 5008.3, 5756.2, 5877.2, 6564.6, 6585.2, 6718.2,7319.10,6300.30]  ;; Vacuum

  P_ref = [146.0, 109.1, 126.6, 105.4, 94.7, 131.5, 86.4,130.7,108.5]  ;; A
  P_ref = [150.0, 160.6, 80.6, 136.4, 107.2, 97.5, 123.4,201.8,179.4]  ;; B

  IF 0 THEN BEGIN

    I_Ref = 5
    I_Targ = 6
    Tune = FLTARR(N_ELEMENTS(Space))

    FOR i = 0, N_ELEMENTS(Space)-1 DO BEGIN
      IF i MOD 10 EQ 0 THEN PRINT, i, SYSTIME()
      Temp = Predict_Tune(L_Ref[I_Ref], P_Ref[I_Ref], L_Ref[I_Targ], Space = Space[i])
      Tune[i] = Temp[WHERE(MIN(ABS(Temp-P_Ref[I_Targ])) EQ ABS(Temp-P_Ref[I_Targ]))]
    ENDFOR

    PRINT, Tune
    KPLOT, Space, Tune, xtitle = 'Spacing in Chamber [cm]', $
      ytitle = 'Predicted Tune [cm Hg]'
    KOPLOT, [Space[0], Space[N_ELEMENTS(Space)-1]], [P_Ref[I_Targ]-0.5, P_Ref[I_Targ]-0.5], $
      LINESTYLE = 2
    KOPLOT, [Space[0], Space[N_ELEMENTS(Space)-1]], [P_Ref[I_Targ], P_Ref[I_Targ]], $
      LINESTYLE = 3
    KOPLOT, [Space[0], Space[N_ELEMENTS(Space)-1]], [P_Ref[I_Targ]+0.5, P_Ref[I_Targ]+0.5], $
      LINESTYLE = 4
    
    Best_Space = Space[WHERE(ABS(Tune-P_Ref[I_Targ]) EQ MIN(ABS(Tune-P_Ref[I_Targ])))]
    KLEGEND, [0.5, 0.1], [3, 0], ['Best Space = ' + STRTRIM(Best_Space, 2)], $
      CHARSIZE = 1.5
  ENDIF
  
  IF 1 THEN BEGIN
    Meta_I_Ref = [0, 2, 4, 4, 5]
    Meta_I_Targ = [1, 3, 5, 6, 6]
    Diff = FLTARR(5)
    
    Space = FINDGEN(24)*0.00001+0.02011
    Stats = FLTARR(N_ELEMENTS(Space), 2)
    FOR i = 0, N_ELEMENTS(Space)-1 DO BEGIN
      FOR j = 0, 4 DO BEGIN
        Temp = Predict_Tune(L_Ref[Meta_I_Ref[j]], P_Ref[Meta_I_Ref[j]], L_Ref[Meta_I_Targ[j]], $
                            Space = Space[i])
        Tune = Temp[WHERE(MIN(ABS(Temp-P_Ref[Meta_I_Targ[j]])) EQ ABS(Temp-P_Ref[Meta_I_Targ[j]]))]
        Diff[j] = P_Ref[Meta_I_Targ[j]]-Tune
      ENDFOR
      PRINT, Space[i], Diff
      Mom = MOMENT(Diff)
      Stats[i, 0] = Mom[0] &  Stats[i, 1] = Mom[1]
    ENDFOR
  ENDIF


END

PRO VAC_AIR
  
  
  v = 10*(FINDGEN(80)*10+200)
  
  d=10*[.0648,.0666,.0687,.0708,.0730,.0753,.0777,.0801, $
     .0825, .0850, .0874, .0899, .0925, .0950, $
     .0975, .1001, .1027, .1053, .1079, .1105, .1131, $
     .1157, .1183, .1209, .1236, .1262, .1288, .1315, $
     .1341, .1368, .1394, .1421, .1448, .1474, .1501, $
     .1528, .1554, .1581, .1608, .1635, .1661, .1688, $
     .1715, .1742, .1769, .1796, .1822, .1849, .1876, $
     .1903, .1930, .1957, .1984, .2011, .2038, .2065, $
     .2092, .2119, .2146, .2173, .2200, .2227, .2254, $
     .2281, .2308, .2335, .2362, .2389, .24216, .2443,$
     .2470, .2497, .2524, .2551, .2578, .2605, .2632, $
     .2660, .2687, .2714]
  
  PLOT, V-d, d, psym = 4
  
  Fit = POLY_FIT(v-d, d, 2)
  PRINT, Fit
  
  ;; Fit is
  ;; fit[0]=.130366
  ;; fit[1]=.000244518
  ;; fit[2]=1.73603e-9
  
  OPLOT, v, fit[0]+fit[1]*v+fit[2]*v^2
  PRINT, d - (fit[0]+fit[1]*v+fit[2]*v^2)
  
END

FUNCTION predict_vel_calib3, P, line_in, silent=silent, obs=obs, vcoeffs=vcoeffs

;+
;NAME: predict_vel_calib3
;PURPOSE: Predict the location of rest wavelength of the line within the
;    standard velocity vector ('geocentric zero')
;SYNTAX: offset = predict_vel_calib3(P, line [, /silent])
;INPUTS:
;   P - array of pressures (in cm): [P_a, P_b]
;   line - string specifying the line: hb, oiii, blue_nii, hei, ha, nii, OR sii
;   obs - 'ctio' or 'kpno'. Default: 'ctio'
;
;MODIFICATION HISTORY
;   2009-11-18 ASH: moved from scale_velocity.pro to this standalone program
;       differences from tune.pro: line is given as a string rather than a
;           keyword; obs=obs keyword allows choice between KPNO and CTIO tunes
;   scale_velocity.pro: 2007-10-16 ASH: slightly modified version of
;       predict_vel_calib, in tune.pro
;-
  
  ;; Predict the location of rest wavelength of the line within the
  ;; standard velocity vector ('geocentric zero')
  
  ;; Determine wavelength and nominal tunes from keywords
  
  version='3.2'
  if ~keyword_set(silent) then print, 'Version ' + version
  
  line = line_in
  obs = keyword_set(obs) ? obs : 'ctio'
IF obs EQ 'kpno' THEN BEGIN
	CASE line OF
		'hb': BEGIN
			lambda=4861.3 & P_0=[146.0, 150.0] & line='Hb'
		END
		'oiii': BEGIN
			lambda=5006.9 & P_0=[109.1, 160.6] & line='[O III]'
		END
		'blue_nii': BEGIN
			lambda=5754.6 & P_0=[126.6, 80.6] & line='blue [N II]'
		END
		'hei': BEGIN
			lambda=5875.6 & P_0=[105.4, 136.4] & line='He I'
		END
		'ha': BEGIN
			lambda=6562.8 & P_0=[94.7, 107.2] & line='Ha'
		END
		'nii': BEGIN
			lambda=6583.4 & P_0=[131.5, 97.5] & line='[N II]'
		END
		'sii': BEGIN
			lambda=6716.4 & P_0=[86.4, 123.4] & line='[S II]'
		END
		'oii': BEGIN
			lambda=7319.1 & P_0=[130.7, 201.8] & line='[O II]'
		END
		'oi': BEGIN
			lambda=6300.3 & P_0=[108.5, 179.5] & line='[O I]'
		END
	ELSE: message, 'Unknown line ', line
	ENDCASE
	spaceA = 0.04707
	spaceB = 0.02013
  ;; Nominal P_0 Tunes above are measured, and place the line at +50
  ;; on the standard scale
	mean_V = 50.0
ENDIF ELSE IF obs EQ 'ctio' THEN BEGIN
    readcol, '/d/wham/pro/calibration/tunes.txt', filter, l_arr, pa, pb, $
        format='A,F,F,F', silent = keyword_set(silent)
    idx = where(line EQ filter)
    IF array_equal(idx, -1) THEN message, 'Unknown line ' + line
    lambda = l_arr[idx]
    P_0 = [pa[idx], pb[idx]]

	spaceA = 0.047077
	spaceB = 0.020144
    v0=150.0 & v1=-2.018 & v2=-0.00034
	IF keyword_set(vcoeffs) THEN IF vcoeffs[0] NE 0 THEN BEGIN
	    v0=vcoeffs[0] & v1=vcoeffs[1] & v2=vcoeffs[2]
    ENDIF

    ; nominal tunes should place the geocentric zero at the 50th ring
	r=50
	mean_V = v0 + v1*r + v2*r^2
ENDIF ELSE message, 'Fatal Error: unknown observatory ' + obs
	
	IF NOT keyword_set(silent) THEN print, 'Using ' + line
  
  ;; velocity to pressure scale factor of 2.8 
  ;; (NOTE: 3.0 used through version 3.1 ~ 1/10/13)
  VtoP = 2.8
  
  ;; Now calculate the nominal vel of the line at rest
  ;;  Use chamber A as a reference and the scale factor
  
  ;; Now we have to make sure that the delta P's are the same, no
  ;;  order hopping
  ;;  This may not be true for high velocity stuff!
  Delta_P_0 = P_0[0]-P_0[1]
  Delta_P = P[0]-P[1]
  
  IF ABS(Delta_P_0 - Delta_P) LE 5 THEN BEGIN ;; Delta Ps are about the same, no order swapping
     Zero = Mean_V + (P_0[0]-P[0]) * VtoP
     
  ENDIF ELSE BEGIN
     ;; One of the chambers is off by some number of FSRs
     ;; Get the FSRs of A and B
     FSR_A = (Opt_Index(lambda, P_0[0], Space = spaceA) * 3.0e5 / (1/(lambda*1.0e-8)) )/VtoP
     FSR_B = (Opt_Index(lambda, P_0[1], Space = spaceB) * 3.0e5 / (1/(lambda*1.0e-8)) )/VtoP
     ;; Loop through +/- 3 orders in both A and B and find the right combo
     Delta_P = FLTARR(7, 7)
     FOR i = 0, 6 DO BEGIN
        FOR j = 0, 6 DO BEGIN
           Delta_P[i, j] = ABS( (Delta_P_0) - (P[0]+((i-3)*FSR_A) - P[1]-((j-3)*FSR_B)) ) 
        ENDFOR
     ENDFOR
     Ind = WHERE(Delta_P LE 10, Count)
     IF Count EQ 1 THEN BEGIN
        N_Ord_A = 3-(Ind MOD 7)
        N_Ord_B = 3-(Ind-(Ind MOD 7))/7
        IF NOT(KEYWORD_SET(Silent)) THEN BEGIN
           PRINT, 'Some order(s) switched!'
           PRINT, STRING('Reference P: ', P_0[0], ' , ', P_0[1], FORMAT = '(A,F5.1,A,F5.1)')
           PRINT, STRING('Given  P   : ', P[0], ' , ', P[1], FORMAT = '(A,F5.1,A,F5.1)')
        ENDIF
        P[0] = P[0] - N_Ord_A * FSR_A
        P[1] = P[1] - N_Ord_B * FSR_B
        Zero = Mean_V + (P_0[0]-P[0]) * VtoP
        IF NOT(KEYWORD_SET(Silent)) THEN BEGIN
           Sign_A = (N_Ord_A LT 0) ? '-' : '+'
           PRINT, 'Chamber A is ', Sign_A, ABS(N_Ord_A), ' order(s) away', $
              FORMAT = '(A,A,I2,A)'
           Sign_B = (N_Ord_B LT 0) ? '-' : '+'
           PRINT, 'Chamber B is ', Sign_B, ABS(N_Ord_B), ' order(s) away', $
              FORMAT = '(A,A,I2,A)'
           PRINT, STRING('Adjusted  P: ', P[0], ' , ', P[1], FORMAT = '(A,F5.1,A,F5.1)')
        ENDIF
     ENDIF ELSE BEGIN
		IF NOT keyword_set(silent) THEN $
		    message, 'Cannot match orders! min(delta_P) = ' + strtrim(min(delta_p),2)
		Zero = -1000
     ENDELSE
     
  ENDELSE
  
  RETURN, Zero
  
END
