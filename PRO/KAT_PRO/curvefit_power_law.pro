PRO curvefit_power_law, x, coefs, y, pder
 
; Computing function to fit
y = ALOG(coefs[0]) + coefs[1]*x
 
; Computing derivatives of function to fit with respect
; to its coefficients.
IF (N_PARAMS() GE 4) THEN $
    pder = [[1./coefs[0] + x*0.],[x]]
END
 