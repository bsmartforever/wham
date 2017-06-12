PRO ext, par, xpar, red = red, r = r, v = v
  
; Get parameters for the functions.
; From Cardelli, Clayton, & Mathis (1979)
; Optical/NIR values (as opposed to IR), Ha = 0.6563um
;
; par is either N_H or E(B-V) if /red is specified
  
IF NOT keyword_set(r) THEN r = 3.1 
IF NOT keyword_set(red) THEN red = par/(4.93*10.0^21) ELSE red = par

IF n_params() LT 1 THEN BEGIN
  print, 'Usage: ext, NH, [inverse wavelength]'
  print, 'Band: U    X parameter (um^-1): 2.78  '
  print, 'Band: B    X parameter (um^-1): 2.27  '
  print, 'Band: V    X parameter (um^-1): 1.82  '
  print, 'Band: R    X parameter (um^-1): 1.43  '
  print, 'Band: I    X parameter (um^-1): 1.11  '
  print, 'Band: J    X parameter (um^-1): 0.80  '
  print, 'Band: H    X parameter (um^-1): 0.63  '
  print, 'Band: K    X parameter (um^-1): 0.46  '
  print, 'Band: L    X parameter (um^-1): 0.29  '
  RETURN
ENDIF
  
IF NOT keyword_set(xpar) THEN BEGIN
   xpar = 1.0/0.6563 ;; wavelength of H alpha is default
   filter = 'H alpha'
ENDIF ELSE $
 IF xpar EQ 2.78 THEN filter = 'U' ELSE $
 IF xpar EQ 2.27 THEN filter = 'B' ELSE $
 IF xpar EQ 1.82 THEN filter = 'V' ELSE $
 IF xpar EQ 1.43 THEN filter = 'R' ELSE $
 IF xpar EQ 1.11 THEN filter = 'I' ELSE $
 IF xpar EQ 0.80 THEN filter = 'J' ELSE $
 IF xpar EQ 0.63 THEN filter = 'H' ELSE $
 IF xpar EQ 0.46 THEN filter = 'K' ELSE $
 IF xpar EQ 0.29 THEN filter = 'L' ELSE $
  filter = 'No specific'

ypar = xpar - 1.82
a = 1+0.17699*ypar-0.50447*ypar^2-0.02427*ypar^3+0.72085*ypar^4 $
     +0.01979*ypar^5-0.77530*ypar^6+0.32999*ypar^7
b = 1.41338*ypar+2.28305*ypar^2+1.07233*ypar^3-5.38434*ypar^4 $
     -0.62251*ypar^5+5.30260*ypar^6-2.09002*ypar^7

model = 1.08*(r*a+b)
Alambda = red*(r*a+b)
opacity = Alambda*1.08
ratio = a+b/r

IF NOT keyword_set(v) THEN BEGIN ;verbose mode
 print, 'Filter: '+filter+' band'
 print, 'Wavelength: '+strtrim(1/xpar, 2)+'um'
 print, '-------------------------------'
 print, 'Inverse wavelength: '+strtrim(xpar, 2) 
 print, 'a(x) parameter: '+strtrim(a, 2)
 print, 'b(x) parameter: '+strtrim(b, 2)
 print, 'Model = '+strtrim(model, 2)
 print, '-------------------------------'
 print, 'A_Lambda = '+strtrim(alambda, 2)
 print, 'A_lambda/A_V ='+strtrim(ratio, 2)
 print, 'E(B-V) = '+strtrim(red, 2)
 print, 'Tau = Model*E(B-V) = '+strtrim(opacity, 2)
 print, '-------------------------------'
ENDIF

END

