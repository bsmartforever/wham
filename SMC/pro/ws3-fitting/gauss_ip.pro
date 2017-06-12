PRO     gauss_ip, X, A, F, PDER
;+
; NAME:
;       gauss_ip  (based on Users' Library routine FUNCT)
;
; PURPOSE:
;       Evaluate the sum of (state.ng) Gaussians and up to a 3rd-order
;       polynomial, convolved with an instrumental profile, and
;       optionally return the value of the partial derivatives.
;       Normally, this function is used by FITTING to fit the sum of
;       lines and a varying background to actual data.
;
; CATEGORY:
;       E2 - Curve and surface fitting.
;
; CALLING SEQUENCE:
;       gauss_ip, X, A, F [, PDER]
;
; INPUTS:
;       X:      The values of the independent variable.
;       A:      The fit parameters
;                 A(0)        : Polynomial order
;                 A(1) - A(4) : Polynomial coefficients
;                 A(5) - .... : Gaussian parameters (3*N_GAUSS)
;
; OUTPUTS:
;       F:      The value of the function at each X(i).
;
; OPTIONAL OUTPUT PARAMETERS:
;       PDER:   An array of the size (N_ELEMENTS(X), N_ELEMENTS(A)) 
;               that contains the partial derivatives, with respect to the
;               variable parameters (only).  Pder(i,j) represents the 
;               derivative at the i'th point with respect to j'th parameter.
;
; COMMON BLOCKS:
;       FITPARAMS: Set up by FIT_HELPER.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       Must call FIT_HELPER before this function to set up COMMON block.
;
; PROCEDURE:
;       F = A(5)*EXP(-Z^2/2) + BKG 
;       Z = (X-A(6))/A(7)
;       BKG = A(1) + A(2)*X + A(3)*X^2 + A(4)*X^3  
;
;       Function is summed over the number of gaussians; first gaussian
;       is in A(5),A(6),A(7), second is in 8,9,10, ...

; MODIFICATION HISTORY:
;	WRITTEN, DMS, RSI, SEPT, 1982.
;	Modified, DMS, Oct 1990.  Avoids divide by 0 if A(2) is 0.
;       July 1994 Kristin Tufte - modified to handle multiple gaussians
;       June 1996 Matt Haffner - extracted out of WHAMFIT for general use
;-
;  ON_ERROR, 2                   ;Return to caller if an error occurs

  COMMON fitparams, all_params, param_mask, num_bk, n_ip, m_ip, w_ip, h_ip

  n_bk = num_bk
  n_gauss = (n_elements(all_params) - n_bk)/3
;  print, a
                                ; These constants are needed since
                                ; we're using FWHM instead of sigma
  
  d1 = 2.0 * sqrt(alog(double(2.0))) / sqrt(!dpi)
                                ; gaussian = d1 / FWHM * exp(-z)
  d2 = 4.0 * alog(double(2.0))  ; z = d2 * (x - mean)^2 / FWHM^2

  b  = dblarr(n_bk)             ; to make the code easier to write
  IF (n_gauss GT 0) THEN BEGIN  ; and read
      m  = dblarr(n_gauss)      
      w  = dblarr(n_gauss)      
      ar = dblarr(n_gauss)
  ENDIF

  a_index = 0

  ; Setup polynomial
  FOR i = 0, n_bk-1 DO BEGIN
      IF ((param_mask AND 2L^i) NE 0) THEN BEGIN
          ;; This param is variable and is taken from the A vector
          b(i) = a(a_index)
          a_index = a_index + 1
      ENDIF ELSE $
        ;; This one is fixed
        b(i) = all_params(i)    
  ENDFOR

  ; Setup Gaussians
  IF (n_gauss GT 0) THEN BEGIN 
      FOR i = 0, n_gauss-1 DO BEGIN
          g_index = 3*i + n_bk

          IF ((param_mask AND 2L^g_index) NE 0) THEN BEGIN
              m(i) = a(a_index)
              a_index = a_index + 1
          ENDIF ELSE m(i) = all_params(g_index)

          IF ((param_mask AND 2L^(g_index+1)) NE 0) THEN BEGIN
              w(i) = a(a_index)
              a_index = a_index + 1
          ENDIF ELSE w(i) = all_params(g_index + 1)

          IF ((param_mask AND 2L^(g_index+2)) NE 0) THEN BEGIN
              ar(i) = a(a_index)
              a_index = a_index + 1
          ENDIF ELSE ar(i) = all_params(g_index + 2)

      ENDFOR
  ENDIF
  
  f = double(0.0)
  
  FOR i = 0, n_bk-1 DO BEGIN
      f = f + b(i)*(x^double(i))
  ENDFOR

                                ; ignore small terms by using LE 88.	
  IF (n_gauss GT 0) THEN BEGIN 
      FOR i = 0, (n_gauss-1) DO BEGIN
          FOR j = 0, (n_ip-1) DO BEGIN
              cw = sqrt(w(i)^2.0 + w_ip(j)^2.0) ; convolved width
              ch = ar(i) * h_ip(j) * w_ip(j)/cw 
                                ; convolved height... This weird
                                ; expression =
                                ; ar(i) * ar_ip(j) * d1 / cw
              arg = d2*((x-m(i)-m_ip(j))/cw)^2.0
              f = f + (ch * exp(-arg)*(abs(arg) LE 88.)) 
          ENDFOR 
      ENDFOR
  ENDIF

  IF n_params() LE 3 THEN RETURN ; don't need partials?

  ;;
  ;; Calculate partials
  ;;

  pder = replicate(double(0.), n_elements(x), n_elements(a)) ; init pder
  a_index = 0
  
  ;; Backgrounds

  FOR i = 0, n_bk-1 DO BEGIN
      IF ((param_mask AND 2L^i) NE 0) THEN BEGIN
          ;; This parameter was variable -- calculate the partial
          pder(0, a_index) = x^double(i)
          a_index = a_index + 1
      ENDIF 
  ENDFOR

  ;; Gaussians

  IF (n_gauss GT 0) THEN BEGIN 
      a_save = a_index          ; save the current param index
      FOR j = 0, (n_ip-1) DO BEGIN
          a_index = a_save      ; restore the param index to the
                                ; 1st gaussian parameter used 
          FOR i = 0, (n_gauss-1) DO BEGIN
              g_index = 3*i + n_bk

              cw = sqrt(w(i)^2.0 + w_ip(j)^2.0) ; convolved width
              ch = ar(i) * h_ip(j) * w_ip(j)/cw 
                                ; convolved height... This weird
                                ; expression =
                                ; ar(i) * ar_ip(j) * d1 / cw
              z = (x-m(i)-m_ip(j))/cw ; Time savers
              ex = exp(-d2*z^2.0)*(abs(z) LE 88.0)
              
              ;; Mean
              IF ((param_mask AND 2L^g_index) NE 0) THEN BEGIN
                  pder(*, a_index) = pder(*, a_index) + $
                    2.0*d2*ch/cw*ex*z
                  a_index =  a_index + 1
              ENDIF 
              
              ;; Width
              IF ((param_mask AND 2L^(g_index+1)) NE 0) THEN BEGIN
                  pder(*, a_index) = pder(*, a_index) + $
                    (2.0*d2*w(i)*ch/cw^2.0*ex*z^2.0) $
                    - (w(i)*ch/cw^2.0*ex) 
                  a_index = a_index + 1
              ENDIF
              
              ;; Area
              IF ((param_mask AND 2L^(g_index+2)) NE 0) THEN BEGIN
                  pder(*, a_index) = pder(*, a_index) + ch/ar(i)*ex
                  a_index = a_index + 1
              ENDIF

          ENDFOR 
      ENDFOR
  ENDIF

END
