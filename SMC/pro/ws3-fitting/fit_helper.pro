FUNCTION fit_helper, params, nbk = nbk, mask = mask, $
                     num_ip = num_ip, mean_ip = mean_ip, $
                     width_ip = width_ip, height_ip = height_ip, $
                     setup = setup, takedown = takedown, noip = noip

; params: On SETUP, contains the full list of polynomial and Gaussian
;         parameters.
;         On TAKEDOWN, contains the sigma list returned from CURVEFIT.
;
; obk: Order of the polynomial background; must be specified with SETUP
;
; mask: Contains the mask of variable parameters; must be specified
;         with SETUP
;
; ..._ip: Contains the instrumental profile parameters
;
; setup: Loads the common block with the appropriate values and
;         returns the shortened list of fit parameters to pass to
;         CURVEFIT
;
; takedown: Unloads the sigma vector and returns a full parameter
;         vector back. Zero's in the returned vector are parameters
;         that have not been fit (i.e. were fixed).
;
; noip: Don't load the ip vectors
;
  
  COMMON fitparams, all_params, param_mask, num_bk, n_ip, m_ip, w_ip, h_ip

  IF (keyword_set(setup)) THEN BEGIN 

      IF (NOT keyword_set(nbk)) THEN message, 'Must specify NBK with SETUP.'
      IF (NOT keyword_set(mask)) THEN message, 'Must specify MASK with SETUP.'
      
      all_params = params
      param_mask = mask
      num_bk =  nbk

      new_params = params*0.0
      new_index = 0
      
      FOR i = 0, n_elements(params)-1 DO BEGIN
          IF ((mask AND 2L^i) NE 0) THEN BEGIN
              new_params(new_index) = params(i)
              new_index = new_index + 1
          END 
      ENDFOR 

      IF (NOT keyword_set(noip)) THEN BEGIN
          IF (NOT keyword_set(num_ip)) THEN $
            message, 'Must specify NUM_IP != 0 or NOIP with SETUP.'
          IF (NOT keyword_set(mean_ip)) THEN $
            message, 'Must specify MEAN_IP or NOIP with SETUP.'
          IF (NOT keyword_set(width_ip)) THEN $
            message, 'Must specify WIDTH_IP or NOIP with SETUP.'
          IF (NOT keyword_set(height_ip)) THEN $
            message, 'Must specify HEIGHT_IP or NOIP with SETUP.'
            
          n_ip = num_ip
          m_ip = mean_ip
          w_ip = width_ip
          h_ip = height_ip
      ENDIF 
          
      return, new_params(0:new_index-1)

  ENDIF ELSE IF (keyword_set(takedown)) THEN BEGIN

      new_params = all_params*0.0
      old_index = 0
      
      FOR i = 0, n_elements(new_params) DO BEGIN
          IF ((param_mask AND 2L^i) NE 0) THEN BEGIN
              new_params(i) = params(old_index)
              old_index = old_index + 1
          ENDIF 
      ENDFOR 

      return, new_params

  ENDIF ELSE message, 'Must specify /SETUP or /TAKEDOWN'
  
END 
