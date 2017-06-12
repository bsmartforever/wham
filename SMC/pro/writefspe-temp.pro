PRO writefspe, filename, ext, v, d, s, $
               extrakeys = extrakeys, extratabkeys = extratabkeys

  ;; For simplicity and lack of easy IDL routines, we only make new
  ;; FITS files with this routine right now. Stub for original
  ;; flexibility is included below.
  
  ;; See if the extension exists already
;  exists = 1
;  errmsg = ''
;  fxbopen, unit, filename, ext, errmsg = errmsg, access = 'rw'
;  IF errmsg NE '' THEN BEGIN 
;    IF errmsg EQ 'Requested extension not found' THEN BEGIN
;      exists = 0
;    ENDIF ELSE BEGIN 
;      message, errmsg
;    ENDELSE 
;  ENDIF 
  
;  ;; if it does, overwrite must be specified to continue
;  IF exists AND NOT keyword_set(overwrite) THEN BEGIN 
;    message, 'Extension "' + ext + '" exists and /OVERWRITE not set.'
;  ENDIF 

;  IF exists THEN BEGIN 
;    message, /info, 'Sorry, no code written yet to overwrite exensions.'
;  endif else begin 

  eksize = size(extrakeys)
  etksize = size(extratabkeys)

  if eksize[0] ne 0 and (eksize[0] ne 2 or eksize[1] ne 3) then $
    message, 'EXTRAKEYS must be a 3 by N array.'
  if etksize[0] ne 0 and (etksize[0] ne 2 or etksize[1] ne 3) then $
    message, 'EXTRATABKEYS must be a 3 by N array.'
    
  fxhmake, header, /init, /extend, /date

  if eksize[0] ne 0 then begin 
      for i = 0, eksize[2]-1 do begin 
          value = 0.0
          catch, err_status
    
          if err_status ne 0 then begin 
              if err_status ne -270 then begin
                  ;; something else is wrong so bail
                  catch, /cancel
                  message, !error_state.msg
              endif else begin 
                  ;; change the data type of 'value'
                  value = ''
              endelse 
          endif 

          ;; We're being very tricky here. IDL will try to read the value
          ;; (passed as a string) as a number the first time through. This
          ;; conversion will allow nicer FITS output by fxaddpar below. If
          ;; the value isn't a number (IDL error number -270), then the
          ;; catch statements above run and change v to string type allowing
          ;; the next line to pass through, essentially just copying the
          ;; passed value into v without modification.
          reads, extrakeys[1, i], value
          
          fxaddpar, header, extrakeys[0, i], value, extrakeys[2, i]
      endfor 
  endif

  fxwrite, filename, header

  ;; Now make the binary table to hold the spectrum
  fxbhmake, xheader, n_elements(v), ext

  fxbaddcol, vcol, xheader, v[0], 'VELOCITY', tunit = 'KM/S'
  fxbaddcol, dcol, xheader, d[0], 'DATA', tunit = 'ADU'
  fxbaddcol, scol, xheader, s[0], 'VARIANCE', tunit = 'ADU^2'

  IF etksize[0] NE 0 THEN BEGIN 
    FOR i = 0, etksize[2]-1 DO BEGIN 
      fxaddpar, xheader, $
                 extratabkeys[0, i], extratabkeys[1, i], extratabkeys[2, i]
    ENDFOR 
  ENDIF 

  fxbcreate, unit, filename, xheader

  fxbwritm, unit, [vcol, dcol, scol], v, d, s

  fxbfinish, unit
  
END 
  
