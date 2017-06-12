;+
;                               iFIND.PRO
;                               Version 1.0;
;Program Description:
; This procedure finds the ion and atomic parameters given a wavelength 
;
;Restrictions:
;      Need to define the path to the atomic file. 
;
;Screen Output:
;       Error text
;
;
;On Input:
;   wave  :== wavelength to search for
;   tol :== wavelength tolerance (optional)
;;On Output:
;   wave_out  :== wavelength of adopted f-value
;   ion_out :==  ion for given wavelength 
;   fval_out :== f-value for given wavelength
;   gam_out :== gamma for given wavelength
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       04/12/13  NL: Version 1.0
;
;External Routines called:
;       None
;----------------------------------------------------------------------------
PRO iFIND,wave,tol,wave_out,ion_out,fval_out,gam_out
;
;Error control.
;
  ON_IOERROR,ESCAPE
;
;Find the fvalue of line within a delta wavelength defined by variable tol.
;
LOOP:
  If NOT keyword_set(tol) THEN tol = 0.001
 
  path = '$HOME/PRO/iNorm/lists/'
  RESTORE,path+'ilines.save'
 
  gg = WHERE(ABS(wavc-wave) LE tol,ct)
  IF ct NE 0 THEN BEGIN
  wave_out = wavc[gg]
  fval_out = fval[gg]
  ion_out = ion[gg]
  gam_out = gam[gg]
  if n_elements(ion_out) gt 1 then begin 
      PRINT,'iFIND::  WARNING: Found more than one transition!'
   for i = 0, n_elements(ion_out) -1 do print, ion_out(i), ' ', double(wave_out(i))
       READ,'iFIND::  Enter EXACT wavelength....   ',wave 
        tol = 1.e-4
        GOTO, LOOP
    endif
; we do not want an array, but a float or string. 
  ENDIF ELSE BEGIN
    PRINT,'iFIND::  Unable to locate line ',STRING(wave,'(f8.3)')
    PRINT,'iFIND::  WARNING: Cannot find wavelength!'
    PRINT,'iFIND::  Enter another (w)avelength or (t)olerance, or (q)uit'
    choice = GET_KBRD(1)
      IF choice EQ 'w' THEN BEGIN
      READ,'iNORM:: Enter wavelength: ',wave 
      GOTO, LOOP
      endif 
      IF choice EQ 't' THEN BEGIN
      READ,'iNORM:: Enter tolerance: ',tol 
      GOTO, LOOP
      endif else begin 
        GOTO, ESCAPE
       endelse 
  ENDELSE  
  wave_out = double(wave_out(0))
  fval_out = double(fval_out(0))
  ion_out = ion_out(0)
  gam_out = double(gam_out(0))
  print, 'iFIND::  Located line ', ion_out[0],' ', STRING(wave_out[0],'(f8.3)')
  RETURN
;------------------------------------------------------------------------------
ESCAPE:
  PRINT,'iFIND:: '+!err_string
  PRINT,'iFIND:: No wavelength defined! Press (Q)uit!'
  RETURN  &  END



