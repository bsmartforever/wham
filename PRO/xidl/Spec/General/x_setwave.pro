;+ 
; NAME:
;  x_setwave   
;   Version 1.1
;
; PURPOSE:
;    Sets a wavelength array given a header from a FITS file
;
; CALLING SEQUENCE:
;   
;   wave = x_setwave(head, ntot)
;
; INPUTS:
;   head     - Header
;   ntot     - number of pixels
;
; RETURNS:
;   wave     - Wavelength array
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   wave = x_setwave(head, 1000L)
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   17-Nov-2001 Written by JXP 
;-
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

function x_setwave, head, ntot

;
  if  N_params() LT 2  then begin 
    print,'Syntax - ' + $
             'wave = x_setwave(head, ntot) [v1.1]'
    return, -1
  endif 

;  Optional Keywords

;  Parse the header

  crpix1 = sxpar(head, 'CRPIX1', count=count)
  if count EQ 0 then crpix1 = 1.0d
  crval1 = sxpar(head, 'CRVAL1', count=count)
  if count EQ 0 then crval1 = 1.0d
  cdelt1 = sxpar(head, 'CDELT1', count=count)
  if count EQ 0 then cdelt1 = 1.0d
;
  cd1_1 = sxpar(head, 'CDELT1', count=count)
  if count EQ 0 or abs(cd1_1-1.) LT 1e-5 then begin
      cdelt1 = sxpar(head, 'CD1_1', count=count)
      if count EQ 0 then cdelt1 = 1.0d
  endif

; ctype
  ctype1 = sxpar(head, 'CTYPE1', count=count)

  case strmid(strtrim(ctype1,2),0,4) of 
      'POLY': begin
          print, 'Not able to handle poly_lambda'
          return, -1
      end
      'LINE': begin
          dcflag = sxpar(head, 'DC-FLAG', count=count)
          if dcflag EQ 1 then stype = 3 else stype = 2
      end
      'LAMB': begin
          dcflag = sxpar(head, 'DC-FLAG', count=count)
          if dcflag EQ 1 then stype = 3 else stype = 2
      end
      'WAVE': begin  ; UVES
          dcflag = sxpar(head, 'DC-FLAG', count=count)
          if dcflag EQ 1 then stype = 3 else stype = 2
       end
      'PIXE': begin ; X-shooter?
         linear = sxpar(head, 'CTYPE2', count = count)
         IF strmatch(linear, '*LINEAR*') THEN stype = 2
      end
      else: stype = -2
  endcase
      
; Make wave array

  case stype of
      -2: return, temporary(dindgen(ntot))
      2: return, crval1 + ( cdelt1 * (temporary(dindgen(ntot)) $
                                      + 1. -crpix1) ) ; Linear
      3: return, 10^(crval1 + ( cdelt1 * (temporary(dindgen(ntot)) $
                                          + 1. - crpix1) )) ; Log
      else: return, -1
  endcase
          
end
