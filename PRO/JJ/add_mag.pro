;+
; NAME:
;        ADD_MAG
;
; PURPOSE:
;        Add or subtract the apparent magnitudes of two stars.
;
; CATEGORY:
;
;        
;
; CALLING SEQUENCE:
;
;        RESULT = ADD_MAG(MA, MB, [=SUBTRACT]
;
; INPUTS:
;
;        MA  -  The magnitude of star A
;        MB  -  The magnitude of the star to be added to A
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;        /SUBTRACT  -  Subtract the flux of B from A
;
; OUTPUTS:
;    
;        The composite magnitude of Star A and B
;
; EXAMPLE:
;
;        IDL> ma = 7.0
;        IDL> mb = 7.0
;        IDL> print, add_mag(ma, mb)
;              6.24743
;
; MODIFICATION HISTORY:
;
; Created sometime in ought-four by JohnJohn
;
;-

function add_mag, m1, m2, subtract=subtract

if total(m1) ge total(m2) and keyword_set(subtract) then begin
    if m1 eq m2 then return,0 else begin
        message,'M1 must be less than M2 in order to subtract!',/info
        return,-999
    endelse
endif

if keyword_Set(subtract) then $
  return, m1 - 2.5*alog10(1-10d^(0.4*(m1-m2))) else $
  return, m1 - 2.5*alog10(1+10d^(0.4*(m1-m2)))
  
end
