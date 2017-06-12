;+ 
; NAME:
; emissstrct__define
;   Version 1.1
;
; PURPOSE:
;  Structure for a simple emission line. 
;
; CALLING SEQUENCE:
;   tmp = {emissstrct}
;
; INPUTS:
;
; RETURNS:
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
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;  Written by JXP
;-
;------------------------------------------------------------------------------
pro emissstrct__define

;  This routine defines the line list structure

  tmp = {emissstrct, $
         ion: ' ', $
         wrest: 0.d, $
         wcen: 0.d, $
         sig_wc: 0.d, $
         vsigma: 0.d, $
         sig_vsig: 0.d, $
         f: 0.d, $
         gamma: 0., $
         set: 0, $              ; Groups abs lines together
         flux: 0.d, $               ; Colm (log) 
         fsig: 0.d, $
         EW: 0., $
         EWsig: 0., $
         zem: 0.d,  $          ; Redshift
         zsig: 0.d  $           ; Error
        }

end
  
         
