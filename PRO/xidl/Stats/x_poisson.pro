;+ 
; NAME:
; x_poisson   
;    Version 1.1
;
; PURPOSE:
;  
;
; CALLING SEQUENCE:
;  alpha = x_maxsngpow( arr, NMIN=nmin )
;
; INPUTS:
;   Array
;
; RETURNS:
;
; OUTPUTS:
;  val == Best fit values
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   x_poisson
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   16-Dec-2004 Written by JXP
;-
;------------------------------------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function x_poisson, x, avg

  ; 
  if  N_params() LT 1  then begin 
      print,'Syntax - ' + $
        'prob = x_poisson( x, avg ) [v1.1]'
      return, 0.
  endif 

  prob = exp(-1.*avg) * (avg^x) / gamma(x+1.)
  return, prob

end
