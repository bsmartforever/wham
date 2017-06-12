;+ 
; NAME:
; dla_calcabnd
;  V1.1
;
; PURPOSE:
;    Calculates the abundance relative to the Sun, [X/Y]
;
; CALLING SEQUENCE:
;   dla_calcabnd, dla, nn, X, Y, ans, sig
;
; INPUTS:
;   dla -- DLA structure array
;   nn  -- Index of the structure
;   X   -- Atomic number of first element
;   Y   -- Atomic number of second element
;
; RETURNS:
;
; OUTPUTS:
;  ans --  [X/Y]
;  sig --  error in [X/Y]
;
; OPTIONAL KEYWORDS:
;  /NOSIGY -- Do not include error in Y in the calculation
;     This is only useful when dealing with [X/H]
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   dla_allabd, dla
;
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   06-Oct-2004 Written by JXP
;- 
;------------------------------------------------------------------------------
pro dla_calcabnd, dla, nn, X, Y, ans, sig, NOSIGY=nosigy

  if (N_params() LT 4) then begin 
    print,'Syntax - ' + $
             'dla_calcabnd, dla, nn, X, Y, ans, sig, /NOSIGY [v1.1]'
    return
  endif 

  ;; Get abundances
  getabnd, nm, X, Xabnd, flag=1
  getabnd, nm, Y, Yabnd, flag=1

  ;; Calculate [X/Y]

  if X NE 1 then begin
      logX = alog10(dla[nn].elm[X].clm)
      logXsig = sqrt(((1./(alog(10.0)*dla[nn].elm[X].clm))^2)* $
                     dla[nn].elm[X].sigclm^2)
  endif else begin
      logX = dla[nn].elm[X].clm
      logXsig = dla[nn].elm[X].sigclm
  endelse
  if Y NE 1 then begin
      logY = alog10(dla[nn].elm[Y].clm)
      logYsig = sqrt(((1./(alog(10.0)*dla[nn].elm[Y].clm))^2)* $
                     dla[nn].elm[Y].sigclm^2)
  endif else begin
      logY = dla[nn].elm[Y].clm
      logYsig = dla[nn].elm[Y].sigclm
  endelse

  ans = logX - logY - Xabnd + Yabnd
  
  ;;     Error
  
  if keyword_set(NOSIGY) then logYsig = 0.
  sig = sqrt(logXsig^2 + logYsig^2)


  return
end

