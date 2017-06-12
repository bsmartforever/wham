;This program takes intspect and repeats ut iver a map. The inputs, vmin and vmax, are the region over which is being integrated. 
;map - whammmap input to be integrated. For SMC, make sure map has been ae corrected before integrating
;vmin - minimum velocity to integrate over
;vmax - maximum velocity to integrate over
;hi - ?
;aplclip - ?
;badpoints - ?
; orig
;avg;
;var
;moment - This dictates whether or not I want the first or second moment.

function intmap, map, vmin = vmin, vmax = vmax, hi = hi, apclip = apclip, $
                 badpoints = badpoints, orig = orig, avg = avg, var = var, moment = moment
  
  ii = fltarr(n_elements(map))

  IF n_elements(vmin) eq 0 THEN vmin = 800
  IF n_elements(vmax) eq 0 THEN vmax = +210

  IF KEYWORD_SET(avg) THEN BEGIN 
    for i = 0L, n_elements(ii)-1 do $
      ii[i] = avgintspect(map[i], vmin, vmax, hi = hi, apclip = apclip)
  ENDIF ELSE BEGIN 
    for i = 0L, n_elements(ii)-1 do $
      ii[i] = intspect(map[i], vmin, vmax, hi = hi, apclip = apclip, moment = moment, var = var)
  ENDELSE 

  IF n_elements(badpoints) NE 0 THEN BEGIN
    orig = ii
    fixbaddies, map, ii, badpoints
  ENDIF 
  
  return, ii
end
