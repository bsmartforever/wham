PRO fixbaddies_fast, map, ii, badpoints, radius = radius

  IF n_elements(radius) EQ 0 THEN radius = 1.0

  ubad = badpoints[uniq(badpoints, sort(badpoints))]
  ubad = ubad[where(ubad NE -1)]
  
  Near = LONARR(N_ELEMENTS(ubad), 7)
  NCnt = INTARR(N_ELEMENTS(ubad))
  Dist = FLTARR(N_ELEMENTS(ubad), 7)

  RESTORE, '/d/wham/pro/data/fixbaddies_wholemap.dat'
;  RESTORE, '/d/wham/pro/data/fixbaddies_0_240.dat'
  
;  HELP, near

  FOR i = 0, n_elements(ubad)-1 DO BEGIN 

;    near_tmp = spectnear(map, map[ubad[i]].glon, map[ubad[i]].glat, $
;                     radius, ncnt_tmp, dist = dist_tmp)

;    Near[i, 0:NCnt_tmp-1] = Near_tmp
;    Dist[i, 0:NCnt_tmp-1] = Dist_tmp
;    Ncnt[i] = NCnt_tmp
    
    IF ncnt[i] LE 1 THEN BEGIN 
      message, 'No pointings within '+strtrim(radius, 2)+' degrees of '+ $
        map[ubad[i]].name, /info
    ENDIF ELSE BEGIN 
    
    ;; Make sure these points are not in the bad points list as
    ;; well. This also gets rid of the original point
      good = indgen(ncnt[i])
      FOR j = 0, ncnt[i]-1 DO BEGIN 
       isbad = where(near[i, j] EQ ubad)
        IF isbad[0] NE -1 THEN good[j] = -1
      ENDFOR
      good = where(good NE -1)
      IF good[0] EQ -1 THEN BEGIN 
      message, 'No good pointings within '+strtrim(radius, 2)+' degrees of '+ $
        map[ubad[i]].name, /info
      ENDIF ELSE BEGIN 

      ;; Finally we can average some of the good points to 'fix' the
      ;; bad one. We weight by distance from the original point.

	ii[ubad[i]] = total(ii[near[i, good]]*dist[i, good])/total(dist[i, good])
      ENDELSE 
    ENDELSE
  ENDFOR 

;  SAVE, Near, NCnt, Dist, file = '/d/wham/pro/fixbaddies_0_240.dat'

END 
