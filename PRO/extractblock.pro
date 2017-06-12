FUNCTION extractblock, map, block

  FOR i = 0, n_elements(block)-1 DO BEGIN 
    w = where(strpos(map.name, block[i]) NE -1, wcnt)

    IF wcnt EQ 0 THEN BEGIN
      message, 'No pointings found for ' + block[i], /info
      return, 0
    ENDIF ELSE BEGIN 
      ww = n_elements(ww) EQ 0 ? w : [ww, w]
    ENDELSE 

  ENDFOR 

  return, map[ww]

END 
