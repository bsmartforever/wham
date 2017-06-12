PRO replacepointings, map, new

  new_names = stregex(new.name, 'b[0-9]+_[0-9]+\.', /extr)
  
  FOR i = 0, n_elements(new)-1 DO BEGIN 
    r = where(strpos(map.name, new_names[i]) NE -1, rcount)
    IF rcount EQ 0 THEN BEGIN 
      message, 'No match found for element ' + strtrim(i, 2), /info
    ENDIF ELSE IF rcount EQ 1 THEN BEGIN 
      map[r] = new[i]
    ENDIF ELSE BEGIN 
      message, 'Multiple matches found for element ' + strtrim(i, 2), /info
    ENDELSE 
  ENDFOR 
  
END
