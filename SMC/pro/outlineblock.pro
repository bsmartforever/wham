PRO outlineblock, map, blocks, label = label, color = color

  on_error, 2
  
  FOR i = 0, n_elements(blocks)-1 DO BEGIN 
    bwhere = where(strpos(map.name, blocks[i]) NE -1, cnt)
    
    IF cnt NE 0 THEN BEGIN 
      lonmin = min(map[bwhere].glon)
      lonmax = max(map[bwhere].glon)
      latmin = min(map[bwhere].glat)
      latmax = max(map[bwhere].glat)
      
      if n_elements(color) ne 0 then begin 
        
      endif else begin 
        plots, -[lonmin, lonmin, lonmax, lonmax, lonmin], $
               [latmin, latmax, latmax, latmin, latmin]

        IF keyword_set(label) THEN BEGIN 
          xyouts, -avg(map[bwhere].glon), avg(map[bwhere].glat), blocks[i], $
                  /data, align = 0.5
        ENDIF 
      ENDELSE  

    ENDIF ELSE BEGIN
      message, 'No pointings of ' + blocks[i] + ' in this map'
    ENDELSE
  ENDFOR 

END 
