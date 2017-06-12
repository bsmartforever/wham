function avgintspect, pointing, vmin, vmax, hi = hi, apclip = apclip

  IF keyword_set(hi) || ~keyword_set(apclip) THEN BEGIN 
    velslice = where(vmin LE pointing.vel AND pointing.vel LE vmax, vcount)
  ENDIF ELSE BEGIN 

    ;; avoid aperture cutoff
    vpmin = max(pointing.vel) - 203
    
    velslice = where(vmin LE pointing.vel AND pointing.vel LE vmax AND $
                     pointing.vel GE vpmin, vcount)
  ENDELSE 
  
  IF vcount LE 1 THEN begin
    return, 0 
  ENDIF ELSE BEGIN 
    return, int_tabulated(pointing.vel[velslice], pointing.data[velslice]) / $
            (max(pointing.vel[velslice])-min(pointing.vel[velslice]))
  ENDELSE 
end
