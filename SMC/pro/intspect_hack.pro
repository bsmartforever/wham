function intspect, pointing, vmin, vmax, hi = hi, apclip = apclip, var=var, moment = moment

  IF keyword_set(hi) || ~keyword_set(apclip) THEN BEGIN 
    velslice = where((vmin LE pointing.vel AND pointing.vel LE vmax) and $
                      not (pointing.vel eq 0 and pointing.data eq 0), vcount)
  ENDIF ELSE BEGIN 

    ;; avoid aperture cutoff
    vpmin = max(pointing.vel) - 203

    velslice = where((vmin LE pointing.vel AND pointing.vel LE vmax) AND $
                     (pointing.vel GE vpmin) and $
                     not (pointing.vel eq 0 and pointing.data eq 0), vcount)
  ENDELSE 
  
  IF vcount LE 1 THEN begin
    return, 0 
  ENDIF ELSE BEGIN 
    if keyword_set(var) then return, int_tabulated_var(pointing.vel[velslice], pointing.var[velslice])
    
    d = keyword_set(moment) ? (pointing.data[velslice] * pointing.vel[velslice] ^ moment) : (pointing.data[velslice])

    return, int_tabulated(pointing.vel[velslice], d)
  ENDELSE 
end
