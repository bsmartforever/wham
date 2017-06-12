; ; This integrates over the entire velocity window given. IF moment is set, then it can integrate the 1st and 2nd moment instead of the total intensity.

function intspect, pointing, vmin, vmax, hi = hi, apclip = apclip, var=var, moment = moment

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
     ; what I am trying to do here is fmom calculates the first moment, which is needed when calculating the second moment
     ;If it is the first moment being calculated, fmom is multiplied by zero and does notfactor intabulated
     ;Then, in the second call of int_tabulated which is returned, the altered data set is sent thtough and divided by the 0th
     ;moment. If moment is 0, then it just normally calculates
    if keyword_set(var) then return, int_tabulated_var(pointing.vel[velslice], pointing.var[velslice])
      IF moment gt 0 THEN BEGIN
        ;calculate first moment. So integrate I(v)*v. Need to be normalized by the 0th moment. zmom is the zeroth moment
        IF moment eq 1 THEN BEGIN
          zmom= int_tabulated(pointing.vel[velslice], pointing.data[velslice])
          d = keyword_set(moment) ? (pointing.data[velslice] * pointing.vel[velslice] ^ moment) : (pointing.data[velslice])
          return, int_tabulated(pointing.vel[velslice], d)/zmom

          ; if it isn't the 1st moment that means the second moment was caled, so this calculates the 1st and 0th, and uses that 
          ; to calculate the 2nd
        ENDIF ELSE BEGIN 
          zmom= int_tabulated(pointing.vel[velslice], pointing.data[velslice])
          d = keyword_set(moment) ? (pointing.data[velslice] * pointing.vel[velslice] ^(moment-1) ): (pointing.data[velslice])
          fmom = int_tabulated(pointing.vel[velslice], d)/zmom
          e = keyword_set(moment) ? (pointing.data[velslice] * (pointing.vel[velslice] - fmom )^moment) : (pointing.data[velslice])
          return, sqrt(int_tabulated(pointing.vel[velslice], e)/zmom)
        ENDELSE
      ENDIF ELSE BEGIN
       return, int_tabulated(pointing.vel[velslice], pointing.data[velslice])

    ENDELSE
  ENDELSE 
end
