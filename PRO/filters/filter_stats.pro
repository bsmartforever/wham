pro filter_stats, profiles

  foreach p, profiles do begin 

    max_l = where(p.transmission eq max(p.transmission))
    sample = where(abs(p.lambda - p.lambda[max_l[0]]) le 1)
    max = avg(p.transmission[sample])

    half_max = where(p.transmission ge max(p.transmission)/2.0)
    
    width = p.lambda[half_max[-1]] - p.lambda[half_max[0]]
    
    center = (p.lambda[half_max[-1]] + p.lambda[half_max[0]])/2.0
    
    print, p.name, max, center, round(center), width, $
      format = '(A-20, "  Peak: ", F8.3, " Center: ", F8.2, " (", I4, ")   Width: ", F5.1)'
  
  endforeach 
  
end