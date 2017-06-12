function mapgrid_gal_360flip_labels, orient, value, fractional, default

  if orient eq 0 then v = reduceto360(360-value) else v = value
  
  if fractional eq 0 then begin
    l = string(v, format = '(I4)')
  endif else begin
    l = string(v, format = '(F6.2)')
  endelse

  return, l
  
end
