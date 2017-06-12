pro lonspan, lon, span, lonmid = lonmid, full = full

  ;; Compute the endpoints of a set of longitudes. We return a two element array, 
  ;;   which (using astronomical convensions) corresponds to the [right, left] 
  ;;   edges of the dataset.

  ;; Find gap
  lon_sorted = lon.sort(indices = ls_i)
  dl = lon_sorted - lon_sorted.shift(1)
  dl[0] += 360
  dl_sorted = dl.sort(indices = ds_i)
  dl_max = dl_sorted[-1]
  dl_2nd_max = dl_sorted[-2]
  
  if (dl_max - dl_2nd_max) gt 0.5 then begin
    left = lon_sorted[ds_i[-1] - 1]
    right = lon_sorted[ds_i[-1]]
    full = 0
  endif else begin
    left = 360.0
    right = 0.0
    full = 1
  endelse
  
  span = reduceto360([right, left])
  lonmid = (left + right) / 2.0

end
