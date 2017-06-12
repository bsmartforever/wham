pro rmblocks, map, blocks, removed = removed, quiet = quiet

  ;; Remove blocks from a map. Optionally return the removed blocks via REMOVED keyword

  ;; TODO:
  ;; Using .filter with a lambda function looked to be shorter, but had problems using
  ;; LIST objects in the lambda... could explore more to shorten this up even more.

  map_list = list(map, /extract)
  removed_list = list()
  
  matched = list()
  foreach b, blocks do begin
    w = where((map.name).matches(b + "_"), count)
    if count eq 0 then begin
      if ~keyword_set(quiet) then message, 'No pointings found for ' + b, /info
    endif else begin
      matched.add, w, /extract
    endelse 
  endforeach

  ;; Do something as long as we have matched blocks  
  if ~matched.isempty() then begin
    removed_list = map_list.remove(matched.toarray())
    map = map_list.toarray()
    removed = removed_list.toarray()
  endif else begin
    if ~keyword_set(quiet) then message, 'Nothing matched, nothing removed', /info
  endelse
    
end
  