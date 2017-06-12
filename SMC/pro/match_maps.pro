pro match_maps, i1, i2, o1, o2, print_missing = print_missing

  ;; create two submaps with identical pointing order
  ;;   matching is based on the pointing name (not coordinates)

;; This was a great idea but took 5+ minutes to create the hash... sigh...
; 
;  i1_names = orderedhash(list(i1.name.extract('b[0-9]+_[0-9]+'), /extract), lindgen(i1.length))
;  i2_names = orderedhash(list(i2.name.extract('b[0-9]+_[0-9]+'), /extract), lindgen(i2.length))
;  
;  o1_names = orderedhash()
;  o2_names = orderedhash()

  i1_names = list(i1.name.extract('b[0-9]+_[0-9]+'), /extract)
  i2_names = list(i2.name.extract('b[0-9]+_[0-9]+'), /extract)
  
  o1_idx = list()
  o2_idx = list()
  
  foreach n, i1_names, i1_idx do begin
    w = i2_names.where(n)
    if isa(w) then begin
      if w.length ne 1 then begin
        message, 'Found more than one match for ' + n + ' in second map. Only using first match.', /info
      endif
      o1_idx.add, i1_idx
      o2_idx.add, w[0]
    endif else begin
      message, 'No match found for ' + n, /info
    endelse
  endforeach

  o1 = i1[o1_idx.toArray()]
  o2 = i2[o2_idx.toArray()]
end