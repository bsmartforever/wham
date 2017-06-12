pro gfunct, x, a, f, pder      ; Function + partials
  bx = exp(a(1) * x)
  f= a(0) * bx + a(2)         ;Evaluate the function
  IF N_PARAMS() ge 4 THEN $   ;Return partials?
  pder= [[bx], [a(0) * x * bx], [replicate(1.0, N_ELEMENTS(f))]]
end