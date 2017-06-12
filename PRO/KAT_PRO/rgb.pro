function rgb, color

;User must follow this function with "Device, Decompose=0" to 
;put the color scale back to normal afterwards.

if n_elements(color) ne 3 then begin
   print, 'Input [r,g,b] array'
   return,0;
endif

Device, Decomposed=1

return,thisColor_d = color[0] + color[1]*2L^8 + color[2]*2L^16;

end

