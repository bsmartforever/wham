function min_max, x, y, range

;Purpose - Calculate the y-min and x-max value over a specified 
;          x-range.
;
; x - data array of same size as y
; y - data array of same size as x
; range - [xmin, xmax]
;
; output - [y-min, y-max] over specified range
;
; e.g., plot,x,y,xrange=[range],yrange=min_max(x,y,range)
;
; Created 04/08/2013 by Dr. Kat Barger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF (N_PARAMS() EQ 0) OR (keyword_set(help)) THEN BEGIN 
   print,''
   print,'function min_max, x, y, range' 
   print,''
   RETURN,[0,0];
ENDIF

if (NOT keyword_set(range)) then range=[-!Values.F_NAN,!Values.F_NAN]

   good_x=where((x ge range[0]) and (x le range[1]),count)

   if count ne 0 then $
      return,[min(y[good_x],/NAN)-(max(y[good_x],/NAN)-min(y[good_x],/NAN))*0.1,$
             max(y[good_x],/NAN)+(max(y[good_x],/NAN)-min(y[good_x],/NAN))*0.1] $
   else return,[min(y,/NAN)-(max(y,/NAN)-min(y,/NAN))*0.1,$
             max(y,/NAN)+(max(y,/NAN)-min(y,/NAN))*0.1]

end