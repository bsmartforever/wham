function min_max_multi, x1, y1, x2, y2, x3, y3, x4, y4, range=range, help=help

;Purpose - Calculate the y-min and x-max value over a specified 
;          x-range for multiple x and y arrays.
;
; xn - data array of same size as y
; yn - data array of same size as x
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
   print,'function min_max_multi, x1, y1, x2, y2, x3, y3, x4, y4, range=range' 
   print,''
   RETURN,[0,0];
ENDIF

if (NOT keyword_set(range)) then range=[-!Values.F_NAN,!Values.F_NAN]

if (NOT keyword_set(x2)) or (NOT keyword_set(y2)) then begin
   x2=[!Values.F_NAN]
   y2=[!Values.F_NAN]
endif 

if (NOT keyword_set(x3)) or (NOT keyword_set(y3)) then begin
   x3=[!Values.F_NAN]
   y3=[!Values.F_NAN]
endif

if (NOT keyword_set(x4)) or (NOT keyword_set(y4)) then begin
   x4=[!Values.F_NAN]
   y4=[!Values.F_NAN]
endif

y1_min_max=min_max(x1,y1,range)
y2_min_max=min_max(x2,y2,range)
y3_min_max=min_max(x3,y3,range)
y4_min_max=min_max(x4,y4,range)

min_all=min([y1_min_max[0],y2_min_max[0],y3_min_max[0],y4_min_max[0]],/NAN)
max_all=max([y1_min_max[1],y2_min_max[1],y3_min_max[1],y4_min_max[1]],/NAN)



   return, [min_all,max_all];

end 