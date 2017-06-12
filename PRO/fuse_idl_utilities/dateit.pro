;+
;		DATEIT.PRO - by David Sahnow
;			11 June 1991 (v2.0)
; Displays the current date in the lower left corner of a plot
;-
pro dateit

	xyouts,0,0,strmid(!stime,0,17),/normal
end
