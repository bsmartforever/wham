pro simple_ticks, data, tickspace, maxticks, tickvals, nticks, padding=padding
;+
; NAME:
;	simple_ticks
; PURPOSE:
;	Setup simple tick marks for an axis of graph or contour,
;	(with simple units like pixels).
;	Assumes that data range is larger than unity. 
; CALLING:
;	simple_ticks, data, tickspace, maxticks, tickvals, nticks
; INPUTS:
;	data, tickspace, maxticks
; OUTPUTS:
;	tickspace, tickvals, nticks
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1989.
;-

	dmax = max( data, MIN=dmin )


print,dmin,dmax
   if (keyword_set(padding)) then begin
        dmin = dmin+dmin*padding
        dmax = dmax-dmax*padding
   endif
print,dmin,dmax

	dran = dmax-dmin

	if (dran LE 1) then begin
		nticks = 1
		tickspace = 0
		tickvals = [dmin,dmax]
		return
	   endif

	CASE 1 OF
		(dran LE 2):	tickspace = 0.5
		(dran LE 10):	tickspace = 1 
		(dran LE 35):	tickspace = 5 
		(dran LE 70):	tickspace = 10 
		(dran LE 140):	tickspace = 20
		(dran LE 400):	tickspace = 50
			else:	tickspace = 100
	ENDCASE

	while ( fix( dran/tickspace ) GT maxticks ) do tickspace = 2*tickspace

	tmax = tickspace * fix( dmax/tickspace )
	tmin = tickspace * fix( dmin/tickspace )

	nticks = fix( (tmax-tmin)/tickspace )
	tickvals = (tmax-tmin) * findgen( nticks+1 )/nticks + tmin

	w = where( (tickvals GE dmin) AND (tickvals LE dmax), nticks )
	tickvals = tickvals(w)

	nticks = nticks-1
	tickspace = tickspace > 2
	if (tickspace GT 20) then tickspace = tickspace/10

return
end