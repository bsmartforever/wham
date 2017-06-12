function axis_factor_range,loc,factor,min=min,max=max

; Purpose:
; 	To determine an axis range that yields a certain fractional distance 
;		above and below a specified value. 
;
;		This is useful when generating 
;		multiple similar plots. For example, when generating spectial line 
;		plots, all of the baselines can be aligned at the same vertical position. 
;		This is achived by setting the "loc" and the "factor" to the same values 
;		for each plot, but min or max range positions can vary.
;
; Input: 
;	loc    - The data value used to align the axis.
;	factor - [fraction below axis, fraction above axis] about the "loc" position. 
;		 		Ideally, this value totals 1, e.g., [0.1,0.9]. This would yield 
;				10% of the range below "loc" and 90% above "loc". However, whatever
;				values are passed, they are normalized before determining the 
;				fractional range above and below "loc". E.g., [200,800]=>[0.2,0.8]
;	min    - min value for range.
;	max    - max value for range.
;
;	*** MUST specify either "min" OR "max"
;
; Example:
;	plot,[1,2,3,4],ystyle=1,yrange=axis_factor_range(0,[0.2,0.8],max=6.)
;
;	This produces a plot with 20% of the y-axis below the value zero and 80% above zero with 
;	the maximum value of that range being 6. This yields yrange=[-1.5,6.].
;
; Created by Dr. Kat Barger
;	Last modified 11/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if ((NOT keyword_set(loc)) AND (NOT keyword_set(factor)) AND (NOT keyword_set(min))) OR $
	   ((NOT keyword_set(loc)) AND (NOT keyword_set(factor)) AND (NOT keyword_set(max))) then begin
	   		print,''
	   		print,'axis_factor_range(loc,factor,min=min,max=max)'
	   		return,[0,0]
	endif

	loc=float(loc)
	factor=float(factor)

	if n_elements(factor) ne 2 then begin
		print,''
		print,'*** factor must be a two element array ***'
		return,[0,0]
	endif 

	factor=abs(factor)/total(abs(factor))

	if keyword_set(min) then begin 
		if (loc gt min) then begin
			min=float(min)
			min_size=abs(loc-min)
			new_range=[min,loc+factor[1]/factor[0]*min_size]
		endif else begin
			print,''
			print,'*** loc must be greater than min ***'
			return,[0,0]
		endelse
	endif

	if keyword_set(max) then begin 
		if (loc lt max) then begin
			max=float(max)
			max_size=abs(loc-max)
			new_range=[loc-factor[0]/factor[1]*max_size,max]
		endif else begin
			print,''
			print,'*** loc must be less than max ***'
			return,[0,0]
		endelse
	endif

	if (NOT keyword_set(min)) AND (NOT keyword_set(max)) then begin
		print,''
		print,'*** Must specify either min or max value ***'
		print,'axis_factor_range(loc,factor,min=min,max=max)'
	   		return,[0,0]
	endif

	return,new_range;

end