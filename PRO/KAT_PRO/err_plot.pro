;+
; NAME:
;	ERR_PLOT
;
; PURPOSE:
;	Plot error bars over a previously drawn plot.
;
; CATEGORY:
;	GRAPHICS
;
; CALLING SEQUENCE:
;	ERR_PLOT, Low, High	;X axis = point number.
;
;	ERR_PLOT, X, Low, High	;To explicitly specify abscissae.
;
; INPUTS:
;	Low:	A vector of lower estimates, equal to data - error.
;	High:	A vector of upper estimates, equal to data + error.
;
; OPTIONAL INPUT PARAMETERS:
;	X:	A vector containing the abscissae.
;
; KEYWORD Parameters:
;	WIDTH:	The width of the error bars.  The default is 1% of plot width.
;
; SIDE EFFECTS:
;	An overplot is produced.
;
; RESTRICTIONS:
;	Logarithmic restriction removed.
;
; PROCEDURE:
;	Error bars are drawn for each element.
;
; 	Examples:
;	To plot symmetrical error bars where Y = data values and 
;	ERR = symmetrical error estimates, enter:
;
;		PLOT, Y			;Plot data
;		ERR_PLOT, Y-ERR, Y+ERR	;Overplot error bars.
;
;	If error estimates are non-symetrical, enter:
;
;		PLOT,Y
;		ERR_PLOT, Upper, Lower	;Where Upper & Lower are bounds.
;
;	To plot versus a vector of abscissae:
;
;		PLOT, X, Y		  ;Plot data (X versus Y).
;		ERR_PLOT, X, Y-ERR, Y+ERR  ;Overplot error estimates.
;
; MODIFICATION HISTORY:
;       Based on errplot.pro from the IDL library
;       Mod. 03/96 by RCJ.  Added _extra keyword.
;	Mod. 05/15/96 by RCJ. Added documentation.
;-
pro err_plot, X, Low, High, Width = width, _extra=_extra
;
	on_error,2                      ;Return to caller if an error occurs
	if n_params(0) eq 3 then begin	;X specified?
		up = high
		down = low
		xx = x
	   endif else begin	;Only 2 params
		up = x
		down = low
		xx=findgen(n_elements(up)) ;make our own x
	   endelse

	if n_elements(width) eq 0 then width = .01 ;Default width
	width = width/2		;Centered
;
	n = n_elements(up) < n_elements(down) < n_elements(xx) ;# of pnts
	xxmin = min(!x.crange)	;X range
	xxmax = max(!x.crange)
	yymax = max(!y.crange)  ;Y range
	yymin = min(!y.crange)

	if !x.type eq 0 then begin	;Test for x linear
		;Linear in x
		wid =  (xxmax - xxmin) * width ;bars = .01 of plot wide.
	    endif else begin		;Logarithmic X
		xxmax = 10.^xxmax
		xxmin = 10.^xxmin
		wid  = (xxmax/xxmin)* width  ;bars = .01 of plot wide
	    endelse
;
	for i=0,n-1 do begin	;do each point.
		xxx = xx(i)	;x value
		if (xxx ge xxmin) and (xxx le xxmax) then begin
			plots,[xxx-wid,xxx+wid,xxx,xxx,xxx-wid,xxx+wid],$
		[down(i),down(i),down(i),up(i),up(i),up(i)],_extra=_extra
			endif
		endfor
	return
end