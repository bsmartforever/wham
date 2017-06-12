FUNCTION numarical_int, X, F, range, error=error, help=help

; Purpose:
;	To quickly calculate integrated intensities and their corresponding errors using 
;	int_tabulated and int_tabulated_var. This program allows the user to pass an 
;	integration range, which is not allowed by the other two programs making them 
;	annoying. 
;
; Input:
;	x - dependent data array
;	f - function array, where f(x)
;	range - integration range. If not specitifed, [min(x),max(x)] is assumed.
;
; Optional:
;	error - error array, containing the errors of f(x). 
;
; Example:
; 	print,numarical_int(CAL_F_NI1201im.vel,CAL_F_NI1201im.column,[100.,200.],error=CAL_F_NI1201im.column_err)
;   1.1909124e+15   3.8187701e+13
;
;
; Created by Dr. Kat Barger 11/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (keyword_set(help)) then begin
	print,'FUNCTION numarical_int, X, F, range'
	return,0
endif

if (NOT keyword_set(range)) then range=[min(x),max(x)]

;Make sure that the minumum value is given first
range=[min(range),max(range)]

good=where((x ge range[0]) AND (x le range[1]),count)

if count ne 0 then begin
	value=int_tabulated(x[good],f[good],/sort,/double)

	if (n_elements(error) eq n_elements(x)) then $
	value_err=sqrt(int_tabulated_var(x[good],(error[good])^2.0,/sort,/double))

endif else begin
	print,''
	print,'*** Please specify a range between ',[min(x),max(x)],' ***',format='(A-34,f8.2,f8.2,A-4)'
	print,''
	return,0
endelse

if (NOT keyword_set(value_err)) then $
	return,value else $
	return,[value,value_err]

end