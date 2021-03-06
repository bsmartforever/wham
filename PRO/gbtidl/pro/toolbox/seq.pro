;+
; Generates a sequence of numbers in an array
;
; @param s_beg {in}{required}{type=int} first value
; @param s_end {in}{required}{type=int} last value
; @param s_inc {in}{optional}{type=int} increment
; 
; @returns long integer array
;
; @version $Id: seq.pro,v 1.2 2005/04/20 17:22:41 bgarwood Exp $
;-

function seq,s_beg,s_end,s_inc
	compile_opt idl2
	if n_elements(s_inc) eq 0 then s_inc = 1
	num = (s_end - s_beg)/s_inc + 1
	a = lindgen(num)*s_inc + s_beg
	return, a
end
