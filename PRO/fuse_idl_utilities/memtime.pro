pro memtime, value
;+
; NAME:
;	MEMTIME
;
; PURPOSE:
;	This procedure displays the system time and memory usage'
;
; CATEGORY:
;	General.
;
; CALLING SEQUENCE:
;	MEMTIME, Label
;
; INPUTS:
;	Value:	A numeric label which is displayed when the program is excecuted
;
; EXAMPLE:
;	memtime,4
;
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow, 20 November 1996.
;-

	print,'|'
	print,'Label ', value
	print,'Time is ',systime()
	help,/memory
	spawn,'swap -s'
	print,'|'

end
