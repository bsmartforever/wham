; +
; NAME: ql_conquit
;
; PURPOSE: quits ql2 and destroys all widgets
;
; CALLING SEQUENCE: ql_conquit, parentbase_id
;
; INPUTS: conbase_id (long) - widget id of the control base
;
; OPTIONAL INPUTS:                     
;
; OPTIONAL KEYWORD INPUTS:
;
; OUTPUTS: 
;
; OPTIONAL OUTPUTS;
;
; EXAMPLE:
;
; NOTES: called by ql_conmenu_event
; 
; PROCEDURES USED:
;
; REVISION HISTORY: 18DEC2002 - MWM: added comments.
; - 

pro ql_conquit, conbase_id

; quits idl and returns back to the operating system
exit

end
