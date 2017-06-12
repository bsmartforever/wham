;+
; NAME:
;	conv_vartype
; PURPOSE:
;	Convert input variable to specified other type.
; CALLING:
;	cvar = conv_vartype( variable, TYPE_CODE_OUT=type_out )
; INPUTS:
;	variable = array, the variable to be converted.
; KEYWORDS:
;	TYPE_CODE_OUT = the desired type code of returned array.
; OUTPUTS:
;	Returns array of converted data.
; HISTORY:
;	Written, Frank Varosi NASA/GSFC 1991.
;-

function conv_vartype, variable, TYPE_CODE_OUT=type_out

	if N_elements( variable ) LE 0 then return,(-1)
	if N_elements( type_out ) NE 1 then return, variable

	s = size( variable )
	if (type_out EQ s( s(0)+1 ) ) then return, variable

	CASE type_out OF
		1:	return, byte( variable )
		2:	return, fix( variable )
		3:	return, Long( variable )
		4:	return, float( variable )
		5:	return, double( variable )
		6:	return, complex( variable )
		7:	return, string( variable )
		else:	return, variable
	ENDCASE
end