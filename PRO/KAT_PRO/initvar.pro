PRO InitVar, X, Xinit, keyword_var=keyword_var, set=set, count=count

;+
; NAME:
;	InitVar
; PURPOSE:
;	Initialize a variable
; CATEGORY:
;	gen/toolbox
; CALLING SEQUENCE:
;	InitVar, X, Xinit [, /keyword_var], set=set
; INPUTS:
;	X				any variable
;	Xinit			any variable; used to initialize X
; OPTIONAL INPUT PARAMETERS:
;	/keyword_var	if set, X is assumed to be a keyword the initialization
;					then is done with the command: X = keyword_set(X)
;	set=set 		instead of initializing X, the variable 'set' is initialized.
;					The content of X is transferred to 'set' if X exists.
;					(this keyword is ignored if /keyword_var is set).
;	count=count	scalar; type: integer; default: 0
;					# elements in X that will trigger the initialization
;					(ignored if /keyword_var is set).
; OUTPUTS:
;	X				any variable; the initialized variable
; INCLUDE:
	@compile_opt.pro			; On error, return to caller
; PROCEDURE:
;	If neither X nor Xinit exist, program is terminated
; MODIFICATION HISTORY:
;	JUL-2002, Paul Hick (UCSD/CASS)
;	JAN-2005, Paul Hick (UCSD/CASS; pphick@ucsd.edu)
;		Added keyword set=set.
;-

CASE keyword_set(keyword_var) OF

0: BEGIN

	IF n_elements(count) EQ 0 THEN count = 0

	IF n_elements(X) EQ count THEN BEGIN
		CASE n_elements(Xinit) OF
		0   : message, /info, 'no data for initialization'
		ELSE: IF arg_present(set) THEN set = Xinit ELSE X = Xinit
		ENDCASE
	ENDIF ELSE	$
		set = X

END

1: X = keyword_set(X)

ENDCASE

RETURN  &  END
