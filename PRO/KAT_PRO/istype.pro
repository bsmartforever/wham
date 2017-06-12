FUNCTION IsType, X,	$
	undefined_x			= Undefined_X,	$
	defined_x			= Defined_X,	$
	byte_x				= Byte_X,		$
	short_integer_x		= Short_Integer_X,	$
	integer_x			= Integer_X,	$
	longword_x			= Longword_X,	$
	floating_x			= Floating_X,	$
	double_x			= Double_X,	$
	complex_floating	= Complex_floating_X,	$
	string_x			= String_X,	$
	structure_x			= Structure_X,	$
	complex_double_x	= Complex_double_X,	$
	pointer_x			= Pointer_X,	$
	object_reference_x	= Object_reference_X,	$
	unsigned_integer_x	= Unsigned_Integer_X,	$
	unsigned_short_integer_x = Unsigned_Short_Integer_X,	$
	unsigned_longword_x	= Unsigned_Longword_X,	$
	integer_64_bit_x	= Integer_64_bit_X,	$
	unsigned_integer_64_bit_x = Unsigned_Integer_64_bit_X,	$

	generic_integer		= Generic_Integer,	$
	generic_float		= Generic_Float,	$

	array				= Array,			$
	scalar				= Scalar,			$

	type				= Type,				$
	bytes				= Bytes,			$
	name				= Name

;+
; NAME:
;	IsType
; PURPOSE:
;	Check for type of variable, or get type code
; CATEGORY:
;	Toolbox: generic
; CALLING SEQUENCE:
;	R = IsType(X, /floating_x)			tests whether X is of type 'floating'
;	R = IsType(/floating_x)				returns type code for type 'floating'
;	R = IsType(X)						returns type code for X
; INPUTS:
;	X		any variable
; OPTIONAL INPUT PARAMETERS:
;	Only one of these should be set:
;	/Byte_X
;	/Integer_X
;	/Short_Integer_X		(same as Integer_X)
;	/Longword_X
;	/Floating_X
;	/Double_X
;	/Complex_floating_X
;	/String_X
;	/Structure_X
;	/Complex_double_X
;	/Pointer_X
;	/Object_reference_X
;	/Unsigned_Integer_X
;	/Unsigned_Short_Integer_X	(same as Unsigned_Integer_X
;	/Unsigned_Longword_X
;	/Integer_64_bit_X
;	/Unsigned_Integer_64_bit_X
; OUTPUTS:
;	IsTime			scalar; type: long integer
;					if argument X is set, in combination with one of the type keywords:
;						0 if argument is not of specified type; 1 if it is
;					otherwise
;						type code of X, or type code for specified type keyword
; OPTIONAL OUTPUT PARAMETERS:
;	type =type		scalar; type: integer
;						code of specified type
;	bytes=bytes		scalar; type: integer
;						# bytes in scalar of specified type
;	name =name		scalar; type: string
;						name of specified type
; INCLUDE:
	@compile_opt.pro		; On error, return to caller
; CALLS:
;	InitVar
; PROCEDURE:
;	Trivial
; MODIFICATION HISTORY:
;	APR-2000, Paul Hick (UCSD/CASS; pphick@ucsd.edu)
;-

InitVar, Undefined_X	, /key
InitVar, Defined_X		, /key
InitVar, Byte_X			, /key

InitVar, Integer_X		, /key
InitVar, Short_Integer_X, /key
Integer_X = Integer_X or Short_Integer_X

InitVar, Longword_X 	, /key
InitVar, Floating_X		, /key
InitVar, Double_X		, /key
InitVar, Complex_floating_X, /key
InitVar, String_X		, /key
InitVar, Structure_X	, /key
InitVar, Complex_double_X, /key
InitVar, Pointer_X		, /key
InitVar, Object_reference_X, /key

InitVar, Unsigned_Integer_X, /key
InitVar, Unsigned_Short_Integer_X, /key
Unsigned_Integer_X = Unsigned_Integer_X or Unsigned_Short_Integer_X

InitVar, Unsigned_Longword_X, /key
InitVar, Integer_64_bit_X, /key
InitVar, Unsigned_Integer_64_bit_X, /key

InitVar, Generic_Integer, /key
InitVar, Generic_Float	, /key

InitVar, Array			, /key
InitVar, Scalar 		, /key

; If no argument was specified then make one if keyword was specified
; (unless keyword was /Undefined_X or /Defined_X)

nparam = n_params()

IF nparam EQ 0 THEN BEGIN

	IF n_elements(value) EQ 0 THEN value = 0B ELSE value = value[0]

	     IF Byte_X						THEN X = byte	(value)	$
	ELSE IF Integer_X					THEN X = fix 	(value)	$
	ELSE IF Longword_X					THEN X = long	(value)	$
	ELSE IF Floating_X					THEN X = float	(value)	$
	ELSE IF Double_X					THEN X = double	(value)	$
	ELSE IF Complex_floating_X			THEN X = complex(value)	$
	ELSE IF String_X					THEN X = ''			$
	ELSE IF Structure_X					THEN X = {nothing_in_here, a:value} $
	ELSE IF Complex_double_X			THEN X = dcomplex(value)$
	ELSE IF Pointer_X					THEN X = ptr_new()	$
	ELSE IF Object_reference_X			THEN X = obj_new()	$
	ELSE IF Unsigned_Integer_X			THEN X = uint	(value)	$
	ELSE IF Unsigned_Longword_X			THEN X = ulong	(value)	$
	ELSE IF Integer_64_bit_X			THEN X = long64	(value)	$
	ELSE IF Unsigned_Integer_64_bit_X	THEN X = ulong64(value)

ENDIF

; Check the type of X

Type = size(X, /type)

Bytes = ([0,1,2,4,4,8,8,0,0,16,4,4,2,4,8,8])[Type]
Name  = (['Undefined','Byte','Integer','Longword','Floating','Double','Complex_floating','String',	$
			'Structure','Complex_double','Pointer','Object_reference','Unsigned_Integer',			$
			'Unsigned_Longword','Integer_64_bit','Unsigned_Integer_64_bit'])[Type]

; If argument and keyword was specified then test for type
; (argument may be undefined)

IF nparam NE 0 	AND					$
	(	Undefined_X				OR	$
		Defined_X				OR	$
		Byte_X					OR	$
		Integer_X				OR	$
		Longword_X				OR	$
		Floating_X				OR	$
		Double_X				OR	$
		Complex_floating_X		OR	$
		String_X				OR	$
		Structure_X				OR	$
		Complex_double_X		OR	$
		Pointer_X				OR	$
		Object_reference_X		OR	$
		Unsigned_Integer_X		OR	$
		Unsigned_Longword_X		OR	$
		Integer_64_bit_X		OR	$
		Unsigned_Integer_64_bit_X OR $

		Generic_Integer			OR	$
		Generic_Float				) THEN BEGIN

	IF Defined_X THEN RETURN, type NE 0

	CASE Type OF
	0:	RETURN, Undefined_X
	1:	RETURN, Byte_X					OR Generic_Integer
	2:	RETURN, Integer_X				OR Generic_Integer
	3:	RETURN, Longword_X				OR Generic_Integer
	4:	RETURN, Floating_X				OR Generic_Float
	5:	RETURN, Double_X				OR Generic_Float
	6:	RETURN, Complex_floating_X
	7:	RETURN, String_X
	8:	RETURN, Structure_X
	9:	RETURN, Complex_double_X
	10:	RETURN, Pointer_X
	11: RETURN, Object_reference_X
	12:	RETURN, Unsigned_Integer_X			OR Generic_Integer
	13: RETURN, Unsigned_Longword_X			OR Generic_Integer
	14: RETURN, Integer_64_bit_X			OR Generic_Integer
	15: RETURN, Unsigned_Integer_64_bit_X	OR Generic_Integer
	ENDCASE

ENDIF ELSE IF Scalar THEN			$

	RETURN, size(X, /n_dim) EQ 0	$

ELSE IF Array THEN					$

	RETURN, size(X, /n_dim) NE 0	$

ELSE								$

	RETURN, Type

END
