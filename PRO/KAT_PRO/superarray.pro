;+
; NAME:
;	SuperArray
; PURPOSE:
;	Add a dimension to an array
; CATEGORY:
;	Array strangling
; CALLING SEQUENCE:
	FUNCTION SuperArray, L, N, lead=Lead, trail=Trail, after=After, before=Before
; INPUTS:
;	L		numerical scalar or array
;	N		int scalar
;				# elements in the extra dimension
; OPTIONAL INPUT PARAMETERS:
;	!!! dimensions are counted 1,..,size(L)
;
;	/lead	new dimension is first dimension
;	/trail	new dimension is last dimension
;	after=After
;			insert new dimension after dimension After
;	before=Before
;			insert new dimension before dimension Before
; OUTPUTS:
;	Array with one dimension of N elements more than input variable
; INCLUDE:
	@compile_opt.pro		; On error, return to caller
; CALLS:
;	IsType
; EXAMPLE:
;		R = SuperArray( indgen(3,4,5), 10, after=2)
;		creates an array R[3,4,10,5)
; PROCEDURE:
;	The basic building block is the expression replicate(1,N)#L
;	Reform and transpose are used to control the array structure.
; MODIFICATION HISTORY:
;	MAR-1998, Paul Hick (UCSD/CASS)
;	FEB-2000, Paul Hick (UCSD/CASS)
;		added handling of string arrays.
;	JUN-2003, Paul Hick (UCSD/CASS, pphick@ucsd.edu)
;		Fixed bug adding to array with trailing dummy dimension.	
;-

nL = (size(L))[0]			; # dimensions
IF nL EQ 0 AND N LE 1 THEN RETURN, L

	 IF keyword_set(Lead  )		 THEN Dim = 0				$
ELSE IF keyword_set(Trail )		 THEN Dim = nL				$
ELSE IF n_elements (After ) NE 0 THEN Dim = After [0]		$
ELSE IF n_elements (Before) NE 0 THEN Dim = Before[0]-1		$
ELSE								  Dim = 0

Dim = (Dim > 0) < nL

; Don't replace the following line by just s = size([L]). This
; would loose trailing dummy dimensions in s.

IF nL EQ 0 THEN s = size([L]) ELSE s = size(L)

b_str = IsType(L, /string)

CASE b_str OF
1: BEGIN
	s_str = s
	R = byte(L)
	IF (size(L))[0] EQ 1 AND n_elements(L) EQ 1 THEN R = reform(R,[size(R,/dim),1])
	s = size(R)
	Dim += 1
END
ELSE: R = L
ENDCASE

CASE Dim OF
0   : R = reform(replicate(1,N)#reform([R],s[s[0]+2]),[N,s[1:s[0]]])
s[0]: R = reform(reform([R],s[s[0]+2])#replicate(1,N),[s[1:s[0]],N])
ELSE: R = transpose(reform(replicate(1,N)#reform([R],s[s[0]+2]),	$
		[N,s[1:s[0]]]),[1+indgen(Dim),0,Dim+1+indgen(s[0]-Dim)])
ENDCASE

IF nL EQ 0 THEN R = reform(R)	; Scalar input: remove leading degenerate dimension

; R = reform(R)					; Remove degenerate dimensions

; The matrix multiplation turns byte and integer arrays into long arrays.
; Restore the input type.

IF b_str THEN s = s_str

CASE 1 OF
IsType(L, /byte_x): RETURN, byte(R)
IsType(L, /short ): RETURN, fix (R)
IsType(L, /string): RETURN, string(byte(R))
ELSE			  : RETURN, R
ENDCASE

END
