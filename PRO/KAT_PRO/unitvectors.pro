FUNCTION unitvectors, r, sphere=sphere, cylin=cylin, rect=rect, degrees=degrees, ascending=ascending
;+
; NAME:
;	unitvectors
; PURPOSE:
;	Find two orthogonal unit vectors perpendicular to vector r.
; CATEGORY:
;	gen/idl/toolbox/math
; CALLING SEQUENCE:
;	axes = unitvectors(r)
; INPUTS:
;	r		array[3]; type: float
; OPTIONAL INPUT PARAMETERS:
;	/sphere, /cylin, /rect
;				indicates the coordinate system used: spherical, cylindrical
;				or rectangular; default: rectangular
;	/degrees	if spherical or cylindrical coordinates are used, setting this
;				keyword indicates that the angles are in degrees; default: radians
;	/ascending	See PROCEDURE.
; OUTPUTS:
;	axes	array[3,3]; type:float
;				x,y,z coordinates of three perpendicular unit vectors
;				axes[*,2] is a unit vector parallel to the input vectors r
; OPTIONAL OUTPUT PARAMETERS:
; INCLUDE:
	@compile_opt.pro			; On error, return to caller
; CALLS:
;	InitVar, vectorproduct, SuperArray
; RESTRICTIONS:
;	The input vector must have a non-zero length
; PROCEDURE:
; >	The unit vectors are returned so that axes[*,0], axes[*,1], axes[*,2] form
;	a right-handed coordinate system with z-axis (axes[*,2]) along the input
;	vector r, the x-axis (axes[*,0]) in the original x-y plane (i.e. pointing to
;	either the ascending or descending node)
; > If /ascending is NOT set then the y-axis (axes[*,1]) will always have a positive
;	z-component
; > If /ascending is SET then the x-axis is always pointing to the ascending
;	node. The z-component of the y-axis will have the same sign as the z-component
;	of the input vector.
; MODIFICATION HISTORY:
;	AUG-1999, Paul Hick (UCSD/CASS)
;	SEP-2006, Paul Hick (UCSD/CASS; pphick@ucsd.edu)
;		Treat special case where input vector is along z-axis separately.
;-

experimental = 0

InitVar, degrees, /key
InitVar, sphere , /key
InitVar, cylin  , /key
InitVar, rect   , /key
InitVar, ascending, /key

cylin AND= 1-sphere
rect   OR= 1-sphere AND 1-cylin

szin = size(r)							; Input vector structure

; Make sure rr is a 2-D array with dimensions [3,n]

rr = reform(r,szin[1],szin[szin[0]+2]/szin[1])	; sz[1]=3

; Convert to rectangular coordinates

CASE 1 OF
sphere: rr = cv_coord(from_sphere=rr, /to_rect, degrees=degrees)
cylin : rr = cv_coord(from_cylin =rr, /to_rect, degrees=degrees)
ELSE  :
ENDCASE

; Initialize output array with structure [3,3,n]
;	First  dim: three components
;	Second dim: three perpendicular unit vectors
;	Third  dim: (optional): # input vectors

pout = SuperArray(0.0*rr,3,after=1)			; Initialize to zero

p3   = total(rr*rr,1)						; Square of vector lengths
plen = where(p3 NE 0, nlen)					; nlen: # vectors with non-zero length

IF nlen NE 0 THEN BEGIN						; Only process vectors with non-zero length
	rr = rr[*,plen]							; Ditch zero-length vectors
	p3 = sqrt(p3[plen])						; Length of vectors (all non-zero)
	p3 = rr/([1.0,1.0,1.0]#p3)				; First unit vector: along r

	pout[*,2,plen] = p3						; Store in 'z-position' of output array

	IF experimental THEN BEGIN

		; The idea is to do the calculation without do-loops. Haven't figured out how yet.

		; Just calculating p1 as the cross-product between vectors along z-axis and r won't work,
		; since it might come out zero (if r along z-axis). The way out is to check cross-products
		; between r and unit vectors along x,y and z-axis and pick the one with the largest
		; cross product vector length.
											; rr[3,plen]
		p1 = SuperArray(0.0*rr, 3, after=1) ; p1[3,3,plen]

		p1[*,0,*] = vectorproduct([1.0,0.0,0.0],p3)	; Cross products with x,y,z unit vectors
		p1[*,1,*] = vectorproduct([0.0,1.0,0.0],p3)
		p1[*,2,*] = vectorproduct([0.0,0.0,1.0],p3)

		; ===
		tmp = total(p1*p1,1)				; lxyz[3,plen] (sum over 1st dim), Square cross product lengths
		tmp = max(tmp, dim=1, itmp) 		; itmp[plen]
		p1 = reform(p1,3,n_elements(p1)/3)	; p1[3,3*plen]
		p1 = p1[*,itmp] 					; p1[3,plen]; list of maximum cross products

		; Must be a way to get rid of this do loop

		;lxyz = total(p1*p1,1)				; lxyz[3,plen] (sum over 1st dim), Square cross product lengths
		;pi = lindgen(nlen)
		;FOR i=0,nlen-1 DO BEGIN				; Take the biggest cross product
		;	tmp = max( lxyz[*,i], itmp)
		;	pi[i] = itmp					; Remember which axis is used
		;	p1[*,0,i] = p1[*,itmp,i]
		;ENDFOR

		;p1 = reform(p1[*,0,*])				; List of maximum cross products
		; ===

		p1 = p1/([1.0,1.0,1.0]#[sqrt(total(p1*p1,1))]); Second unit vector

		p2 = vectorproduct(p3,p1)			; Third unit vector
											; (completes a right handed set of unit vectors p1,p2,p3
whatis, p1,p2,p3
		; At this point p1 and p2 are perpendicular, and p3 parallel to the input vectors r.
		; The p1-p2 plane intersects the x-y plane in a straight line.
		; I want the p1-axis along this line on the ascending node. (|| cross product z x r)

		; The line of nodes is defined by the relation (a*p1 + b*p2).z = 0 and a^2+b^2 = 1

		pout[*,0,plen] = p1
		pout[*,1,plen] = p2

	ENDIF ELSE BEGIN

		FOR i=0,nlen-1 DO BEGIN			; Loop over vectors of non-zero length

			; The counterclockwise rotation around the x-axis rotates such that
			;	unit vector along x-axis stays put (p1')
			;	unit vector along y-axis into a position with a positive y-component (p2')
			;	unit vector along z-axis into position making the same angle with the z-axis as r (p3')
			; The rotation around the z-axis rotates such that:
			;	p1' stays in the x-y plane (becomes p1)
			;	p2' keeps its positive y-component (becomes p2)
			;	p3' becomes parallel to r (p3)
			; I.e. the p1 axis is along the intersection of x-y plane and p1-p2 plane;
			;	the p3 axis is along r. p2 completes a right-handed coordinate system p1,p2,p3
			;	in terms of the nodes of the p1-p2 plane on the x-y plane, the p1 axis is toward
			;	the ascending node if r is less than 90 deg from the z-axis, and in the direction
			;	of the descending node if r is bigger than 90 degree.
			;	(the keyword 'ascending' can be used to put the p1 axis always towards the
			;	ascending node).

			; Intercept special case where vector is along z-axis to avoid
			; complication with the atan function if both arguments are zero (atan(0.0,-0.0) = pi !!)

			CASE 1 OF
			p3[0,i] EQ 0.0 AND p3[1,i] EQ 0.0: tmp = [ acos(p3[2,i]),0.0,0.0					]
			ascending 	   AND p3[2,i] LT 0.0: tmp = [-acos(p3[2,i]),0.0,atan(-p3[0,i], p3[1,i])]
			ELSE							 : tmp = [ acos(p3[2,i]),0.0,atan( p3[0,i],-p3[1,i])]
			ENDCASE

			t3d, /reset, rotate=tmp*!radeg, mat=pt

			pout[*,0,plen[i]] = ([1.0,0.0,0.0,1.0]#pt)[0:2]	; Store in 'x-position'
			pout[*,1,plen[i]] = ([0.0,1.0,0.0,1.0]#pt)[0:2]	; Store in 'y-position'
		ENDFOR

	ENDELSE

ENDIF

; Convert to spherical or cylindrical coordinates if necessary

IF NOT rect THEN BEGIN
	szout = size(pout)
	pout = reform(pout,szout[1],szout[szout[0]+2]/szout[1], /overwrite)
	pout = cv_coord(from_rect=pout, to_sphere=sphere, to_cylin=cylin, degrees=degrees)
	pout = reform(pout,szout[1:szout[0]], /overwrite)
ENDIF

szout = [szin[1],3]
IF szin[0] GT 1 THEN szout = [szout,szin[2:szin[0]]]

pout = reform(pout, szout, /overwrite)

RETURN, pout  &  END
