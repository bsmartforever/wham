PRO arrow3d, tail, tip, tiplen=tiplen, tipwid=tipwid, color=color, $
;	labeltext=labeltext, labeldist=labeldist, labeloffset=labeloffset,	$
	_extra=_extra
;+
; NAME:
;	arrow3d
; PURPOSE:
;	Rudimentary way of plotting a 3D arrow
; CATEGORY:
;	Tricks
; CALLING SEQUENCE:
;	arrow3d, tail, tip, tiplen=tiplen, tipwid=tipwid
; INPUTS:
;	tail	array[3], type: any
;				x,y,z coordinates for end of arrow
;	tip		array[3], type: any
;				x,y,z coordinates for tip of arrow
; OPTIONAL INPUT PARAMETERS:
;	tiplen=tiplen	scalar, type: float, default: 0.1*(arrow length)
;					length of the arrow tip
;	tipwid=tipwid	scalar, type: float, default: 0.1*(arrow length)
;					thickness of the arrow tip
;	color=color		scalar, type: integer
;					color used to fill 'inside' of array tip
;	_extra=_extra	extra keywords passed to IDL plots and xyouts command
;					(linestyle, thick, charsize, etc.)
;
;	labeltext=labeltext
;				scalar; type: string
;					label to plotted near axis (usually near the end r1)
;
;	There are two keywords to determine label placement:
;
;	labeldist=labeldist
;				scalar; type: int or float
;					labeldist is a distance (in data coordinates) along the line from r0 to r1 where the label
;					is placed. Since this placement sometime looks messy when a strange 3D transformation
;					is in effect, labeloffset
;	labeloffset=labeloffset
;				array[2]; type: int or float
;					adjustment to the position of labeltext in x and y data coordinates
;					This is usually used to manually tweak the position determined with labeldist
;					(depending on the !p.t matrix the computed position can be awkward).
;
; OUTPUTS:
;	(none)
; INCLUDE:
	@compile_opt.pro		; On error, return to caller
; CALLS:
;	unitvectors, coord3to2
; RESTRICTIONS:
;	A valid !p.t matrix must be defined
; PROCEDURE:
;	Calls to plots and polyfill
; MODIFICATION HISTORY:
;	AUG-1999, Paul Hick (UCSD/CASS; pphick@ucsd.edu)
;-

;IF n_elements(labeldist  ) EQ 0 THEN labeldist = 0

pp = tip-tail
IF IsType(tiplen,/undefined) THEN tiplen = 0.1*sqrt(total(pp*pp)) ELSE tiplen = tiplen[0]
IF IsType(tipwid,/undefined) THEN tipwid = 0.1*sqrt(total(pp*pp)) ELSE tipwid = tipwid[0]

pp = unitvectors(pp)

tip0 = tip-tiplen*pp[*,2]

n = 101
zero2one = findgen(n)/(n-1)

tmp = 360./!radeg*zero2one
tmp = tip0#replicate(1.,n)+tipwid*(pp[*,0]#cos(tmp)+pp[*,1]#sin(tmp))
FOR i=0,n-1 DO plots, [[tip],[tmp[*,i]]], /t3d

xtip  = convert_coord(tip , /to_normal, /t3d)
xtip0 = convert_coord(tip0, /to_normal, /t3d)

xtip  = coord3to2(xtip , /normal)
xtip0 = coord3to2(xtip0, /normal)

; This probably won't work for oblique projections

IF xtip[2] LT xtip0[2] THEN BEGIN
	IF n_elements(color) EQ 0 THEN color = 220
	polyfill, tmp, /t3d, color=color
	plots, tmp, /t3d
ENDIF

plots, [[tail],[tip]], /t3d, _extra=_extra

;IF n_elements(labeltext) NE 0 THEN BEGIN
;	P = tip-tail
;	P = P/sqrt(total(P*P))
;	plot3dtext, tip+labeldist*P, labeltext, labeloffset=labeloffset, _extra=_extra
;ENDIF

RETURN  &  END
