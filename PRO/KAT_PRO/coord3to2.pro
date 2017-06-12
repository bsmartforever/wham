FUNCTION coord3to2, p, data=data, device=device, normal=normal
;+
; NAME:
;	coord3to2
; PURPOSE:
;	Convert a 3D position vector to a 2D 'screen location'
; CATEGORY:
; CALLING SEQUENCE:
;	result = coord3to2(p, [,/device, /normal])
; INPUTS:
;	p			array[3,*]; type: int or float
;					coordinates of 3d vectors
; OPTIONAL INPUT PARAMETERS:
;	/data, /normal, /device
;					only one of these should be set
;					if set the input coordinates are assumed to be in data, normal
;					or device coordinates. If all are absent, then data coordinates are assumed
; OUTPUTS:
;	p			array[3,*]; same type as input
;					the converted 'screen location'
; OPTIONAL OUTPUT PARAMETERS:
; INCLUDE:
	@compile_opt.pro		; On error, return to caller
; CALLS:
;	InitVar, SyncDims
; PROCEDURE:
;	If a 3D transformation (!p.t) is in effect than all 3D vector arguments supplied to IDL
;	plot functions while using the /t3d keyword are converted to a 'screen location' for actual plotting.
;	This function is my best guess as to how this works. The x,y coordinates of the returned
;	vector indicate where on the screen (inside the plotwindow) the point would be plotted, while
;	the z-coordinate provides the 'depth' dimension perpendicular to the screen.
; MODIFICATION HISTORY:
;	AUG-1999, Paul Hick (UCSD/CASS; pphick@ucsd.edu)
;-

InitVar, normal , /key
InitVar, device , /key
data = 1-normal and 1-device

szp = size(p)

q = reform(p,szp[1],szp[szp[0]+2]/szp[1])		; Reform to [3,*] array
												; Convert to normal coordinates
IF NOT normal THEN q = convert_coord(q, data=data, device=device , /to_normal, /t3d)

; convert_coord turns an array[3,1] into an array[3] (drops trailing dimension of 1)
; so we have to check for his explicitly.

sz = size(q)
CASE sz[0] EQ 1 OF
0: q = [q,[replicate(1,1,sz[2])]]				; Convert from [3,*] to [4,*]
1: q = [q,1]
ENDCASE

; This seems to work without needing the convert_coord calls???????
; q= invert(!p.t)#q

q = transpose(transpose(q)#!p.t)				; Get 2D position

q = q[0:2,*]
												; Convert back to input coordinates
IF NOT normal THEN q = convert_coord(q, /normal, to_data=data, to_device=device , /t3d)

SyncDims, q, sizeinfo=szp

RETURN, q  &  END
