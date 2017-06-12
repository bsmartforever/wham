; $Id: set_xy.pro,v 1.8 2003/02/03 18:14:02 scottm Exp $
;
; Copyright (c) 1989-2003, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

pro set_xy, xmin, xmax, ymin, ymax
;+
; NAME:
;	SET_XY
;
; PURPOSE:
;	This procedure emulates the Version I, VMS/IDL SET_XY procedure
;	to set the default axis ranges. 
;
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	SET_XY, Xmin, Xmax [, Ymin, Ymax]
;
; INPUTS:
;	Xmin:	Minimum X data coordinate of the plotting data window.
;	Xmax:	Maximum X data coordinate of the plotting data window.
;
; OPTIONAL INPUT PARAMETERS:
;	Ymin:	Minimum Y data coordinate of the plotting data window.
;	Ymax:	Maximum X data coordinate of the plotting data window.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	No explicit outputs.
;
; SIDE EFFECTS:
;	Sets the RANGE, CRANGE, and S fields of !X and !Y.
;
; RESTRICTIONS:
;	SET_XY should only be used to emulate VMS Version I of IDL.
;	This procedure does a number of things which generally should
;	not be done.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DMS, June, 1989.
;-
on_error,2              ;Return to caller if an error occurs
n = n_params()
if n eq 0 then begin	;Reset if no params?
	!x.range = 0
	!y.range = 0
	endif
if n ge 2 then begin	;set X ?
	!x.range = [ xmin, xmax]
	!x.crange = !x.range
	if !x.window(0) eq !x.window(1) then begin ;Window already set?
		tmp = !x.margin*!d.x_ch_size / !d.x_size
		!x.window = [ tmp(0), 1.0 - tmp(1)]
		endif ;window set
		;Compute slope and intercept
	if (xmax ne xmin) then $
	  !x.s(1) = (!x.window(1) - !x.window(0)) / (xmax - xmin) $
	else !x.s(1) = 1.
	!x.s(0) = !x.window(0) - !x.s(1) * xmin
	endif		;X present
	
if n ge 4 then begin	;Do Y
	!y.range = [ ymin, ymax]
	!y.crange = !y.range
	if !y.window(0) eq !y.window(1) then begin ;Window already set?
		tmp = !y.margin*!d.y_ch_size / !d.y_size
		!y.window = [ tmp(0), 1.0 - tmp(1)]
		endif ;window set
		;Compute slope and intercept
	if ymax ne ymin then $
	  !y.s(1) = (!y.window(1) - !y.window(0)) / (ymax - ymin) $
	else !y.s(1) = 1.0
	!y.s(0) = !y.window(0) - !y.s(1) * ymin
	endif			;Y present
end
