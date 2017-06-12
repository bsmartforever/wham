; $Id: set_viewport.pro,v 1.7 2003/02/03 18:14:02 scottm Exp $
;
; Copyright (c) 1989-2003, Research Systems, Inc.  All rights reserved.
;       Unauthorized reproduction prohibited.

pro set_viewport, xmin, xmax, ymin, ymax
;+
; NAME:
;	SET_VIEWPORT
;
; PURPOSE:
;	Emulate the Version I, VMS/IDL SET_VIEWPORT procedure.
;	Sets the default position and size of the plot data window.
;
; CATEGORY:
;	Plotting.
;
; CALLING SEQUENCE:
;	SET_VIEWPORT, Xmin, Xmax [, Ymin, Ymax]
;
; INPUTS:
;	Xmin:	Minimum X normalized coordinate of the plotting data window.
; 
;	Xmax:	Maximum X normalized coordinate of the plotting data window.
;
; OPTIONAL INPUT PARAMETERS:
;	Ymin:	Minimum Y normalized coordinate of the plotting data window.
;
;	Ymax:	Maximum Y normalized coordinate of the plotting data window.
;
; KEYWORD PARAMETERS:
;	None.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	Sets !P.POSITION.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.  !P.POSITION is directly set.
;
; MODIFICATION HISTORY:
;	DMS, June, 1989.
;
;	Modified, April, 1991 to restore defaults if called with no
;			 parameters.
;-
on_error,2              ;Return to caller if an error occurs
n = n_params()
if n_elements(xmin) eq 0 then xmin = 0.
if n_elements(xmax) eq 0 then xmax = 0.

IF xmin eq xmax then begin	;Set defaults?
	!x.margin = [10,3]
	!y.margin = [4,2]
	!p.position = 0
	return
	ENDIF

IF n le 2 then begin	;Calculate Ymin and Ymax
	y = !x.margin * !d.x_ch_size / !d.x_size ;Margins in normalized coords
	ymin = y(0)
	ymax = 1.0 - y(1)
	ENDIF
!p.position = [ xmin, ymin, xmax, ymax] ;Set it
end
