; $Id: rmd_makepoints.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;+
; NAME:
;       RMD_MAKEPOINTS
;
; PURPOSE:
;
;       Generates a vector of evenly spaced points between two limits.
;
; AUTHOR:
;
;       Robert M. Dimeo, Ph.D.
;		NIST Center for Neutron Research
;       100 Bureau Drive
;		Gaithersburg, MD 20899
;       Phone: (301) 975-8135
;       E-mail: robert.dimeo@nist.gov
;       http://www.ncnr.nist.gov/staff/dimeo
;
; CATEGORY:
;
;       Mathematics
;
; CALLING SEQUENCE:
;
;       Y = RMD_MAKEPOINTS(XLO = XLO, XHI = XHI, NPTS = NPTS)
;
; INPUT PARAMETERS:
;
;       X -	A numerical vector.
;
; RETURNS:
;
;   A vector of NPTS evenly spaced points between XLO and XHI.
;
;
; INPUT KEYWORDS:
;
;   XLO			-lower limit (default: 0.0)
;	XHI			-upper limit (default: 1.0)
;	NPTS		-number of points in resulting vector (default: 10)
;
; OUTPUT KEYWORDS:
;
;		NONE
;
; REQUIRED PROGRAMS:
;
;       NONE
;
; EXAMPLE
;
;	IDL>	phi = rmd_makepoints(xlo = 0.0,xhi = 360.0,npts = 100)
;	IDL>	plot,phi,cos(phi*!dtor)
;
;	DISCLAIMER
;
;		This software is provided as is without any warranty whatsoever.
;		Permission to use, copy, modify, and distribute modified or
;		unmodified copies is granted, provided this copyright and disclaimer
;		are included unchanged.
;
; MODIFICATION HISTORY:
;
;       Written by Rob Dimeo, September 6, 2002.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function rmd_makepoints,xlo = xlo, $
                        xhi = xhi, $
                        npts = npts

if n_elements(xlo) eq 0 then xlo = 0.0
if n_elements(xhi) eq 0 then xhi = 1.0
if n_elements(npts) eq 0 then npts = 10
dx = (xhi-xlo)/(npts-1.0)
x = xlo+dx*findgen(npts)
return,x
end