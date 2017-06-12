; $Id: rmd_heaviside.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;+
; NAME:
;       RMD_HEAVISIDE
;
; PURPOSE:
;
;       Implementation of the Heaviside step function.  This function
;		is defined as follows:
;
;		RMD_HEAVISIDE(X) = 0.0 for X < 0.0
;		RMD_HEAVISIDE(X) = 0.5 for X = 0.0
;		RMD_HEAVISIDE(X) = 1.0 for X > 0.0
;
;		This routine has been vectorized for speed.
;
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
;       Y = RMD_HEAVISIDE(X,COUNT = COUNT,/DOUBLE)
;
; INPUT PARAMETERS:
;
;       X -	A numerical array, in general.
;
; RETURNS:
;
;   The resulting array as defined above.
;
;   If no parameters are passed in a value of -1L is returned, and COUNT
;   is set to zero.  Note that the only way to recognize this error
;   is to examine COUNT.
;
; INPUT KEYWORDS:
;
;   DOUBLE -	Setting this keyword forces output with double precision.
;
; OUTPUT KEYWORDS:
;
;   COUNT - Upon return, this is the number of elements in the result.
;           This is only important when there is an error in the result,
;			in which case COUNT is set to zero.
;
; REQUIRED PROGRAMS:
;
;        NONE
;
; EXAMPLE
;
;	IDL>	xlo = -5.0 & xhi = 5.0 & npts = 100 & dx = (xhi-xlo)/(npts-1.0)
;	IDL>	x = xlo + dx*findgen(npts)
;	IDL>	y = rmd_heaviside(x-1.0)
;	IDL>	plot,x,y,yrange = [-2.0,2.0],ystyle = 1
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
function rmd_heaviside,x,count = count,double = double
if n_params() eq 0 then begin
  count = 0
  return,-1L
endif
if keyword_set(double) then begin
  a = 1D & b = 0D & c = 0.5D
endif else begin
  a = 1. & b = 0. & c = 0.5
endelse
y = (x gt 0)*a + ((x lt 0))*b + c*(x eq 0)
count = n_elements(y)
return,y
end