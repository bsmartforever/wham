;-------------------------------------------------------------
;+
; NAME:
;       SPHDIST
; PURPOSE:
;       Angular distance between points on a sphere.
; CALLING SEQUENCE:
;       d = sphdist(long1, lat1, long2, lat2)
; INPUTS:
;       long1 = longitude of point 1, scalar or vector
;       lat1 = latitude of point 1, scalar or vector
;       long2 = longitude of point 2, scalar or vector
;       lat2 = latitude of point 2, scalar or vector
;
; OUTPUTS:
;       d = angular distance between points (in radians unless /DEGREES
;           is set.)
; PROCEDURES CALLED:
;       RECPOL, POLREC
; NOTES:
;       (1) The procedure GCIRC is similar to SPHDIST(), but may be more 
;           suitable for astronomical applications.
;
;       (2) If long1,lat1 are scalars, and long2,lat2 are vectors, then
;           SPHDIST returns a vector giving the distance of each element of 
;           long2,lat2 to long1,lat1.   Similarly, if long1,lat1 are vectors,
;           and long2, lat2 are scalars, then SPHDIST returns a vector giving
;           giving the distance of each element of long1,lat1 to to long2,lat2. 
;           If both long1,lat1 and long2,lat2 are vectors then SPHDIST returns
;           vector giving the distance of each element of long1,lat1 to the 
;           corresponding element of long2, lat2.   If the input vectors are 
;           not of equal length, then excess elements of the longer ones will 
;           be ignored.
; MODIFICATION HISTORY:
;       R. Sterner, 5 Feb, 1991
;       R. Sterner, 26 Feb, 1991 --- Renamed from sphere_dist.pro
;
; Copyright (C) 1991, Johns Hopkins University/Applied Physics Laboratory
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.  Other limitations apply as described in the file disclaimer.txt.
;	Converted to IDL V5.0   W. Landsman   September 1997
;-
;-------------------------------------------------------------
 
	function sphdist, xpos1, ypos1, xpos2, ypos2, $
	  help=hlp, degrees=degrees, double=double,$
	  radec=radec

	if (n_params(0) lt 4) or keyword_set(hlp) then begin
	  print,' Angular distance between points on a sphere.'
	  print,' d = sphdist(long1, lat1, long2, lat2)'
	  print,'   long1 = longitude of point 1.         in'
	  print,'   lat1 = latitude of point 1.           in'
	  print,'   long2 = longitude of point 2.         in'
	  print,'   lat2 = latitude of point 2.           in'
	  print,'   d = angular distance between points.  out'
	  print,' Keywords:'
	  print,' Notes: points 1 and 2 may be arrays.'
	  return, -1
	endif

	if keyword_set(radec) then begin
		glactc,xpos1,ypos1,2000,long1,lat1,1,degree=degree 
		glactc,xpos2,ypos2,2000,long2,lat2,1,degree=degree 
	endif else begin 
	   long1=xpos1 & lat1=ypos1 
	   long2=xpos2 & lat2=ypos2 
	endelse 

 
	cf = 1.0
	if (keyword_set(double)) then cf = !dradeg $
	else if (keyword_set(degrees)) then cf = !radeg
 
	;--- Convert both points to rectangular coordinates. ---
	polrec, 1.0, lat1/cf, rxy, z1, double=double
	polrec, rxy, long1/cf, x1, y1, double=double
	polrec, 1.0, lat2/cf, rxy, z2, double=double
	polrec, rxy, long2/cf, x2, y2, double=double
 
	;--- Compute vector dot product for both points. ---
	cs = x1*x2 + y1*y2 + z1*z2
 
	;--- Compute the vector cross product for both points. ---
	xc = y1*z2 - z1*y2
	yc = z1*x2 - x1*z2
	zc = x1*y2 - y1*x2
	sn = sqrt(xc*xc + yc*yc + zc*zc)
 
	;--- Convert to polar.  ------
	recpol, cs, sn, r, a
	return, cf*a
 
	end
