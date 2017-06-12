function dist_angle, l1, b1, l2, b2, d=d, adist=adist, magellanic=magellanic, radec=radec, angle=angle

;+
; Purpose - To calculate the physical distane between two points 
;			on a sphere, given a line-of-sight distane.
;
; Inputs:
; 	l1, b1, l2, b2 - longitude and latitude of the two locations in degrees.
;		Assumes Galactic coordinants unless the magellanic keyword is set.
;		If radec keyword is set, then they are assumed to be ra and dec.
;	d - line-of-sight distance to objects in kpc. Assumes the same distance to each.
;   adist - angular distance between objects. Default in degrees.
;   magellanic - sets the input longitue and latitude to magellanic coordinants. 
;
; Optional:
;	radec - Sets l1, b1, l2, b2 to ra1, dec1, ra2, dec2 in degrees
;
;
; Outpupt:
;	distance - distance between objects in kpc.
;
; Examples:
;	print,dist_angle(ten(5.,3.,3.93),ten(-66.,33.,46.50),ten(5.,3.,8.84),ten(-66.,40.,57.30),d=50.,/radec)
;		0.10466887 => 104.7 pc.
;
;	print,dist_angle(ten(5.,3.,3.93),ten(-66.,33.,46.50),ten(5.,3.,8.84),ten(-66.,40.,57.30),/radec,/angle)*60.
;		7.1965013 => arcmin (note the times by 60. at the end of the expression to convert to arcmins)
;
; Created by Dr. Kat Barger 06/2013
;-
if n_elements(l1) ne 0 then begin
	l1_orig=l1
	b1_orig=b1
	l2_orig=l2
	b2_orig=b2
endif

if (NOT keyword_set(d)) then angle=1.

if (NOT keyword_set(adist)) then begin
	if n_elements(radec) ne 0 then begin
		glactc,[l1,l2],[b1,b2],2000,glon,glat,1,/degree
		l1=glon[0] & l2=glon[1]
		b1=glat[0] & b2=glat[1]
	endif

	if keyword_set(magellanic) then begin
		mag2gal,[l1,l2],[b1,b2],glon,glat
		lon1=glon[0] & lon2=glon[1]
		lat1=glat[0] & lat2=glat[1]
	endif else begin
		lon1=l1 & lon2=l2
		lat1=b1 & lat2=b2
	endelse
	adist=sphdist(lon1,lat1,lon2,lat2,/degrees)
endif

if keyword_set(angle) then return, adist;

distance=d*adist*!pi/180.

if n_elements(l1) ne 0 then begin
	l1=l1_orig
	b1=b1_orig
	l2=l2_orig
	b2=b2_orig
endif

return, distance;

end