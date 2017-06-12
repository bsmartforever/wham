function dist_stream,lon,lat, dtip=dtip, dstart=dstart, magellanic=magellanic, npoints=npoints

;+
; Purpose: Determine the distance along the Magellanic Stream at specified lon and lat. 
;
; Assumptions: Assumes that all mlon positions along the Stream are at the same distance,
;    regardless of what mlat is. 
; 
; lon     - longitude in degrees. Default is Galactic, unless "magellanic" keyword is passed.
; lat     - latitude in degrees. Default is Galactic, unless "magellanic" keyword is passed.
; dtip    - distance of the tip of the Stream in kpc. The tip of the Stream is defined as 
;           100 degreees from the LMC, with mlon=-100 and mlat=0
; dstart  - Starting distance for the Magellanic Stream at mlon=0 and mlat=0. 
;           Default is the LMC distance at 50 kpc. 
; magellanic - Sets passed lon and lat to Magellanic coordinants. 
; npoints - Number of points to calculate the distance and position along the Stream. This is 
;           done by assuming that one point is at the start of the Stream at mlon=0, mlat=0, 
;           one at the end at mlon=-100 and mlat=0, and npoints-2 evenly spaced intervals in between. 
;           Will return a structure with glon, glat, mlon, mlat, d.
; 
; output:
;    Distances along the Stream.
;
; example: 
;    IDL> print,dist_stream([340,300,297,296],[-55,-66,-60,-64],dtip=200.)
;       109.94230       102.54420       93.411097       98.839812
;
;    IDL> help,dist_stream(dtip=40.,npoints=5),/str 
;  ** Structure <a9122d8>, 5 tags, length=160, data length=160, refs=1:
;   GLON            DOUBLE    Array[5]
;   GLAT            DOUBLE    Array[5]
;   MLON            FLOAT     Array[5]
;   MLAT            FLOAT     Array[5]
;   D               DOUBLE    Array[5]
;
;
; Created by Dr. Kat Barger 5/2013
;-

;center of LMC in Galactic coordinants
;gLMC=[280.47,-32.75] => mLMC = 0, 2.4
;But I want the Stream to be along mlat=0
mLMC=[0,0]
mag2gal,mLMC[0],mLMC[0],glon,glat
gLMC=[glon,glat]

;100 degrees down the Stream
mStream=[-100.,0.]
mag2gal,mStream[0],mStream[1],glon,glat
gStream=[glon,glat]

;Angular distance of the Magellanic Stream, starting at the LMC center
angular_dist=sphdist(gLMC[0],gLMC[1],gStream[0],gStream[1],/degrees)

dLMC=50.
dSMC=60.
if (NOT keyword_set(dstart)) then dstart=dLMC

;distance gradiant along Stream
dist_angle=(dtip-dstart)/angular_dist

if (NOT keyword_set(npoints)) then begin

	longitude=lon
	latitude=lat
	
	;convert to Magellanic coordinants
	if (NOT keyword_set(magellanic)) then gal2mag,longitude,latitude,lon,lat
	
	distance=-(lon)*dist_angle+dstart

	return,distance;
	
end else begin

	;Distance from the Sun in kpc
	dist_conditions=[dLMC,dtip]
	distarr=congrid(dist_conditions,npoints,/interp,/minus_one)
	
	;Agular distances along the Stream in Magellanic coordinants
	;don't get distorted as they are along latitude=0 contour. 
	mlonarr=congrid([mLMC[0],mStream[0]],npoints,/interp,/minus_one)
	mlatarr=congrid([mLMC[1],mStream[1]],npoints,/interp,/minus_one)

	distance=-(mlonarr)*dist_angle+dstart

	mag2gal,mlonarr,mlatarr,glonarr,glatarr

	return,{glon:glonarr,glat:glatarr,mlon:mlonarr,mlat:mlatarr,d:distance}

endelse


end