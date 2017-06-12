; $Id: map_image.pro,v 1.22 1999/05/28 16:38:18 ali Exp $
;
; Copyright (c) 1993-1999, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.

function map_image_missing, image_orig, max_value, min_value
;
; Return an array of 1's where the data are outside the range of min_value
; to max_value.  Max_value and/or min_value may be undefined.  If both are
; undefined, return a -1.
;
COMPILE_OPT hidden

k = n_elements(max_value) * 2 + n_elements(min_value)
case k of
0: return, -1
1: return, Image_orig le min_value
2: return, Image_orig ge max_value
3: return, (Image_orig ge max_value) or (Image_orig le min_value)
endcase
end



FUNCTION  map_image, Image_Orig, Startx, Starty, xsize, ysize, $
                LATMIN = latmin,   LATMAX = latmax,  $
                LONMIN = lonmin,   LONMAX = lonmax,  $
                COMPRESS = compress, BILINEAR = bilin, $
		WHOLE_MAP = whole_map, SCALE = scalef, $
		MISSING = missing, MAX_VALUE = max_value, MIN_VALUE=min_value
;+NODOCUMENT
;NAME:
;     map_image
;PURPOSE:
;       This function returns the Image_Orig image warped to fit
;       the current map. Image_Orig must be centered at 0.  This
; 	routine works in image space.
;Category:
;        Mapping
;Calling Sequence:
;        result = map_image(Image_Orig [, Startx, Starty [, xsize, ysize]])
;INPUT:
;      Image_Orig--- A two-dimensional array representing geographical
;               image to be overlayed on map.  It has Nx columns,
;		and Ny rows.
;KEYWORDS:
;	LATMIN --- the latitude corresponding to the first row of Image_Orig.
;		The default value is -90.  Latitude and Longitude values
;		refer to the CENTER of each cell.
;	LATMAX --- the latitude corresponding to last row of Image_Orig. The
;		default is  90 - (180. / Ny).
;	LONMIN --- the longitude corresponding to the first [left] column of
;		Image_Orig. The default value is -180.  Lonmin must be
;		in the range of -180 to +180 degrees.
;	LONMAX --- the longitude corresponding to the last column
;		of Image_Orig. Lonmax must be larger than Lonmin.
;		If the longitude of the last column is equal to 
;		(lonmin - (360. /Nx)) MODULO 360. it is assumed that
;                the image covers all longitudes. 
;	BILINEAR --- A flag, if set, to request bilinear interpolation. The
;		default is nearest neighbor.  Bilinear appears much better.
;	COMPRESS --- Interpolation compression flag.  Setting this to
;		a higher number saves time --- lower numbers produce
;		more accurate results.  Setting this to 1
;		solves the inverse map transformation for every
;		pixel of the output image.  Default = 4 for output devices
;		with fixed pixel sizes. Fix is used to make this an int.
;	SCALE = pixel / graphics scale factor for devices with scalable
;		pixels (e.g. PostScript).  Default = 0.02 pixels/graphic_coord.
;		This yields an approximate output image size of 350 x 250.
;		Make this number larger for more resolution (and larger
;		PostScript files, and images), or smaller for faster
;		and smaller, less accurate images.
;	MISSING = value to set areas outside the valid map coordinates.
;		If omitted, areas outside the map are set to 255 (white) if
;		the current graphics device is PostScript, or 0 otherwise.
;	MAX_VALUE = values in Image_Orig greater than or equal to MAX_VALUE
;		are considered missing.  Pixels in the output image
;		that depend upon missing pixels will be set to MISSING.
;	MIN_VALUE = values in Image_Orig less than or equal to MIN_VALUE
;		are considered missing.
; Optional Output Parameters:
;	Startx --- the  x coordinate where the left edge of the image
;		should be placed on the screen.
;	Starty --- the y coordinate where th bottom edge of the image
;		should be placed on the screen.
;	xsize ---  returns the width of the resulting image expressed in
;		graphic coordinate units.  If current graphics device has
;		scalable pixels,  the value of XSIZE and YSIZE should
;		be passed to the TV procedure.
;	ysize ---  returns the pixel height of the resulting image, if the
;		current graphics device has scalable pixels. 
;
;Output:
;      The warped image is returned.
;
; Procedure:  An image space algorithm is used, so the time required
;	is roughly proportional to the size of the final image.
;	For each pixel in the box enclosed by the current window,
;	and enclosed by the Image, the inverse coordinate transform is
;	applied to obtain lat/lon.  The lat/lon coordinates are then scaled
;	into image pixel coordinates, and these coordinates are then
;	interpolated from Image values.
;
; Restrictions: Probably won't work on VMS cause it doesn't support
;	 NaN.
;

;MODIFICATION HISTORY:
;       CAB, Feb, 1992. Map_image has been changed to handle images
;       	crossing the international dateline in a more convenient way.
;       	Specifically, it no longer requires that the keyword LONMIN be
;       	greater than or equal to -180 or the keyword LONMAX be
;		less than or equal to 180.
;	DMS, Aug, 1992.  Completly rewritten.  Uses different algorithms.
;	DMS, Dec, 1992.  Coordinates were off by part of a pixel bin.
;		Also, round when not doing bi-linear interpolation.
;	DMS, Sep, 1993.  Added MAX_VALUE keyword.
;	DMS, Nov, 1994.  Added MIN_VALUE keyword.
;       SVP, Mar, 1995.  Compress is now fix()'d. y is now scaled correctly.
;       SVP, May, 1996.  Removed Whole_Map keyword. Changes in the noborder
;                        behavior of MAP_SET make this keyword obselete.
;	DMS, Nov, 1996.	Adapted for new maps, rev 2.
;-

ON_ERROR,2

;t0 = systime(1)
if (!x.type NE 2) and (!x.type ne 3) THEN  $        ;Need Mapping Coordinates
   message, "Current window must have map coordinates"

s = size(Image_Orig)
if s[0] ne 2 THEN message, " Image must be a two- dimensional array."
s1 = s[1]           ; # of columns
s2 = s[2]           ; # of rows
if s[1] le 1 or s[2] le 1 THEN $
    message, 'Each dimension must be greater than 1."

; If both latmin & latmax are missing, assume image covers entire globe.
if N_ELEMENTS(latmin) eq 0 then latmin = -90.
if N_ELEMENTS(latmax) EQ 0 THEN latmax = 90. ;This is as documented
; if N_ELEMENTS(latmax) EQ 0 THEN latmax = 90. - 180./s2  ;This isn't

; If both lonmin & lonmax are missing, assume image covers all longitudes
;       with duplication.
if N_ELEMENTS(lonmin) EQ 0 THEN lonmin = -180.
if N_ELEMENTS(lonmax) EQ 0 THEN lonmax = 180. ;This is as documented
; if N_ELEMENTS(lonmax) EQ 0 THEN lonmax = 180.- (360./s1)   ;This isn't

latmin = float(latmin) 	&	lonmin = float(lonmin)
latmax = float(latmax) 	&	lonmax = float(lonmax)

;	Does image wrap around globe?
wrap = ((lonmin - 360./s1 + 720.) mod 360.) - ((lonmax + 720.) mod 360.)
wrap = abs(wrap) lt 1e-4	;Allow for roundoff
ltlim = [latmin, latmax]

if wrap eq 0 then lnlim = [lonmin, lonmax] else lnlim = [-180., 180.]

; Find the extent of the our limits in the map on the screen by
;       making a n x n array of lon/lats spaced over the extent of
;       the image and saving the extrema.


scale = !d.flags and 1		;TRUE if device has scalable pixels
if n_elements(scalef) le 0 then scalef = 0.02   ;PostScript scale factor
IF scale THEN BEGIN		;Fudge for postscript?
	!x.s = !x.s * scalef
	!y.s = !y.s * scalef
ENDIF ELSE scalef = 1

if n_elements(compress) le 0 then compress = 4   $ ;Default value
else compress = fix(compress)

; Missing data value should equal the background or user-supplied value.
if n_elements(missing) le 0 then begin
  if (!d.flags and 512 ne 0) then missing = !d.n_colors-1 else missing = 0
  endif

screen_x = long(scalef * !x.window * !d.x_size) ;Map extent on screen
screen_y = long(scalef * !y.window * !d.y_size)

; See if we can use a smaller area than the plot window
if wrap eq 0 and abs(latmax-latmin) lt 90 then begin
    n = 31                      ;Subdivisions across lat/lon range.
    x = (findgen(n) * ((lnlim[1]-lnlim[0])/float(n-1)) + lnlim[0]) # $
      replicate(1.,n)
    y = replicate(1.,n) # $
      (findgen(n) * ((ltlim[1]-ltlim[0])/float(n-1)) + ltlim[0])
    x = convert_coord(x,y, /DATA, /TO_DEVICE) ;Latlon to device
    
    y = reform(x[1,*], n^2, /OVER) ;Get device coords separately
    x = reform(x[0,*], n^2, /OVER)
    good = where(finite(x))
    x = x[good] & y = y[good]
    screen_x = long([screen_x[0] > min(x, MAX=maxx), screen_x[1] < maxx])
    screen_y = long([screen_y[0] > min(y, MAX=maxy), screen_y[1] < maxy])
endif

;       Get next larger multiple of compress for resulting image size.
nx = ((screen_x[1] - screen_x[0]+compress) < !d.x_size) / compress
ny = ((screen_y[1] - screen_y[0]+compress) < !d.y_size) / compress

                        ;X and Y screen coordinates
x = (findgen(nx) * compress) # replicate(1.0,ny) + screen_x[0]
y = replicate(1.0, nx) # (findgen(ny) * compress) + screen_y[0]

x = convert_coord(x, y, /DEVICE, /TO_DATA)      ;Screen to lat/lon
y = reform(x[1,*], nx, ny, /OVER)       ;Separate lat/longit
x = reform(x[0,*], nx, ny, /OVER)

bad = where(finite(x) eq 0b, count) ;Not all machines handle NaN properly
if count ne 0 then begin        ;So fake it for points off the map.
    x[bad] = 1.0e10
    y[bad] = 1.0e10
    endif

w = where(x lt lonmin, count)        ;Handle longitude wrap-around
while count gt 0 do begin
        x[w] = x[w] + 360.0
        w = where(x lt lonmin, count)
        endwhile


sx = ((s1-1.)/(lonmax - lonmin))  ;Scale from lat/lon to pixels
sy = ((s2-1.)/(latmax - latmin))


;               Now interpolate the screen image from the original.
if KEYWORD_SET(bilin) THEN BEGIN
; If the image wraps around the globe, we must treat those pixels
; after the last column and before the first column specially. 

    badb = map_image_missing(image_orig, max_value, min_value)
    x = ((x - lonmin) MOD 360) * sx		;To pixels
    y = (y - latmin) * sy

    if wrap then begin
	col1 = where(x gt (s1-1), count)	;Wrap around elements
	if count le 0 then goto, full_image
	threecol = [Image_Orig[s1-1,*], Image_Orig[0:1,*]]
	col1x = x[col1] - (s1-1)	;Interpolate value
	if badb[0] ne -1 then begin	;Missing data value?
	    bad = interpolate(float(badb), x, y, miss=0)
	    bad[col1] = interpolate(float([badb[s1-1,*], badb[0:1,*]]), $
			col1x, y[col1], miss=0)
	    bad = where(bad, count)
	    x = interpolate(Image_Orig, x, y, miss = missing) ; full image
		;Add in points that wrapped in X between s1-1 and s1.
	    x[col1] = interpolate(threecol, col1x, y[col1], miss = missing)
	    if count gt 0 then x[bad] = missing
	endif else begin			;badb
	    x = interpolate(Image_Orig, x, y, miss = missing) ; full image
		;Add in points that wrapped in X between s1-1 and s1.
	    x[col1] = interpolate(threecol, col1x, y[col1], miss = missing)
	endelse
    endif else begin
  full_image:
	if badb[0] ne -1 then begin
	    bad = where(interpolate(float(badb), x, y, miss = 0), count)
	    x = interpolate(Image_Orig, x, y, miss = missing) ;No wrap
	    if count gt 0 then x[bad] = missing
	endif else x = interpolate(Image_Orig, x, y, miss = missing) ;No wrap
    endelse

ENDIF ELSE BEGIN		;  Scale to pixel coords & round.  
;  This is the same as: x = (x-lonmin) * sx + 0.5, but faster for arrays.
	x = (x - (lonmin - .5/sx)) * sx
	y = (y - (latmin - .5/sy)) * sy
	if wrap then begin
	    bad = where(x ge s1 and x lt (2*s1), count)
	    if count gt 0 then x[bad]=x[bad]-s1
	    endif
	bad = where(x lt 0 or x gt s1 or y lt 0 or y gt s2, count)
	if count gt 0 then begin
	    x[bad] = 0
	    y[bad] = 0
	    x = Image_Orig[x, y]
	    badb = map_image_missing(x, max_value, min_value)
	    if badb[0] ne -1 then begin
		worse = where(badb, count)
		if count gt 0 then x[worse] = missing
		endif		;Max value
	    x[bad] = missing
	endif else begin
	    x = Image_Orig[x,y]
	    badb = map_image_missing(x, max_value, min_value)
	    if badb[0] ne -1 then begin
		bad = where(badb, count)
		if count gt 0 then x[bad] = missing
		endif		;Max value
	endelse			;Count
ENDELSE

Startx = long(screen_x[0] / scalef)
Starty = long(screen_y[0] / scalef)
xsize = long(nx / scalef * compress)
ysize = long(ny / scalef * compress)

if compress ne 1 then $		;Resample to screen?
   x = rebin(x, nx*compress, ny*compress)  ;Interpolate screen image if necess.
;print,systime(1)-t0,' seconds'

IF scale THEN BEGIN		;Unfudge scale factors
	!x.s = !x.s / scalef
	!y.s = !y.s / scalef
	ENDIF	
return, x
end
