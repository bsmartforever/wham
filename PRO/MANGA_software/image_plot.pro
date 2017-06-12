;----------------------------------------------------------------------
PRO image_plot, z, x, y, WINDOW_SCALE=window_scale, ASPECT=aspect, $
        INTERP=interp, CHARSIZE=chars, _EXTRA=extra
;+
;
; NAME:
;   IMAGE_PLOT
;
; PURPOSE:
;   Plot an image with axis.
;
; CATEGORY:
;   General graphics.
;
; CALLING SEQUENCE:
;   IMAGE_PLOT, Z, X, Y
;
; INPUTS:
;   Z:  The two-dimensional array to display.
;   X:  x scale (the sintax is the same as in CONTOUR)
;   Y:  y scale
;
; KEYWORD PARAMETERS:
; WINDOW_SCALE: Set this keyword to scale the window size to the image size.
;       Otherwise, the image size is scaled to the window size.
;       This keyword is ignored when outputting to devices with
;       scalable pixels (e.g., PostScript).
;
;   ASPECT: Set this keyword to retain the image's aspect ratio.
;       Square pixels are assumed.  If WINDOW_SCALE is set, the
;       aspect ratio is automatically retained.
;
;   INTERP: If this keyword is set, bilinear interpolation is used if
;       the image is resized.
;
;   _EXTRA: also accept all options accepted by PLOT
;
; OUTPUTS:
;   No explicit outputs.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   The currently selected display is affected.
;
; RESTRICTIONS:
;   None.
;
; PROCEDURE:
;   If the device has scalable pixels, then the image is written over
;   the plot window.
;
; MODIFICATION HISTORY:
;   DMS, May, 1988.
;   Michele Cappellari, May 1998:
;  (Adapted from IMAGE_CONT to use PLOT instead of CONTOUR)
;   MC, May 2000: added _EXTRA feature
;-

ON_ERROR,2                  ;Return to caller if an error occurs
sz = SIZE(z)                ;Size of image
IF sz[0] LT 2 THEN MESSAGE, 'Parameter not 2D'

;set window used by plot
if keyword_set(title) then begin
print,title
 title1=title
 title=''
endif
PLOT, [[0,0],[1,1]], /NODATA, XSTYLE=4, YSTYLE=4, charsize=chars, _EXTRA=extra


px = !X.WINDOW * !D.X_VSIZE ;Get size of window in device units
py = !Y.WINDOW * !D.Y_VSIZE
swx = px[1]-px[0]           ;Size in x in device units
swy = py[1]-py[0]           ;Size in Y
six = FLOAT(sz[1])          ;Image sizes
siy = FLOAT(sz[2])
aspi = six / siy            ;Image aspect ratio
aspw = swx / swy            ;Window aspect ratio
f = aspi / aspw             ;Ratio of aspect ratios


IF (!D.FLAGS AND 1) NE 0 THEN BEGIN                 ;Scalable pixels?
    IF KEYWORD_SET(aspect) THEN BEGIN               ;Retain aspect ratio?
        IF f GE 1.0 THEN swy=swy/f ELSE swx=swx*f   ;Adjust window size
    ENDIF
    if not keyword_set(nodata) then TVSCL, z, px[0], py[0], XSIZE=swx, YSIZE=swy, /DEVICE
ENDIF ELSE BEGIN                                    ;Not scalable pixels
    IF KEYWORD_SET(window_scale) THEN BEGIN         ;Scale window to image?
        if not keyword_set(nodata) then TVSCL, z, px[0], py[0]                      ;Output image
        swx = six                                   ;Set window size from image
        swy = siy
    ENDIF ELSE BEGIN                                ;Scale window
        IF KEYWORD_SET(aspect) THEN BEGIN
            IF f GE 1.0 THEN swy=swy/f ELSE swx=swx*f
        ENDIF                                       ;aspect
        if not keyword_set(nodata) then TV, POLY_2D(BYTSCL(z), $                    ;Have to resample image
            [[0,0],[six/swx,0]],[[0,siy/swy],[0,0]], $
            KEYWORD_SET(interp), swx, swy), px[0], py[0]
    ENDELSE                                         ;window_scale
ENDELSE                                             ;scalable pixels

PLOT, [MIN(x),MAX(x)], [MIN(y),MAX(y)], /NODATA, /NOERASE, /DEVICE, $
       POS=[px[0],py[0],px[0]+swx,py[0]+swy], /XSTYLE, /YSTYLE, charsize=chars , _EXTRA=extra

RETURN
END
;----------------------------------------------------------------------
