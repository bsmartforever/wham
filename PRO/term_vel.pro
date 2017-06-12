function term_vel, l, b

;+
;NAME: term_vel
;PURPOSE: calculate terminal velocity given Galactic latitude and longitude
;SYTAX: term_vel = term_vel(l, b)
;INPUTS: l, b: Galactic latitude and longitude in degrees.
;   Can be scalars or vectors.
;-

IF n_elements(l) NE n_elements(b) THEN message, $
    'TERM_VEL ERROR: l and b must be of same length'

a1 = fltarr(n_elements(l))
a2 = a1
tv = a1

idxQI = where(l GE 0 AND l LE 90)
IF NOT array_equal(idxQI, -1) THEN BEGIN
    a1[idxQI] = 0.855
    a2[idxQI] = 0.209
ENDIF

idxQIV = where(l GE 270 AND l LE 360)
IF NOT array_equal(idxQIV, -1) THEN BEGIN
    a1[idxQIV] = -0.829
    a2[idxQIV] = -0.221
ENDIF

idxQ23 = where(l GT 90 AND l LT 270)

tv = 220 * cos(b * !dtor) * (a1 + a2*abs(sin(l*!dtor)) - sin(l*!dtor))
IF NOT array_equal(idxQ23,-1) THEN tv[idxQ23] = 0.

return, tv

end
