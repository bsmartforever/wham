FUNCTION d2r, d
    return, d*!pi/180.
END

FUNCTION zd2airmass, zd

;+
;NAME: zd2airmass
;PURPOSE: calculate airmass from zenith distance
;SYNTAX: airmass = zd2airmass(zd)
;INPUT: zd - zenith distance in degrees
;
;MODIFICATION HISTORY:
;   Written by Alex S Hill 2009-11-4, lifted wholesale from zd2airmass.c
;   zd2airmass.c attribution:
;** Copyright Jeffrey W Percival
;** *******************************************************************
;** Space Astronomy Laboratory
;** University of Wisconsin
;** 1150 University Avenue
;** Madison, WI 53706 USA
;** *******************************************************************
;** Do not use this software without permission.
;** Do not use this software without attribution.
;** Do not remove or alter any of the lines above.
;** *******************************************************************
;-

zd = abs(zd)
zd = zd < 87.

x = 1 / cos(d2r(zd)) - 1

return, 1 + x * (0.9981833 - x * (0.002875 + (x * 0.0008083)))

END
