pro whamtolsr, ell, bee, vwham, vlsr, vvwhamsrc = vvwhamsrc, vvlsrsrc = vvlsrsrc
;+
;NAME:
;       WHAMTOLSR
;
;PURPOSE:
;       CONVERT VELOCITIES FROM THE 'WHAM FRAME' TO THE 'LSR FRAME'
;       'WHAM' MOVES WITH 19.500 KM/S TOWARDS L=54.9042, B=21.9774 (from Mihalas & Binney)
;       'LSR' MOVES WITH 20.000 KM/S TOWARDS ra1900, dec1900 = 18.000, 30.000
;
;CALLING SEQUENCE:
;       WHAMTOLSR, ELL, BEE, VWHAM, VLSR
;
;INPUTS:
;       ELL: the Galactic longitude of the position.
;       BEE: the Galactic latitude.
;       VWHAM: the velocity in the WHAM frame. Can be a vector or array.
;
;OUTPUTS:
;       VLSR: the velocity in the LSR frame.
;
;HISTORY:
;       Written by Carl Heiles. 11 Sep 1998.
;       Values updated and name changed by LMH. 21 Dec 1999.
;       Added optional output of the correction components by LMH. 27 Sep 2016.
;-
  
;...FIRST, GET THE XYZ VECTOR OF THE SOURCE...
;CONVERT INPUT GALACTIC COORDS TO 1900 EPOCH EQUATORIAL COORDS...
glactc, ra1900, dec1900, 1900.0, ell, bee, 2
  
;CONVERT RA, DEC TO RADIANS...
ra = !dpi * ra1900/12.0d0
dec = !dpi * dec1900/180.0d0

;GET THE X,Y,Z VECTOR OF THE SOURCE AT EQUINOX 1900...
xx = fltarr(3)
xx[0] = cos(dec) * cos(ra)
xx[1] = cos(dec) * sin(ra)
xx[2] = sin(dec)

;...SECOND, GET THE WHAM SOLAR MOTION.
;       WHAM MOVES WITH 19.500 KM/S TOWARDS L=54.9042, B=21.9774
glactc, rawham1900, decwham1900, 1900.0, 54.9042, 21.9774, 2
;glactc, rawham1900, decwham1900, 1900.0, 56.0, 23.0, 2
;print, rawham1900,decwham1900
rawham = !dpi * rawham1900/12.0d0
decwham = !dpi * decwham1900/180.0d0
xxwham = fltarr(3)
xxwham[0] = cos(decwham) * cos(rawham)
xxwham[1] = cos(decwham) * sin(rawham)
xxwham[2] = sin(decwham)
vvwham = 19.5*xxwham

;GET THE COMPONENT OF THIS VELOCITY ALONG THE DIRECTION TO THE SOURCE...
vvwhamsrc = total(xx*vvwham)

;...THIRD, DO THE IDENTICAL STUFF FOR THE CONVENTIONAL LSR SOLAR MOTION.
;       LSR MOVES WITH 20.000 KM/S TOWARDS ra1900, dec1900 = 18.000, 30.000
ralsr = !dpi * 18.0d0/12.0d0
declsr = !dpi * 30.0d0/180.0d0
xxlsr = fltarr(3)
xxlsr[0] = cos(declsr) * cos(ralsr)
xxlsr[1] = cos(declsr) * sin(ralsr)
xxlsr[2] = sin(declsr)
vvlsr = 20.0*xxlsr
vvlsrsrc = total(xx*vvlsr)

vhelio = vwham - vvwhamsrc
vlsr = vhelio + vvlsrsrc

;print, vvwhamsrc, vvlsrsrc
;print, 'vwham, vvwhamsrc, vvlsrsrc =', vwham, vvwhamsrc, vvlsrsrc
;print, 'vhelio, vlsr = ', vhelio, vlsr

;stop
return
end
