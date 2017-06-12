;+
; NAME: arm_multgauss
;       
; CATEGORY: math
;
; PURPOSE: evaluate multiple gaussians plus underlying polynomial
;
; CALLING SEQUENCE: result = ARM_MULTGAUSS(x, p, [ngauss=, /quiet])
;
; INPUTS:
;   x - array of independent values
;   p - array of 3xNGAUSS gaussian parameters [optionally] followed by
;       polynomial coefficients, such that: 
;         P[3xn+0] - nth gaussian amplitude
;         P[3xn+1] - nth gaussian center
;         P[3xn+2] - nth gaussian sigma
;         P[3xNGAUSS+m] - mth order polynomial coefficient
;
; OPTIONAL INPUTS:
;   ngauss - number of gaussian curves; unnecessary if P contains no 
;            non-gaussian parameters (polynomial coefficients)
;   parinfo - allows the fixing of various paramaters, 
;                including Gauss width, center, height.
;   _extra - the only purpose of this parameter is for passing NGAUSS
;            through MPFITFUN() by ARM_MULTGAUSSFIT()
;
; KEYWORDS:
;   quiet - suppress text messages
;
; OUTPUTS:
;   returns evaluation of gaussians and underlying polynomial for X
; 
; OPTIONAL OUTPUTS:
;
; EXAMPLE:
;
;   IDL> x = LINDGEN(100)
;   IDL> g1 = [10,50,14] & g2 = [5,25,5] ; parameters for 2 gaussians
;   IDL> c = [5,0.05] ; coefficients for a 1st order polynomial
;   IDL> y = ARM_MULTGAUSS(x, [g1,g2,c], ngauss=2)
;
; PROCEDURES USED:
;
; COMMENTS: ARM_MULTGAUSSFIT is a complimentary fitting routine
; 
; BUG REPORT: Please report any bugs to Andrew R. Marble.
;
; MODIFICATION HISTORY:
;    written by A.R.Marble, Steward Obs., 2005 Aug 18
;    -Added 'ip' (instrument profile) to functargs so it can modify 
;     the assumed function through mpfitfun. Dr. Kat Barger 02/2014
;
; Copyright (C) 2005, Andrew R. Marble
; 
; This program is free software; you can redistribute it and/or modify 
; it under the terms of the GNU General Public License as published by 
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version. 
; 
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details. 
;-

pro arm_multgauss_recalc, ng, new, recalc
    recalc = 1L
    ng = new
    return
end

function arm_multgauss, x, p, ngauss=ngauss, ip=ip, QUIET=quiet, _extra=extra

    np = N_ELEMENTS(p)

    recalc = 0L
    if N_ELEMENTS(ngauss) gt 0L then begin ; conditions intentionally uncombined!  
       ng = ngauss
       if ng lt 0L then arm_multgauss_recalc, ng, np / 3, recalc
       if FIX(ng) - ng ne 0.0 then arm_multgauss_recalc, ng, ROUND(ng), recalc
       if np lt ng * 3 then arm_multgauss_recalc, ng, np / 3, recalc
    endif else arm_multgauss_recalc, ng, np / 3, recalc

    if recalc and not KEYWORD_SET(quiet) then begin
       ngtxt = STRTRIM(STRING(ng,f='(i)'),2)
       npolytxt = STRTRIM(STRING(np-ng*3,f='(i)'),2)
       PRINT, 'ARM_MULTGAUSS: NGAUSS unspecified/invalid, assuming '+ngtxt
       PRINT, '               gaussian(s) and '+npolytxt+' polynomial coefficient(s)'
    endif

    npoly = np - ng * 3

    y = x * 0.0

    if (NOT keyword_set(ip)) then for i = 0L, ng-1L do y = y + GAUSSIAN(x, p[3*i:3*i+2]) $
    else begin 
      if (size(ip,/type) eq 8) then ip_arr=ip.ip else ip_arr=ip
      for i = 0L, ng-1L do y = y + CONVOL(GAUSSIAN(x, p[3*i:3*i+2]), ip_arr, /CENTER, /EDGE_TRUNCATE)
    endelse 

    if npoly gt 0L then for i = 0D, npoly-1D do y = y + p[3*ng+i] * x ^ i

    return, y

 end