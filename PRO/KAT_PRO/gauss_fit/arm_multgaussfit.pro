;+
; NAME: arm_multgaussfit
;       
; CATEGORY: math
;
; PURPOSE: simultaneously fit multiple gaussians (and a polynomial)
;
; CALLING SEQUENCE: result = ARM_MULTGAUSSFIT(x, y, p, [ngauss=, $ 
;                              err=, mask=, chisq=, /quiet, _extra=) 
;
; INPUTS:
;   x - array of independent values
;   y - data points corresponding to X
;   p - initial parameter value guesses (superceded by PARINFO.VALUE,
;       see EXTRA) - an array of 3xNGAUSS gaussian parameters
;       [optionally] followed by polynomial coefficients, such that:  
;         P[3xn+0] - nth gaussian amplitude
;         P[3xn+1] - nth gaussian center
;         P[3xn+2] - nth gaussian sigma
;         P[3xNGAUSS+m] - mth order polynomial coefficient
;       
; OPTIONAL INPUTS:
;   ngauss - number of gaussian curves; unnecessary if P contains no 
;            non-gaussian parameters (polynomial coefficients)
;   err    - 1-sigma uncertainties in Y (fit will be weighted by
;            1/ERR^2, if ERR is not supplied then weights=1)
;   mask   - array of mask values corresponding to X (zeroes indicate
;            values of X to ignore in the fitting process)
;   _extra - additional parameters passed to MPFITFUN(); in particular,
;            see the MPFITFUN() documentation for PARINFO (and the
;            example provided therein), which allows great flexibility
;            in constraining the fitting algorithm 
;
; KEYWORDS:
;   quiet - suppress text messages (does not apply to fatal errors)
;
; OUTPUTS:
;   returns best-fit parameter values
; 
; OPTIONAL OUTPUTS:
;   chisq  - Chi squared statistic (square root of BESTNORM parameter
;            returned by MPFITFUN divided by degrees of freedom)
;   
; EXAMPLE:
;
; PROCEDURES USED: MPFITFUN(), ARM_MULTGAUSS()
;
; COMMENTS:
; 
; BUG REPORT: Please report any bugs to Andrew R. Marble.
;
; MODIFICATION HISTORY:
;    written by A.R.Marble, Steward Obs., 2005 Aug 18
;    -Added parinfo to mpfitfun so that Gaussian parameters (width,center,height)
;     can be flagged as free, constrained, or fixed. Dr. Kat Barger 02/2014
;    -Changed chisq to bestnorm/dof from SQRT(bestnorm / dof). Dr. Kat Barger 02/2014
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

pro arm_multgaussfit_recalc, ng, new, recalc
    recalc = 1L
    ng = new
    return
end

function arm_multgaussfit, x, y, p, ngauss=ngauss, err=err, mask=mask, $
            chisq=chisq, dof=dof, perror=perror, ip=ip, yfit=yfit, quiet=quiet, _extra=extra

; defaults and error-checking

    if N_ELEMENTS(mask) eq 0L then mask = x * 0 + 1

    if N_ELEMENTS(err) eq 0L then weights = x * 0 + 1 else weights = 1d0 / (err^2d)
    
    if N_ELEMENTS(extra) gt 0L then begin
       if TOTAL(STRMATCH(TAG_NAMES(extra), 'PARINFO')) eq 1 then begin
          if TOTAL(STRMATCH(TAG_NAMES(extra.parinfo), 'VALUE')) eq 1 then begin
             p = extra.parinfo.value
          endif
       endif
    endif
    
    n = N_ELEMENTS(x)
    if N_ELEMENTS(y) ne n or N_ELEMENTS(weights) ne n or N_ELEMENTS(mask) ne n then begin
       PRINT, 'ARM_MULTGAUSSFIT: X, Y, MASK or ERR (if passed) has incompatible dimensions!'
       return, 0
    endif
    
    np = N_ELEMENTS(p)
    if np eq 0L then begin
       PRINT, 'ARM_MULTGAUSSFIT: no starting parameter values given!'
       return, 0
    endif

    recalc = 0L
    if N_ELEMENTS(ngauss) gt 0L then begin ; conditions intentionally uncombined!  
       ng = ngauss
       if ng lt 0L then ARM_MULTGAUSSFIT_RECALC, ng, np / 3, recalc
       if FIX(ng) - ng ne 0.0 then ARM_MULTGAUSSFIT_RECALC, ng, ROUND(ng), recalc
       if np lt ng * 3 then ARM_MULTGAUSSFIT_RECALC, ng, np / 3, recalc
    endif else ARM_MULTGAUSSFIT_RECALC, ng, np / 3, recalc

    if recalc and not KEYWORD_SET(quiet) then begin
       ngtxt = STRTRIM(STRING(ng,f='(i)'),2)
       npolytxt = STRTRIM(STRING(np-ng*3,f='(i)'),2)
       PRINT, 'ARM_MULTGAUSSFIT: NGAUSS unspecified/invalid, assuming '+ngtxt
       PRINT, '                  gaussian(s) and '+npolytxt+' polynomial coefficient(s)'
    endif

    npoly = np - ng * 3

; ignore masked values

    xall = x
    yall = y
    masked = WHERE(mask eq 0, nmasked)
    if nmasked gt 0 then REMOVE, masked, x, y, weights

; package extra information for passing to ARM_MULTGAUSS

    if (NOT keyword_set(ip)) then functargs = {ngauss: ng, quiet: KEYWORD_SET(quiet)} else $
    functargs = {ngauss: ng, quiet: KEYWORD_SET(quiet), ip:ip}

    error = weights ;* 0.0 ; dummy error values (WEIGHTS overrides)

    pnew = MPFITFUN('arm_multgauss', x, y, error, p, weights=weights, $
                    bestnorm=bestnorm, functargs=functargs, quiet=quiet, $
                    parinfo=parinfo,perror=perror,dof=dof,yfit=yfit,$
                    _extra=extra)

    dof = TOTAL(mask) - ng*3 - npoly ; degrees of freedom
    chisq = bestnorm/dof ; SQRT(bestnorm / dof) ;This is not chisq

    return, pnew

 end 