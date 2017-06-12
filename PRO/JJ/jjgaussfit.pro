;+
; NAME:
;          JJGAUSSFIT
;
;
; PURPOSE:
;         Fits a gaussian to a 1D distribution of points, This is a wrapper
;         for Craig Markwardt's infinitely useful fitting package MPFIT.
;
;         Unlike IDL's GAUSSFIT, this function optionally accounts
;         for measurement errors 
;
;         Fits the function:
;
;         f(x) = a0 * exp(-z^2/2) + a3
;
;         where
;
;         z = (x - a1)/a2
;
; CATEGORY:
;
;         Getting the job done (TCB).
;
; CALLING SEQUENCE:
;
;         fit = jjgaussfit(x, y [, param])
;
; INPUTS:
;         x:  independant variable
;         y:  independant variable
;
; KEYWORDS:
;
;         FIXAMP=, FIXCEN=,  FIXWID=, FIXBG=: Scalar value at which the 
;         centroid, amplitude, width or background (offset),
;         respectively, should be held fixed during the fit.
;
;         LIMITAMP=, LIMITCEN=, LIMITWID=, LIMITBG=: 2-element array
;         specifying lower and upper limits of the centroid, amplitude,
;         width or background (offset), respectively. 
;
;         MOVIE: Play a movie during the fit procedure. 
;
;         GUESS: 4-element array containing the parameter guesses. If
;         guesses are not set manually, then parameter guesses are
;         determined automatically. 
;    
;         GUESS[0] = Amplitude
;         GUESS[1] = Centroid 
;         GUESS[2] = Sigma width
;         GUESS[3] = Background offset
;
;         YERR: 1 sigma errors associated with each Y
;
; OUTPUTS:
;
;         FIT: The best-fit Gaussian evaluated at each X
;         PARAM: 4 element vector containing the fit parameters
;                param = [a0, a1, a2, a3]
;
; EXAMPLE:
;
; x = findgen(121) * 0.25 - 15
; y = 1d2 * exp(-x^2) + 2
; yn = 1d2 - (y + randomn(seed, 121)*sqrt(y))
; p0 = [-110, 0.05, 1.1, 90]
; g = jjgaussfit(x, yn, a, yerr=sqrt(y), guess=p0, /movie)
;         
; MODIFICATION HISTORY:
; 12 Dec 2003 Written by JohnJohn
; 05 Dec 2004 JJ - fixed bug in FWHM guess
; 22 Feb 2005 JJ - fixed to work with negative amplitudes. Stole
;                  parameter guesses from GAUSSFIT.PRO
; 06 Mar 2005 JJ - added MOVIE keyword.
; 15 Mar 2005 JJ - modified parameter guess logic to w ork  with
;                  continuum-normalized absorption lines 
; 18 Mar 2005 JJ - added GUESS keyword
;-

function jjgaussfit_func, p, x=x, y=y, err=err, fit=fit, movie=movie
z = (x - p[1])/p[2]
fit = p[0]*exp(-z^2/2d) + p[3]
if n_elements(err) eq 0 then err = 1.
if keyword_set(movie) then begin
    yr = minmax(y)
    if abs(max(y)) gt abs(min(y)) then yr = yr * [0.95, 1.05] else $
      yr = yr * [1.05, 0.95]
    plot, x, y, yr=yr, /ys, ps=4
    oplot, x, fit
    wait,.5
endif
return, (y - fit)/err
end

pro jjgaussfit_plot, fcn, par, iter, fnorm, functargs=fa $
                  , parinfo=parifno, quiet=quiet, dof=dof
wset,30
resid = jjgaussfit_func(par, x=fa.x, y=fa.y, /movie)

mpfit_defiter, fcn, par, iter, fnorm, FUNCTARGS=fa, $
                   quiet=quiet, parinfo=parinfo, dof=dof

wset,0
device,copy = [0,0,!d.x_size,!d.y_size,0,0,30]
end


function jjgaussfit, x, y, p $
                     , yerr=yerr $
                     , fixamp=fixamp , limitamp=limitamp $
                     , fixcen=fixcen , limitcen=limitcen $
                     , fixsig=fixsig, limitsig=limitsig $
                     , fixbg=fixbg, limitbg=limitbg $
                     , loud=loud, movie=movie, guess=guess
ON_ERROR,2
if keyword_set(yerr) then begin
    fa = {x:double(x), y:double(y), err:double(yerr)}
endif else begin
    fa = {x:double(x), y:double(y)}
endelse
;;;Set up parameter guesses, stolen from GAUSSFIT.PRO and modified to
;;;work with the case of a continuum normalized absorption line
if 1-keyword_set(guess) and n_elements(guess) lt 4 then begin
    n = n_elements(y)
    ymed = median(y)
    ytest = y - ymed
    ymax = max(ytest, imax)
    ymin = min(ytest, imin)
    if abs(ymax) gt abs(ymin) then begin ;emiss or absorp?
        i0 = imax 
        amp = ymax - ymin
    endif else begin
        i0 = imin
        amp = ymin - ymax
    endelse
    bg = ymed

    i0 = i0 > 1 < (n-2)         ;never take edges
    del = amp/exp(1.)           ;1/e value
    i=0
    while ((i0+i+1) lt n) and $ ;guess at 1/2 width.
      ((i0-i) gt 0) and $
      (abs(ytest[i0+i]) gt abs(del)) and $
      (abs(ytest[i0-i]) gt abs(del)) do i=i+1

    p0 = [amp, x[i0], abs(x[i0]-x[i0+i]), bg]
endif else p0 = guess

npar = n_elements(p0)
parinfo = replicate( { fixed: 0b, $
                       limited: [0b,0b], $
                       limits: dblarr(2) $
                     }, npar)

if n_elements(fixamp) gt 0 then begin
    parinfo[0].fixed = 1b
    p0[0] = fixamp
endif

if n_elements(fixcen) gt 0 then begin
    parinfo[1].fixed = 1b
    p0[1] = fixcen
endif

if keyword_set(fixsig) then begin
    parinfo[2].fixed = 1b
    p0[2] = fixsig
endif

if n_elements(fixbg) gt 0 then begin
    parinfo[3].fixed = 1b
    p0[3] = fixbg
endif

if keyword_set(limitamp) then begin
    parinfo[0].limited = 1b
    parinfo[0].limits = limitamp
    p0[0] = mean(limitamp)
endif

if keyword_set(limitcen) then begin
    parinfo[1].limited = 1b
    parinfo[1].limits = limitcen
    p0[1] = mean(limitcen)
endif

if keyword_set(limitsig) then begin
    parinfo[2].limited = 1b
    parinfo[2].limits = limitsig
    p0[2] = mean(limitsig)
endif

if keyword_set(limitbg) then begin
    parinfo[3].limited = 1b
    parinfo[3].limits = limitbg
    p0[3] = mean(limitbg)
endif

if keyword_set(movie) then begin
    window, 0
    window, 30, /pixmap
    ip = 'jjgaussfit_plot'
endif
p = mpfit('jjgaussfit_func', p0, parinfo=parinfo, funct=fa, maxiter=20 $
          , quiet=1-keyword_set(loud), iterproc=ip)

z = (x - p[1])/p[2]
fit = p[0]*exp(-z^2/2d) + p[3]
return, fit
end
