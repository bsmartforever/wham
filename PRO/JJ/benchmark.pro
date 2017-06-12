;+
; NAME:
;       BENCHMARK
;     
; PURPOSE:
;       To determine which of two operations or procedures is faster.
;;     
; EXPLANATION: 
;      By conducting a procedure NREPS times, we get an
;      average time for its operation.  By gathering NAVGS of these
;      averages we can remove statistical outliers (perhaps the computer
;      was busy doing something else?) and determine a mean time for
;      the procedure's operation. This routine is also useful for comparing
;      two procedures designed to accomplish the same goal.
;     
; CALLING SEQUENCE:
;       benchmark [,NREPS=nreps][,NAVGS=navgs][,/TEST]
;     
; INPUTS: 
;       None.
;     
; OPTIONAL INPUTS:
;       NREPS : number of repetitions to determine time average.
;       NAVGS : number of averages to conduct.
;
; OUTPUTS: 
;       None.
;
; OPTIONAL OUTPUTS:
;       None.
;
; KEYWORDS:
;       TEST : Do nothing over and over; a check of system performance.
;
; COMMON BLOCKS:
;       None.
;
; PROCEDURES CALLED:
;       CHAUVENET
;       LEGEND
;       GETCOLOR
;
; EXAMPLE: 
;       Find which is faster: fltarr(100) or replicate(0,100)
;       Copy "fltarr(100)" into testroutine1 module below.
;       Copy "replicate(0,100)" into testroutine2 module below.
;
;       IDL> benchmark, nreps=1d3, navgs=1d3
;
; REVISION HISTORY:
;       Written by T. Robishaw, Berkeley 15 Mar 2001
;-

;==================================
pro testroutine1
restore, '~/test.dat'
xip = fillarr(0.25, -15, 15)
psf = gpfunc(xip, info.par, info=info)
end

pro testroutine2
restore, '~/test.dat'
xip = fillarr(0.25, -15, 15)
psf = test_gpfunc(xip, info.par, info=info)
end

pro testroutine1a
xpsf = fillarr(0.25, -15, 15)
cen = 0
wid = 2
amp = indgen(15)
g = 0
for j = 0, 50 do begin
    for i = 0, 14 do begin
        g += amp[i] * exp( -0.5 * (xpsf-cen)^2 / wid^2)
    endfor
endfor

end ; testroutine1
;==================================
pro testroutine2a
xpsf = fillarr(0.25, -15, 15)
ngau = 15
gaussarr = fltarr(121, ngau)
for i = 0, ngau-1 do begin
    cen = 0
    wid = 2
    z = -0.5 * (xpsf - cen)^2 / wid^2
    gaussarr[0, i] = exp(z)
endfor

amp = indgen(15)
for i = 0, 50 do begin
    g = total(gaussarr * fan(amp, 121, /trans), 2)
endfor
end; testroutine2
;=================================

pro benchmark, NREPS=nreps, NAVGS=navgs

; SET THE DEFAULTS...
if not keyword_set(NAVGS) then navgs=3d2
if not keyword_set(NREPS) then nreps=1d2

; MAKE SURE WE LOOK AT AT LEAST TEN AVERAGES...
navgs = navgs>1d1

;=============
; PREPARE VARIABLES HERE...

;=============
; BENCHMARK...

delt1 = dblarr(navgs)
delt2 = dblarr(navgs)
for j = 0, navgs-1 do begin

    tstart = systime(1)
    for i = 1d0, nreps do begin
        testroutine1
    endfor
    tstop = systime(1)

    delt1[j] = (tstop-tstart)

    ; WANT DIFFERENCE IN TIME TO BE MUCH LARGER THAN
    ; MACHINE ACCURACY OF SYSTEM TIME...
    if (median(delt1[0:j]) lt 0.05) then begin
        benchmark, NAVGS=navgs, NREPS=fix(nreps*0.06/delt1[j])
        return
    endif
    
    tstart = systime(1)
    for i = 1d0, nreps do begin
        testroutine2
    endfor
    tstop = systime(1)

    delt2[j] = (tstop-tstart)

    ; WANT DIFFERENCE IN TIME TO BE MUCH LARGER THAN
    ; MACHINE ACCURACY OF SYSTEM TIME...
    if (median(delt2[0:j]) lt 0.05) then begin
        benchmark, NAVGS=navgs, NREPS=fix(nreps*0.06/delt2[j])
        return
    endif

    ; CHECK HOW FAR WE'VE GOTTEN...
    case j of
        fix(navgs/4.)    : print, '25%' 
        fix(navgs/2.)    : print, '50%'
        fix(3.*navgs/4.) : print, '75%'
        else             :
    endcase
endfor

;==============
; GET THE AVERAGE TIMES...
avg1 = delt1/nreps
avg2 = delt2/nreps

frac = avg2/avg1

;==============
; REMOVE OUTLIERS USING CHAUVENET CRITERION...
goodindx = chauvenet(frac, Ngood, /ITERATE)

frac = frac[goodindx]
avg1 = avg1[goodindx]
avg2 = avg2[goodindx]

;==============
; GET THE MEAN...
mnavg1 = mean(avg1)
mnavg2 = mean(avg2)
mnfrac = mean(frac)

;============
; TELL US ABOUT THE RESULTS...
print
print, 'Average1  = ',mnavg1,' s'
print, 'Average2  = ',mnavg2,' s'
print
case (mnfrac gt 1) of
    0 : begin
          print, '<Avg2/Avg1> = ', mnfrac
          print, 'Avg2 faster than Avg1 by ',1d2*(1d0/mnfrac-1d0),'%'
        end
    1 : begin
          print, '<Avg1/Avg2> = ', 1./mnfrac
          print, 'Avg1 faster than Avg2 by ',1d2*(mnfrac-1d0),'%'
        end
endcase
print

;============
; PLOT THE DISTRIBUTION...
!p.multi = [0,1,2]
avgs   = indgen(Ngood)
maxy   = max([avg1,avg2],min=miny)
repstr = strtrim(ulong64(nreps),2)
plot, avgs, avg1, /xs, /ys, yr=[0.9*miny,1.1*maxy], /nodata, $
  ytitle='Average Time for '+repstr+' Repetitions [seconds]', $
  xtitle='Group of '+repstr+' Repetitions', $
  xmargin=[14,3]
symsize = (alog10(navgs)>1)^(-1L)
oplot, avgs, avg2, ps=4, co=getcolor('green'), symsize=symsize
oplot, avgs, avg1, ps=6, co=getcolor('yellow'), symsize=symsize
legend, ['Avg1','Avg2'], psym=[6,4], $
  colors=[getcolor('yellow'),getcolor('green')]

fracdiff = 1d2*(frac-1d0)
plot, avgs, fracdiff, /xs, /ys, /nodata, $
  xtitle='Group of '+repstr+' Repetitions', $
  ytitle='Fractional difference, <t!D2!N/t!D1!N>-1 [%]', $
  yr=max(abs(fracdiff))*1.5*[-1,1]
oplot, !x.crange, [0,0], lines=1, co=!red
oplot, !x.crange, 1d2*(mnfrac*[1,1]-1d0), co=!orange
oplot, avgs, 1d2*(frac-1d0), ps=4, symsize=symsize, co=!cyan
!p.multi=0

end; benchmark

