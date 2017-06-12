pro trans_dummy
; This makes it so that you don't have to compile before running
FORWARD_FUNCTION devtrans, transfunc
end

;--------------------

function devtrans,p,y=y,mag=mag,col=col,am=am,night=night,weight=weight

model = transfunc(p,mag=mag,col=col,am=am,night=night)

return,(y-model)*weight

end

;---------------------------

function transfunc,p,mag=mag,col=col,am=am,night=night,weight

nnights = max(night)

zeropar = P[0:nnights-1]
ampar = P[nnights]
colpar = P[nnights+1]

nightzero = zeropar[night-1]

model = mag + nightzero + col*colpar + am*ampar

return,model

end 

;---------------------

pro fit_transphot,input,magname,colname,stp=stp

;+
;
; FIT_TRANSPHOT
;
; This program finds the photometric transformation equations
; with iterative outlier rejection given the proper input file
; (same as SKAWDPHOT.PRO).
;
; INPUTS:
;  input    Filename of input data (i.e. M.data).  Same as for
;            SKAWDPHOT.PRO.
;  magname  Name of magnitude (i.e. M).  If this is not given
;            then 'MAG' is put in the output file.  The correct
;            magnitude name needs to be put in the transformation
;            files for MSCMAGMA.PRO.
;  colname  Name of color (i.e. M-T2).   If this is not given
;            then 'COLOR' is put in the output file.  The correct
;            color name needs to be put in the transformation
;            files for MSCMAGMA.PRO.
;  /stp     Stop at the end of the program.
;
; OUTPUTS:
;  The transformation equations are written to a file called
;  input+'.trans'.  Separate transformation equations for each
;  night are also put in the file.  Also, each night's transformation
;  equations are written to their own files called 'n#MAG.trans'
;  (i.e. n1M.trans').
;
; USAGE:
;  IDL>fit_transphot,'M.data','M','M-T'
;
; By D. Nidever   Jan. 2008
;-

; Not enough inputs
if n_elements(input) eq 0 then begin
  print,'Syntax - fit_transphot,input,magname,colname,stp=stp'
  return
endif

; Loading the data
fieldnames = ['ID','cmag','col','airmass','mag','night','number','ut','weight']
fieldtypes = [7,4,4,4,4,3,3,7,4]
arr = importascii(input,fieldnames=fieldnames,fieldtypes=fieldtypes)

arr0 = arr

; Outlier rejection loop
flag=0
count=0
par = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0]
print,'-------------------------------------'
print,' ITER  NPTS   RMS       MAD     NREJ  '
print,'====================================='
WHILE (flag ne 1) do begin

  ; Fitting with MPFIT.PRO
  func = 'devtrans'
  fa = {y:arr.mag, mag:arr.cmag, col:arr.col, am:arr.airmass, night:arr.night, weight:arr.weight}

  fpar = MPFIT(func,par,functargs=fa,status=status,perror=perror,bestnorm=chisq,$
               parinfo=parinfo,dof=dof,autoderivative=1,ftol=ftol,/quiet)

  sigpar = perror * sqrt(chisq/dof)

  ; total resid
  model = transfunc(fpar,mag=arr.cmag,col=arr.col,am=arr.airmass,night=arr.night)
  resid = arr.mag-model
  ;rms = sqrt(total(resid^2)/(npts-n_elements(fpar)-1))
  npts = n_elements(arr.mag)
  rms = sqrt(total(arr.weight*resid^2.)/(npts-1))
  mad = mad(resid)
  
  bd = where(abs(resid) gt 3.0*mad,nbd)
  if nbd gt 0 then begin
    remove,bd,arr
    par = fpar
  endif else begin
    flag=1
  endelse
  count++

  fmt = '(I3,I6,F10.5,F10.5,I5)'
  print,format=fmt,count,npts,rms,mad,nbd

end

print,'-------------------------------------'

nnights = max(arr.night)

; Print out the transformation equations
fmt2 = '(F7.4)'
undefine,lines
push,lines,''
push,lines,'FINAL TRANSFORMATION COEFFICIENTS:'
push,lines,'Final RMS      = '+string(rms,format=fmt2)
push,lines,''
for i=0,nnights-1 do begin
  push,lines,'Night '+strtrim(i+1,2)+' zpoint = '+string(fpar[i],format=fmt2)+' ('+string(sigpar[i],format=fmt2)+')'
end
push,lines,'Airmass term   = '+string(fpar[nnights],format=fmt2)+' ('+string(sigpar[nnights],format=fmt2)+')'
push,lines,'Color term     = '+string(fpar[nnights+1],format=fmt2)+' ('+string(sigpar[nnights+1],format=fmt2)+')'
printline,lines

; Print transformation equations to file

;    M    M-T  -0.9990    0.1402     -0.1345    0.0000   0.0000
;              1.094E-02  5.037E-03  2.010E-03  0.0000   0.0000

if n_elements(magname) eq 0 or n_elements(colname) eq 0 then needname=1
if n_elements(magname) eq 0 then magname='MAG'
if n_elements(colname) eq 0 then colname='COLOR'

; Final transformation equations
writeline,input+'.trans',lines

; Transformation equations for each night
undefine,lines1,all
for i=0,nnights-1 do begin
  undefine,lines1
  ;push,lines1,' '
  ;push,lines1,'Night '+strtrim(i+1,2)+' Transformation Equations'
  add='  '+magname+'  '+colname+'  '
  nadd = strlen(add)
  push,lines1,add+string(fpar[i],format=fmt2)+'   '+string(fpar[nnights],format=fmt2)+$
       '   '+string(fpar[nnights+1],format=fmt2)+'   0.0000   0.0000'
  spaces = string(byte(lonarr(nadd)+32))
  push,lines1,spaces+string(sigpar[i],format=fmt2)+'   '+string(sigpar[nnights],format=fmt2)+$
       '   '+string(sigpar[nnights+1],format=fmt2)+'   0.0000   0.0000'

  ; Writing transformation equations for this night only

  writeline,'n'+strtrim(i+1,2)+magname+'.trans',lines1

  push,all,' '
  push,all,'Night '+strtrim(i+1,2)+' Transformation Equations'
  push,all,lines1
end

writeline,input+'.trans',all,/append
;printline,lines1

print,''
print,'Transformation equations written to: ',input+'.trans'
print,''
if keyword_set(needname) then print,'NEED TO ADD MAGNITUDE AND COLOR NAME!!!'

if keyword_set(stp) then stop

end
