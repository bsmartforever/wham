; $Id: func_cont__define.pro,v 1.10 2003/08/19 19:47:51 dimeo Exp $
; FUNC_CONT__DEFINE.PRO
;
; Written by R.M.Dimeo (08/20/02)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::cleanup
self->IDL_CONTAINER::cleanup
ptr_free,self.pcor_ptr
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::init
; Initialize superclass

self.chisq = (-1L)
if not self->IDL_CONTAINER::Init() then return,0
self.pcor_ptr = ptr_new(/allocate_heap)
return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getstep
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,step = step
  if i eq 0 then stepout = step else stepout = [stepout,step]
endfor
return,stepout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::drawComponents,_Extra = extra
nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,xvalues = xvalues, yvalues = yvalues,name = name
  if (name ne 'pan_delta') and (n_elements(yvalues) gt 1) then $
  	oplot,xvalues,yvalues,_Extra = extra
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setstep,newstep
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,step = oldStep
  nstep = n_elements(oldStep)
  oall[i]->setproperty,step = newStep[count:count+nstep-1]
  count = count + nstep
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getExpr
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,expr = expr
  if i eq 0 then exprout = expr else exprout = [exprout,expr]
endfor
return,exprout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getfixed
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,fixed = fixed
  if i eq 0 then fixedout = fixed else fixedout = [fixedout,fixed]
endfor
return,fixedout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setfixed,newfixed
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,fixed = oldFixed
  nfixed = n_elements(oldFixed)
  oall[i]->setproperty,fixed = newfixed[count:count+nfixed-1]
  count = count + nfixed
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::gettied
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,tied = tied
  if i eq 0 then tieddout = tied else tieddout = [tieddout,tied]
endfor
return,tieddout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::settied,newtied
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,tied = oldtied
  ntied = n_elements(oldtied)
  oall[i]->setproperty,tied = newtied[count:count+ntied-1]
  count = count + ntied
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getfixvalues
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,fixvalues = fixvalues
  if i eq 0 then fixout = fixvalues else fixout = [fixout,fixvalues]
endfor
return,fixout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setfixvalues,newfixvalues
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,fixvalues = oldFixvalues
  nfixed = n_elements(oldFixvalues)
  oall[i]->setproperty,$
           fixvalues = newfixvalues[count:count+nfixed-1],/calculate
  count = count + nfixed
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getchisq
return,self.chisq
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setchisq,chisq
self.chisq = chisq
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getlowvalues
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,lovalues = lovalues
  if i eq 0 then lo_out = lovalues else lo_out = [lo_out,lovalues]
endfor
return,lo_out
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setlowvalues,newlovalues
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,lovalues = oldLovalues
  nlovalues = n_elements(oldLovalues)
  oall[i]->setproperty,lovalues = newlovalues[count:count+nlovalues-1]
  count = count + nlovalues
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::gethighvalues
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,hivalues = hivalues
  if i eq 0 then hi_out = hivalues else hi_out = [hi_out,hivalues]
endfor
return,hi_out
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::sethighvalues,newhivalues
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,hivalues = oldhivalues
  nhivalues = n_elements(oldhivalues)
  oall[i]->setproperty,hivalues = newhivalues[count:count+nhivalues-1]
  count = count + nhivalues
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getlow
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,low = low
  if i eq 0 then low_out = low else low_out = [low_out,low]
endfor
return,low_out
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setlow,newlow
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,low = oldlow
  nlow = n_elements(oldlow)
  oall[i]->setproperty,low = newlow[count:count+nlow-1]
  count = count + nlow
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::gethigh
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,high = high
  if i eq 0 then high_out = high else high_out = [high_out,high]
endfor
return,high_out
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::sethigh,newhigh
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,high = oldHigh
  nhigh = n_elements(oldHigh)
  oall[i]->setproperty,high = newhigh[count:count+nhigh-1]
  count = count + nhigh
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getparms
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)

for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parms = parms
  if i eq 0 then parmout = parms else parmout = [parmout,parms]
endfor
return,parmout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getParmError
nfuncs = self->count()
if nfuncs eq 0 then return,-1L
oall = self->get(/all)

for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parmError = parmError
  if i eq 0 then errout = parmError else errout = [errout,parmError]
endfor
return,errout
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setparmError,newError
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parmError = oldError
  nparms = n_elements(oldError)
  oall[i]->setproperty,parmError = newError[count:count+nparms-1],/calculate
  count = count + nparms
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setparms,newparms
if n_params() eq 0 then return
nfuncs = self->count()
if nfuncs eq 0 then return

oall = self->get(/all)
count = 0
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parms = oldParms
  nparms = n_elements(oldParms)
  oall[i]->setproperty,parms = newparms[count:count+nparms-1],/calculate
  count = count + nparms
endfor
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::draw,noerase = noerase,overplot = overplot,resPtr = resPtr
nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
oall[0]->func::getproperty,xvalues = xvalues
self->evaluate,xvalues,yout = yvalues
if n_elements(noerase) eq 0 then noerase = 0
if n_elements(overplot) eq 0 then overplot = 0
if overplot ne 0 then begin
  oplot,xvalues,yvalues,linestyle = 2,color=5
endif else begin
  plot,xvalues,yvalues,linestyle = 2,noerase = noerase,color=5
endelse
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setxvalues,x,resPtr = resPtr,yout = yout, $
               rlimit = rlimit, $
               _Extra = extra


nfuncs = self->count()
yout = 0.0
delta = 0	; no delta function present
deltaloc = -1L	; index/location for delta function
if nfuncs ne 0 then begin
  oall = self->get(/all)
  for i = 0,nfuncs-1 do begin
    oall[i]->setproperty,xvalues = x,/calculate
  endfor
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::evaluate,x,resPtr = resPtr,yout = yout, $
               rlimit = rlimit, $
               _Extra = extra
nfuncs = self->count()
yout = 0.0
delta = 0	; no delta function present
qens = 0	; no qens lineshape present
macro = 0	; no macro lineshape present
qensloc = -1
if nfuncs ne 0 then begin
  oall = self->get(/all)
  for i = 0,nfuncs-1 do begin
    oall[i]->getproperty,xvalues = xvalues,yvalues = yvalues,name = name
	yout = i eq 0 ? yvalues : yout+yvalues
  endfor
  return
endif else begin
  return
endelse
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getcurvename,index
; This function returns the curve name given an index into the
; long parameter array.
nfuncs = self->count()
if nfuncs eq 0 then return,''

names = strarr(nfuncs)
oall = self->get(/all)
nparms = intarr(nfuncs)
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,name = name,parms = parms
  if i eq 0 then begin
    nparms = n_elements(parms)
    names = name
  endif else begin
     nparms = [nparms,n_elements(parms)]
     names = [names,name]
  endelse
endfor

totparms = total(nparms)
start = intarr(nfuncs)
finish = intarr(nfuncs)

for i = 0,nfuncs-1 do begin
  if i eq 0 then start[i] = 0 else start[i] = finish[i-1]+1
  finish[i] = start[i] + nparms[i] - 1
  if (index ge start[i]) and (index le finish[i]) then curveName = names[i]
endfor

return,curveName
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getnames
  nfuncs = self->count()
  if nfuncs eq 0 then begin
    names = ""
  endif else begin
    oall = self->get(/all)
    names = strarr(nfuncs)
    for i = 0,nfuncs-1 do begin
      oall[i]->getproperty,name = name
      names[i] = name
    endfor
  endelse
return,names
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getparmnames

  nfuncs = self->count()
  if nfuncs eq 0 then begin
    names = ""
    return,-1L
  endif else begin
    oall = self->get(/all)
    names = strarr(nfuncs)
    for i = 0,nfuncs-1 do begin
      oall[i]->getproperty,parmnames = pn
      if i eq 0 then parmnames = pn else parmnames = [parmnames,pn]
    endfor
  endelse
  return,parmnames
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::getCanDraw

  nfuncs = self->count()
  if nfuncs eq 0 then begin
    return,-1L
  endif else begin
    oall = self->get(/all)
    names = strarr(nfuncs)
    for i = 0,nfuncs-1 do begin
      oall[i]->getproperty,canDraw = thisDraw
      if i eq 0 then canDraw = thisDraw else canDraw = [canDraw,thisDraw]
    endfor
  endelse
  return,canDraw
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::displayparms,output = output
nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
parms = self->getparms()
sigma = self->getparmerror()
count = 0
;MSW: increased accuracy of output values - now 4 d.p.
format = '(f13.4)'
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parmnames = pn,name = name,expr = expr
  name = strmid(name,4)
  nparms = n_elements(pn)
  thisLine = '-----------------'
  if i eq 0 then output = thisLine else output = [output,thisLine]
  output = [output,'Curve '+strtrim(string(i+1),2)+': '+STRUPCASE(name)]
  if expr ne '' then output = [output,expr]
  for j = 0,nparms-1 do begin
    if n_elements(sigma) eq n_elements(parms) then begin
      strout = '#'+strtrim(string(count),2)+': '+ $
                strtrim(string(pn[j]),2)+': '+ $
                strtrim(string(parms[count],format = format),2)+ $
               ' +/- '+strtrim(string(sigma[count],format = format),2)
      output = [output,strout]
    endif else begin
      strout = strtrim('#'+string(count)+':',2)+string(pn[j])+':'+string(parms[count])
      output = [output,strout]
    endelse
    count = count+1
  endfor
endfor
chiFormat = '(f12.5)'
strout = 'Chi-squared: '+strtrim(string(self.chisq,format = chiFormat),2)
output = [output,'']
output = [output,strout]
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MSW
pro func_cont::displayparms_katrina,output=output,trans=trans,spax_id=spax_id
; procedure to format the results into a one-line output array

nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
parms = self->getparms()
sigma = self->getparmerror()
parm_counter = 0   ; <- increased for each param read, not reset for each func,
		   ;    used to reference array indices of 'parms' and 'sigma'
comp_counter = 1   ; <- similar to func number, but still begins at 1 even if 
		   ;    'background' has already been looped over, used for 
		   ;    component part of 'line_id' string
format = '(f12.4)'
output = '------'   ; initialise the output array
for i = 0,nfuncs-1 do begin    ; loop through functions (e.g. gaussian 1, 2)
  oall[i]->getproperty,parmnames = pn,name = name,expr = expr
  name = strmid(name,4)    ; strip the first 4 chars off the 'name' string
  nparms = n_elements(pn)
  if name eq 'background' then begin    ; if the function is 'background'
    parm_counter=parm_counter+2         ; then ignore everything else, set
    flag = 1				; the output flag to 1 and
  continue 				; 'continue' to next iteration
  endif
  flag=0
  
  for j = 0,nparms-1 do begin    ; loop through parameters for each function
    if n_elements(sigma) eq n_elements(parms) then begin
     ; concatenate result + error for each parameter
     if j eq 0 then begin
        strout = strtrim(string(parms[parm_counter],format = format),2)+ ' '+ $
                 strtrim(string(sigma[parm_counter],format = format),2)
      endif else begin
        strout = strout+' '+strtrim(string(parms[parm_counter],format = format),2)+ $
        ' '+ strtrim(string(sigma[parm_counter],format = format),2)
      endelse
    endif else begin
      message,'Error: no uncertainites calculated for param '+strtrim(string(j),2)
    endelse
    parm_counter = parm_counter+1
  endfor
 
 ; concatenate results line with spax_id and line_id (made of trans+comp)
  chiFormat = '(f15.5)'
  strout = 'chi-sq= '+ strtrim(string(self.chisq,format = chiFormat),2)+' '+strout+$
           ' '+strtrim((spax_id+1),2)+' '+strtrim(trans,2)+ $
           'c'+strtrim(string(comp_counter),2)
;WITHOUT CHI-SQ VALUE ***MARKER*** MSW
;  strout = strout+$
;           ' '+strtrim((spax_id+1),2)+' '+strtrim(trans,2)+ $
;           'c'+strtrim(string(comp_counter),2)


  ; Because you have to initialise 'output' to something, you can't concatinate it
  ; back onto itself. This can only happen once it's filled with the 1st comp fits.
  ; But you can't just use 'if i eq 0' because if the 1st func is a background, then
  ; i=0 is taken up by that.
  if (flag eq 1) or (i eq 1) then begin
	;print,'1st if'
	output = strout
  endif else begin
	;print,'2nd if'
	output = [output,strout]
  endelse
  comp_counter = comp_counter+1
endfor
return
end 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;MSW
pro func_cont::displayparms_katrina_cont,output=output,trans=trans,spax_id=spax_id
; procedure to format the results into a one-line output array

nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
parms = self->getparms()
sigma = self->getparmerror()
parm_counter = 0   ; <- increased for each param read, not reset for each func,
		   ;    used to reference array indices of 'parms' and 'sigma'
comp_counter = 1   ; <- similar to func number, but still begins at 1 even if 
		   ;    'background' has already been looped over, used for 
		   ;    component part of 'line_id' string
format = '(f12.3)'
output = '------'   ; initialise the output array
for i = 0,nfuncs-1 do begin    ; loop through functions (e.g. gaussian 1, 2)
  oall[i]->getproperty,parmnames = pn,name = name,expr = expr
  name = strmid(name,4)    ; strip the first 4 chars off the 'name' string
  nparms = n_elements(pn)
  if name ne 'background' then begin    ; if the function is 'background'
    parm_counter=parm_counter+2         ; then ignore everything else, set
    flag = 1				; the output flag to 1 and
  continue 				; 'continue' to next iteration
  endif
  flag=0
  
  for j = 0,nparms-1 do begin    ; loop through parameters for each function
    if n_elements(sigma) eq n_elements(parms) then begin
     ; concatenate result + error for each parameter
     if j eq 0 then begin
        strout = strtrim(string(parms[parm_counter],format = format),2)+ ' '+ $
                 strtrim(string(sigma[parm_counter],format = format),2)
      endif else begin
        strout = strout+' '+strtrim(string(parms[parm_counter],format = format),2)+ $
        ' '+ strtrim(string(sigma[parm_counter],format = format),2)
      endelse
    endif else begin
      message,'Error: no uncertainites calculated for param '+strtrim(string(j),2)
    endelse
    parm_counter = parm_counter+1
  endfor
 
 ; concatenate results line with spax_id and line_id (made of trans+comp)
  chiFormat = '(f10.3)'
  ;strout = 'chi-sq='+ strtrim(string(self.chisq,format = chiFormat),2)+' '+strout+$
  ;         ' '+'0.000 0.000 '+strtrim(spax_id,2)+' '+strtrim(trans,2)+ $
  ;         'c'+strtrim(string(comp_counter),2)
  ;WITHOUT CHI-SQ VALUE ***MARKER***
  strout = strout+$
           ' '+'0.000 0.000 '+strtrim((spax_id),2)+' '+strtrim(trans,2)+ $
           'c'+strtrim(string(comp_counter),2)
 ; Because you have to initialise 'output' to something, you can't concatinate it
  ; back onto itself. This can only happen once it's filled with the 1st comp fits.
  ; But you can't just use 'if i eq 0' because if the 1st func is a background, then
  ; i=0 is taken up by that.
  if (flag eq 1 or i eq 0) then begin
	;print,'1st if'
	output = strout
  endif else begin
	;print,'2nd if'
	output = [output,strout]
  endelse
  comp_counter = comp_counter+1
endfor
return
end 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::displayParmsAndConstraints,output = output
; New method RMD 07/01/03
nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
parms = self->getparms()
sigma = self->getparmerror()
; Here we get all of the parameter constraint information
fixed_parms = self->getfixed()
tied_parms = self->gettied()
low_check = self->getlow()
high_check = self->gethigh()
low_values = self->getlowvalues()
high_values = self->gethighvalues()

count = 0
format = '(e12.3)'
comma = ','
for i = 0,nfuncs-1 do begin
  oall[i]->getproperty,parmnames = pn,name = name,expr = expr
  name = strmid(name,4)
  nparms = n_elements(pn)
  thisLine = '-----------------'
  if i eq 0 then output = thisLine else output = [output,thisLine]
  output = [output,'Curve '+strtrim(string(i+1),2)+': '+STRUPCASE(name)]
  if expr ne '' then output = [output,expr]
  for j = 0,nparms-1 do begin
    if n_elements(sigma) eq n_elements(parms) then begin
      strout = '#'+strtrim(string(count),2)+': '+ $
                strtrim(string(pn[j]),2)+': '+ $
                strtrim(string(parms[count],format = format),2)+ $
               ' +/- '+strtrim(string(sigma[count],format = format),2) + $
; Here is where we have the departure from the original method called DISPLAYPARMS
; We append the constraint information to the end of strout as comma-delimited text
; RMD 07/01/03
				comma+strtrim(string(fixed_parms[count]),2)+					$
				comma+strtrim(string(low_check[count]),2)+						$
				comma+strtrim(string(low_values[count],format = format),2)+		$
				comma+strtrim(string(high_check[count]),2)+						$
				comma+strtrim(string(high_values[count],format = format),2)+	$
				comma+strtrim(string(tied_parms[count]),2)

      output = [output,strout]
    endif else begin
      strout = strtrim('#'+string(count)+':',2)+string(pn[j])+':'+string(parms[count])
      output = [output,strout]
    endelse
    count = count+1
  endfor
endfor
chiFormat = '(f10.3)'
strout = 'Chi-squared: '+strtrim(string(self.chisq,format = chiFormat),2)
output = [output,'']
output = [output,strout]
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::get_pcor,pcor
if n_elements(*self.pcor_ptr) ne 0 then pcor = *self.pcor_ptr else $
	pcor = 0.0
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::set_pcor,pcor
*self.pcor_ptr = pcor
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::createParInfo
nfuncs = self->count()
nparms = n_elements(self->getparms())

parinfo = replicate({fixed:0,limited:[0,0],limits:[0.D,0.D],step:0D,tied:''},nparms)
parinfo.step = self->getstep()
parinfo.fixed = self->getfixed()
parinfo.limited[0] = self->getlow()
parinfo.limited[1] = self->gethigh()
parinfo.limits[0] = self->getlowvalues()
parinfo.limits[1] = self->gethighvalues()

; Now get the tied parameter information out
tied = self->gettied()
if n_elements(tied) gt 1 then begin
 ok = where(tied eq 1,count_tied)
 for i = 1,count_tied-1 do begin
   rhs = 'p['+strtrim(string(ok[0]),2)+']'
   parinfo[ok[i]].tied = rhs
 endfor
endif

return,parinfo
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont__define

define = {FUNC_CONT,$
          chisq:0.0, $
          pcor_ptr:ptr_new(),	$
          INHERITS FUNC,$
          INHERITS IDL_CONTAINER}
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
