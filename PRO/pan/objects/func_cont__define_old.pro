; $Id: func_cont__define.pro,v 1.2 2002/09/20 19:16:50 dimeo Exp $
; FUNC_CONT__DEFINE.PRO
;
; Written by R.M.Dimeo (08/20/02)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::cleanup
self->IDL_CONTAINER::cleanup
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func_cont::init
; Initialize superclass

self.chisq = (-1L)
if not self->IDL_CONTAINER::Init() then return,0
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
pro func_cont::draw,noerase = noerase,overplot = overplot
nfuncs = self->count()
if nfuncs eq 0 then return
oall = self->get(/all)
oall[0]->func::getproperty,xvalues = xvalues
self->evaluate,xvalues,yout = yvalues
if n_elements(noerase) eq 0 then noerase = 0
if n_elements(overplot) eq 0 then overplot = 0
if overplot ne 0 then begin
  oplot,xvalues,yvalues,linestyle = 2
endif else begin
  plot,xvalues,yvalues,linestyle = 2,noerase = noerase
endelse
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::setxvalues,x,yout = yout, $
               _Extra = extra


nfuncs = self->count()
yout = 0.0
if nfuncs ne 0 then begin
  oall = self->get(/all)
  for i = 0,nfuncs-1 do begin
    oall[i]->setproperty,xvalues = x,/calculate
  endfor
endif
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func_cont::evaluate,x,yout = yout, $
               _Extra = extra

nfuncs = self->count()
yout = 0.0
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
format = '(e12.3)'
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
chiFormat = '(f10.3)'
strout = 'Chi-squared: '+strtrim(string(self.chisq,format = chiFormat),2)
output = [output,'']
output = [output,strout]
return
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
          INHERITS FUNC,$
          INHERITS IDL_CONTAINER}
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
