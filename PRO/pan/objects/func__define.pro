; $Id: func__define.pro,v 1.2 2002/09/20 19:16:50 dimeo Exp $
;+
; NAME:
;       FUNC__DEFINE
;
; PURPOSE:
;
;       Object class for a set of functions used in OPAN__DEFINE.PRO.
;
;
; AUTHOR:
;
;       Robert M. Dimeo, Ph.D.
;		NIST Center for Neutron Research
;       100 Bureau Drive
;		Gaithersburg, MD 20899
;       Phone: (301) 975-8135
;       E-mail: robert.dimeo@nist.gov
;       http://www.ncnr.nist.gov/staff/dimeo
;
; CATEGORY:
;
;       Objects, data analysis, curve fitting
;
; CALLING SEQUENCE:
;
;       object = obj_new('FUNC')
;
;
; INPUT PARAMETERS:
;
;       NONE
;
; INPUT KEYWORDS:
;
;       name (required).
;
; REQUIRED PROGRAMS:
;
;        NONE
;
; COMMON BLOCKS:
;
;       NONE
;
; RESTRICTIONS
;
;       NONE
;
; OBJECT METHODS:
;	init -- usual lifecycle method
;	cleanup -- usual lifecycle method
;   changefirst -- user changes "amplitude" and "mode" of function
;	changesecond -- user changes "width" of function
;   draw
;   setproperty
;   getproperty
;	getDrawMessage
;
; DISCLAIMER
;
;		This software is provided as is without any warranty whatsoever.
;		Permission to use, copy, modify, and distribute modified or
;		unmodified copies is granted, provided this disclaimer
;		is included unchanged.
;
; MODIFICATION HISTORY:
;
;       Written by Rob Dimeo, September 17, 2002.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::cleanup
ptr_free,self.extra
ptr_free,self.xPtr
ptr_free,self.yPtr
ptr_free,self.stepPtr
ptr_free,self.ymaxPtr,self.xmodePtr
ptr_free,self.lowPtr,self.lowvalPtr
ptr_free,self.highPtr,self.highvalPtr
ptr_free,self.fixPtr,self.fixedvalPtr
ptr_free,self.parmErrorPtr
ptr_free,self.tiedPtr
ptr_free,self.parmPtr
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func::getDrawMessage
y = call_function(self.name,*self.xPtr,*self.parmPtr, $
    expr = self.expr, $
    extra = *self.extra,drawMessage = drawMessage)
return,drawMessage
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::changefirst,x,y,xrange
; This calculates new yvalues based on a first mouse click
self->getproperty,parms = parms
case self.name of
'pan_gaussian':		begin
					  oldwidth = 0.05*(xrange[1]-xrange[0])
					  area = parms[0] & center = parms[1]
					  width = oldwidth
					  sig = width/sqrt(8.0*alog(2.0))
					  area = y*sqrt(2.0*!dpi*sig^2)
					  parms[0] = area
					  parms[1] = x
					  parms[2] = width
					  *self.ymaxPtr = y
					  self->setproperty,parms = parms,/calculate
					end

'pan_lorentzian':	begin
					  oldwidth = 0.05*(xrange[1]-xrange[0])
					  area = parms[0] & center = parms[1]
					  width = oldwidth
					  gam = 0.5*width
					  area = y*gam*!dpi
					  parms[0] = area
					  parms[1] = x
					  parms[2] = width
					  *self.ymaxPtr = y
					  self->setproperty,parms = parms,/calculate
					end

'pan_lognormal':	begin
					  oldwidth = 0.02*(xrange[1]-xrange[0])
					  area = parms[0] & center = parms[1]
					  amp = y
					  *self.ymaxPtr = y
					  sig = oldWidth/2.354
					  xmode = exp(center-sig^2)
					  *self.xmodePtr = xmode
					  area = 2.0*sqrt(2.0*!dpi*sig^2)*abs(xmode)*y*exp(-0.5*sig^2)
					  parms[0] = area
					  parms[1] = sig^2+alog(abs(x))
					  parms[2] = sig
					  self->setproperty,parms = parms,/calculate
					end

'pan_dho':			begin
					  oldwidth = 0.1*(xrange[1]-xrange[0])
					  amp = parms[0]
					  center = parms[1]
					  width = oldwidth
					  gamma = 0.5*width
					  t = 293.0
					  area = y/pan_dho(x,[1.0,center,width,t])
					  parms[0] = area
					  parms[1] = x
					  parms[2] = width
					  parms[3] = 293.0
					  *self.ymaxPtr = y
					  self->setproperty,parms = parms,/calculate
					end

'pan_background':	begin
					  parms[0] = y
					  parms[1] = 0.0
					  *self.ymaxPtr = y
					  self->setproperty,parms = parms,/calculate
					end

'pan_step':			begin
					  width = 0.05*(xrange[1]-xrange[0])
					  parms[0] = y
					  parms[1] = x
					  parms[2] = width
					  self->setproperty,parms = parms,/calculate
					end

else:
endcase
return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::changesecond,x,y,xrange
; This calculates new yvalues based on a second mouse click
self->getproperty,parms = parms
case self.name of
'pan_gaussian':		begin
					  area = parms[0] & center = parms[1]
					  width = 2.0*abs(x-center)
					  sig = width/sqrt(8.0*alog(2.0))
					  amp = parms[0]/sqrt(2.0*!dpi*sig^2)
					  area = *self.ymaxPtr*sqrt(2.0*!dpi*sig^2)
					  parms[0] = area
					  parms[2] = width
					  self->setproperty,parms = parms,/calculate
					end

'pan_lorentzian':	begin
					  area = parms[0] & center = parms[1]
					  width = 2.0*abs(x-center)
					  gamma = 0.5*width
					  amp = parms[0]/(gamma*!dpi)
					  area = *self.ymaxPtr*gamma*!dpi
					  parms[0] = area
					  parms[2] = width
					  self->setproperty,parms = parms,/calculate
					end

'pan_lognormal':	begin

					  area = parms[0] & mu = parms[1]
					  wid = 0.02*(xrange[1]-xrange[0])
					  oldSig = wid/2.354
					  xmode = *self.xmodePtr

					  newWidth = abs(-(abs(x)-xmode-wid)) > 1d-8
					  newSig = newWidth/2.354
					  xmode = exp(mu-newSig^2)

					  newArea = 2.0*sqrt(2.0*!dpi*newSig^2)*abs(xmode)*(*self.ymaxPtr)* $
					            exp(-0.5*newSig^2)

					  parms[0] = area
					  parms[1] = newSig^2+alog(abs(xmode))
					  parms[2] = newSig
					  self->setproperty,parms = parms,/calculate
					end

'pan_dho':			begin
					  oldwidth = 0.1*(xrange[1]-xrange[0])
					  amp = parms[0] & center = parms[1]
					  width = oldwidth
					  newWidth = abs(-(abs(x)-center-width)) > 1d-8
					  gamma = 0.5*newWidth
					  area = (*self.ymaxPtr)/pan_dho(x,[1.0,center,newWidth,293.0])
					  parms[0] = area
					  parms[2] = newWidth
					  *self.ymaxPtr = y
					  self->setproperty,parms = parms,/calculate
					end

'pan_background':	begin
					  offset = *self.ymaxPtr
					  mp = 0.5*(xrange[0]+xrange[1])
					  slope = (y-offset)/(x-mp)
					  parms[1] = slope
					  parms[0] = offset-slope*mp
					  self->setproperty,parms = parms,/calculate
					end

'pan_step':			begin
					  width = 0.01*(xrange[1]-xrange[0])
					  center = parms[1]
					  parms[2] = abs(x-width-center)
					  self->setproperty,parms = parms,/calculate
					end

else:
endcase

return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::draw,overplot = overplot
if n_elements(overplot) eq 0 then overplot = 0
if overplot eq 0 then begin
  plot,*self.xPtr,*self.yPtr,linestyle = 2
endif else begin
  oplot,*self.xPtr,*self.yPtr,linestyle = 2
endelse
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::setproperty, $
              name = name,$
              xvalues = xvalues, $
              step = step, $
              parms = parms, $
              fixed = fixed, $
              fixvalues = fixvalues, $
              low = low, $
              lovalues = lovalues, $
              high = high, $
              hivalues = hivalues, $
              parmError = parmError, $
              canDraw = canDraw, $
              tied = tied, $
              expr = expr, $
              calculate = calculate	; set this keyword to populate self.yPtr with
              						; the updated values

!except = 0
if n_elements(expr) ne 0 then self.expr = expr
if n_elements(canDraw) ne 0 then self.canDraw = canDraw
if n_elements(tied) ne 0 then *self.tiedPtr = tied
if n_elements(step) ne 0 then *self.stepPtr = step
if n_elements(name) ne 0 then self.name = name
if n_elements(xvalues) ne 0 then *self.xPtr = xvalues
if n_elements(parms) ne 0 then *self.parmPtr = parms
if n_elements(parmError) ne 0 then *self.parmErrorPtr = parmError
if n_elements(fixed) ne 0 then *self.fixPtr = fixed
if n_elements(fixvalues) ne 0 then *self.fixedvalPtr = fixvalues
if n_elements(low) ne 0 then *self.lowPtr = low
if n_elements(lovalues) ne 0 then *self.lowvalPtr = lovalues
if n_elements(high) ne 0 then *self.highPtr = high
if n_elements(hivalues) ne 0 then *self.highvalPtr = hivalues
; Now that we have all of the information, calculate the function if
; the user so desires.
if keyword_set(calculate) then begin
  if n_elements(*self.xPtr) ne 0 then begin
    *self.yPtr = call_function(self.name,*self.xPtr,*self.parmPtr, $
                 expr = self.expr, extra = *self.extra)
  endif else begin
    print,'Error...no valid x-values'
  endelse
endif
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func::getproperty, $
              name = name,  $
              xvalues = xvalues, $
              yvalues = yvalues,  $
              step = step, $
              parms = parms, $
              fixed = fixed, $
              fixvalues = fixvalues, $
              low = low, $
              lovalues = lovalues, $
              high = high, $
              hivalues = hivalues,$
              parmnames = parmnames,$
              parmError = parmError, $
              tied = tied, $
              canDraw = canDraw, $
              expr = expr

if arg_present(parmnames) then begin
  if self.expr eq '' then begin
    z = call_function(self.name,parmnames = parmnames,extra = *self.extra)
  endif else begin
    z = call_function(self.name,*self.xPtr,*self.parmPtr, $
                      parmnames = parmnames, $
                      expr = self.expr, extra = *self.extra)
  endelse
endif
if arg_present(expr) then expr = self.expr
if arg_present(canDraw) then canDraw = self.canDraw
if arg_present(tied) then tied = *self.tiedPtr
if arg_present(step) then step = *self.stepPtr
if arg_present(name) then name = self.name
if arg_present(xvalues) then xvalues = *self.xPtr
if arg_present(yvalues) then yvalues = *self.yPtr
if arg_present(parms) then parms = *self.parmPtr
if arg_present(parmError) then parmError = *self.parmErrorPtr
if arg_present(fixed) then fixed = *self.fixPtr
if arg_present(fixvalues) then fixvalues = *self.fixedvalPtr
if arg_present(low) then low = *self.lowPtr
if arg_present(lovalues) then lovalues = *self.lowvalPtr
if arg_present(high) then high = *self.highPtr
if arg_present(hivalues) then hivalues = *self.highvalPtr
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function func::init, $
                   name = name, $
                   xvalues = xvalues,$
                   step = step, $
                   parms = parms, $
                   fixed = fixed, $
                   fixvalues = fixvalues, $
                   low = low, $
                   lovalues = lovalues, $
                   high = high, $
                   hivalues = hivalues, $
                   tied = tied, $
                   expr = expr, $
                   _Extra = extra

; Name must be present!
self.extra = ptr_new(extra)
if n_elements(name) eq 0 then return,-1
self.parmErrorPtr = ptr_new(/allocate_heap)
self.name = name
self.stepPtr = ptr_new(/allocate_heap)
self.xPtr = ptr_new(/allocate_heap)
self.yPtr = ptr_new(/allocate_heap)
if n_elements(xvalues) eq 0 then begin
  xlo = -10.0 & xhi = 10.0 & nx = 100
  dx = (xhi-xlo)/(nx-1.0) & x = xlo+dx*findgen(nx)
  *self.xPtr = x
endif else begin
  *self.xPtr = xvalues
endelse
; Let's fill up the function with some dummy data if none is given
; when instantiating the class....
self.parmPtr = ptr_new(/allocate_heap)
self.lowPtr = ptr_new(/allocate_heap)
self.lowvalPtr = ptr_new(/allocate_heap)
self.highPtr = ptr_new(/allocate_heap)
self.highvalPtr = ptr_new(/allocate_heap)
self.fixPtr = ptr_new(/allocate_heap)
self.tiedPtr = ptr_new(/allocate_heap)
self.fixedvalPtr = ptr_new(/allocate_heap)
self.ymaxPtr = ptr_new(/allocate_heap)
self.xmodePtr = ptr_new(/allocate_heap)

; Test if we're defining a user function.  We are explicitly
; assuming here that the xvalues, expression, parms, and
; parmnames are being passed in here!!!!

if n_elements(expr) ne 0 then begin

  self.expr = expr
  *self.parmPtr = parms
  dummy = call_function(self.name,xvalues,parms, $
                        parmnames = parmnames, $
                        expr = self.expr, $
                        canDraw = canDraw, $
                        extra = *self.extra)
  self.canDraw = canDraw
  nparms = n_elements(parms)

endif else begin

  ; Call the function to get out the parameter names
  dummy = call_function(self.name,parmnames = parmnames,$
          extra = *self.extra)
  nparms = n_elements(parmnames)

endelse

unity = 1.0+0.0*findgen(nparms)
zerof = 0.0*unity
zeroi = 0*indgen(nparms)
*self.tiedPtr = n_elements(tied) eq 0 ? replicate(0D,nparms) : tied
*self.stepPtr = n_elements(step) eq 0 ? replicate(0D,nparms) : step
*self.parmErrorPtr = replicate(0D,nparms)

*self.parmPtr = n_elements(parms) eq 0 ? unity : parms
if n_elements(expr) eq 0 then expr = ''
self.expr = expr
canDraw = 0
*self.yPtr = call_function(self.name,*self.xPtr,*self.parmPtr, $
                             expr = self.expr, canDraw = canDraw, $
                             extra = *self.extra)

self.canDraw = canDraw

*self.fixPtr = n_elements(fixed) eq 0 ? zeroi : fixed
*self.fixedvalPtr = n_elements(fixvalues) eq 0 ? *self.parmPtr : fixvalues
*self.lowPtr = n_elements(low) eq 0 ? zeroi : low
*self.lowvalPtr = n_elements(lovalues) eq 0 ? zerof : lovalues
*self.highPtr = n_elements(high) eq 0 ? zeroi : high
*self.highvalPtr = n_elements(hivalues) eq 0 ? zerof : hivalues
return,1
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro func__define
;
define = {func, $
          name:"", $
          expr:"", $
          canDraw:0, $
          extra:ptr_new(), $
          ymaxPtr:ptr_new(), $
          xmodePtr:ptr_new(), $
          parmPtr:ptr_new(), $
          parmErrorPtr:ptr_new(), $
          xPtr:ptr_new(),  $
          yPtr:ptr_new(),  $
          stepPtr:ptr_new(), $
          fixPtr:ptr_new(),  $
          fixedvalPtr:ptr_new(),  $
          lowPtr:ptr_new(),  $
          lowvalPtr:ptr_new(),  $
          highPtr:ptr_new(),  $
          tiedPtr:ptr_new(), $
          highvalPtr:ptr_new() $
          }
end
