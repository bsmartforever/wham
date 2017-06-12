; $Id: pan_lognormal.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_lognormal, x,parms,$
                    	parmnames = parmnames, $
                    	canDraw = canDraw, $
                    	drawMessage = drawMessage, $
                    	_Extra = extra

if n_params() eq 0 then begin
  parmnames = ['area','mean','std dev']
  return,-1
endif

drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag function centroid']
drawMessage[2:3] = ['Hold left mouse button down','and drag to change width']

amp = parms[0]
cen = parms[1]
sig = parms[2]

wherezero = where(x eq 0.0,countZero)
whereNotZero = where(x ne 0.0)
y = dblarr(n_elements(x))
y[whereNotZero] = (0.5*amp/(sqrt(2.0*!pi*sig^2)))*(1.0/abs(x[whereNotZero]))*$
                  exp(-0.5*(alog(abs(x[whereNotZero]))-abs(cen))^2/sig^2)
if countZero gt 0 then begin
  y[wherezero] = 0.0
endif

canDraw = 1
return,y
end