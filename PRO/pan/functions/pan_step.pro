; $Id: pan_step.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_step, 		x,parms,$
                    	parmnames = parmnames, $
                    	canDraw = canDraw, $
                    	drawMessage = drawMessage, $
                    	_Extra = extra
if n_params() eq 0 then begin
  parmnames = ['amp','center','fwhm']
  return,-1
endif
drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag transition point']
drawMessage[2:3] = ['Hold left mouse button down', $
                    'and drag to change transition width']
amp = parms[0]
cen = parms[1]
width = parms[2]
sig = width/2.354
if (!version).release le 5.4 then $
  y = 0.5*amp*(1.0+errorf((x-cen)/sqrt(2.0*sig^2))) $
else y = 0.5*amp*(1.0+erf((x-cen)/sqrt(2.0*sig^2)))
canDraw = 1
return,y
end