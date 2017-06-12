; $Id: pan_gaussian.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_gaussian,x,parms,$
                    	parmnames = parmnames, $
                    	canDraw = canDraw, $
                    	drawMessage = drawMessage, $
                    	_Extra = extra

if n_params() eq 0 then begin
  parmnames = ['area','center','FWHM']
  return,-1
endif
drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag function centroid']
drawMessage[2:3] = ['Hold left mouse button down','and drag to change width']
fwhm = parms[2]
sig = fwhm/2.354
area = parms[0]
cen = parms[1]
yout = (area/sqrt(2.0*!dpi*sig^2))*exp(-0.5*((x-cen)/sig)^2)
canDraw = 1
return,yout
end