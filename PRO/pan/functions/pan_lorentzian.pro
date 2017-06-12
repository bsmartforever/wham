; $Id: pan_lorentzian.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_lorentzian,x,parms,$
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
gamma = fwhm/2.0
area = parms[0]
cen = parms[1]
yout = (area*gamma/!dpi)/((x-cen)^2+gamma^2)
canDraw = 1
return,yout
end