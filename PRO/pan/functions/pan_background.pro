; $Id: pan_background.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_background,x,parms,$
                      	parmnames = parmnames, $
                      	canDraw = canDraw, $
                      	drawMessage = drawMessage, $
                    	_Extra = extra

if n_params() eq 0 then begin
  parmnames = ['offset','slope']
  return,-1
endif
drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag to set offset']
drawMessage[2:3] = ['Hold left mouse button down','and drag to change slope']

b = parms[0]
m = parms[1]
y = m*x+b
canDraw = 1
return,y
end