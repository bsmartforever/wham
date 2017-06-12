; $Id: pan_dho.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_dho, 		x,parms,$
                    	parmnames = parmnames, $
                    	canDraw = canDraw, $
                    	drawMessage = drawMessage, $
                    	_Extra = extra
if n_params() eq 0 then begin
  parmnames = ['amp','center','fwhm','temperature']
  return,-1
endif

drawMessage = strarr(4)
drawMessage[0:1] = ['Hold left mouse button down','and drag function centroid']
drawMessage[2:3] = ['Hold left mouse button down','and drag to change width']

amp = parms[0]
cen = abs(parms[1])
width = abs(parms[2])
gam = 0.5*width
temperature = parms[3]

npts = n_elements(x)
bose = dblarr(npts)
wherevalid = where(x ne 0.0,count)
if count eq npts then begin
  bose = 1./(1.-exp(-11.6*x/temperature))
endif else begin
  wherenotvalid = where(x eq 0.0,newcount)
  bose[wherevalid] = 1./(1.-exp(-11.6*x[wherevalid]/temperature))
  bose[wherenotvalid] = 1./(1.-exp(-11.6*x[wherenotvalid]/temperature))
endelse
y = dblarr(npts)
y = bose*amp*(gam/!pi)*((1.0/((x-cen)^2+gam^2))-(1.0/((x+cen)^2+gam^2)))

canDraw = 1
return,y
end