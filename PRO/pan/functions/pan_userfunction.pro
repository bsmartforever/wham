; $Id: pan_userfunction.pro,v 1.1 2002/09/19 21:29:56 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function pan_userfunction,x,p,$
                    	parmnames = parmnames, $
                    	expr = expr, $
                    	canDraw = canDraw, $
                    	eval = eval, $
                    	_Extra = extra

parmnames = strarr(n_elements(p))
for i = 0,n_elements(p)-1 do begin
  parmnames[i] = 'p['+strtrim(string(i),2)+']'
endfor

evalExpr = 'yout='+expr
eval = execute(evalExpr,1)
if eval ne 1 then yout = -1
canDraw = 0

return,yout
end