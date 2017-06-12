; $Id: homanycommas.pro,v 1.1 2002/09/19 21:30:49 dimeo Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
function howmanycommas,instr
len = strlen(instr)
n = 0
for i = 0,len-1 do begin
  thisChar = strmid(instr,i,1)
  if thisChar eq ',' then n = n+1
endfor
return,n
end