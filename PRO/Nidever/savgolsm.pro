function savgolsm,y,filter

if n_params() lt 1 then begin
  print,'Syntax - smy = savgolsm(y,filter)
  print,' e.g. filter = [16,16,4]'
  return,-1
endif

ny = n_elements(y)
if (ny lt filter(0)+filter(1)+2) then return,y

savgolfilter = savgol(filter(0),filter(1),0,filter(2))
return,convol(y,savgolfilter,/edge_truncate)

end
