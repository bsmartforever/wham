function slope, y, x, acc=acc

ny = n_elements(y)

if N_params() LT 1 or ny eq 0 then begin   
       print, 'Syntax - result=slope(Yarray [,Xarray])'
       print, 'dY(i) = Y(i+1)-Y(i)
       return,-1
endif

n=n_elements(y)

if not keyword_set(acc) then begin
  dely=y(1:n-1)-y(0:n-2)

  if keyword_set(x) then dely=dely/(x(1:n-1)-x(0:n-2))
endif else begin

  xx = dindgen(n)
  yy = spline(xx,y,xx+0.01)
  dely = (yy-y)/0.01

endelse

return,dely

end
