function linear_interpol_err,x,x_new,y,y_old_err
;calculates the error for linear interpolation, the default for interpol.pro
;   Math:
;       (y-y0)/(x-x0)=(y1-y0)/(x1-x0)
;       
;        y=y0+(x-x0)(y1-y0)/(x1-x0)
;
;        y_err=sqrt((dy/dy0*delta_y0)^2+(dy/dy1*delta_y1)^2)
;
;        y_err=sqrt(((1+(x-x0)(-1)/(x1-x0))*delta_y0)^2 + ((x-x0)(1)/(x1-x0)*delta_y1)^2)
;
;   Note: If y_old_err => variance, then y_old_err=sqrt(variance)

x=double(x)
x_new=double(x_new)
y=double(y)
y_old_err=double(y_old_err)
y_new_err=dblarr(n_elements(x_new))

if n_elements(x_new) eq n_elements(x) then return, y_old_err

for i=0, n_elements(x)-2 do begin
    between=where((x_new lt x[i+1]) and (x_new gt x[i]),num_between)
    ;print,num_between
    for j=0,num_between-1 do begin
        y0_err=(1+(x_new(between(j))-x(i))*(-1.0)/(x(i+1)-(x(i))))*y_old_err(i)        
        y1_err=(x_new(between(j))-x(i))*(1)/(x(i+1)-x(i))*y_old_err(i+1)
        y_new_err(between(j))=sqrt(y0_err^2.0+y1_err^2.0)
        ;print,y_new_err(between(j)),x_new(between(j))
    endfor
endfor

return,y_new_err;

end