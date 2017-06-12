function trapz,x,y

;
;	Numerical integration using the composed trapezoidal rule
;
;	int[f(x)dx] ~= 1/2 (y1*(X2-X1)+yn*(Xn-Xn-1))+ 1/2*sum_i[yi*(Xi+1-Xi-1)]
;
;	IN: x 	- fltarr	abscisas
;	    y	- fltarr	ordinates
;
;	OUT: trazp - float	the numerical approx. to the integral of y(x)
;	
;	NOTE: double precision is used
;
;	C. Allende Prieto, Sep 1998
;	", March 2010, changed loop variable to long
;
n=n_elements(x)-1
trapz=0.d0
trapz=0.5d0*y(0)*(x(1)-x(0))
for i=1l,n-1 do begin
	trapz=trapz+0.5d0*y(i)*(x(i+1)-x(i-1))
endfor
if (n gt 0) then trapz=trapz+0.5d0*y(n)*(x(n)-x(n-1))
return,trapz
end
