pro gaussianmia,x,a,f,pder
;
;	Gauss function
;	f=a(0)*exp(-((x-a(1))/a(2))^2./2) + a(3)

f=a(0)*exp(-((x-a(1))/a(2))^2./2) + a(3)
if n_params() ge 4 then begin
	pder = dblarr(n_elements(x),4)
	pder(*,0)=exp(-((x-a(1))/a(2))^2./2.d0)
	pder(*,1)=(a(1)-x)*a(0)/a(2)^2*exp(-((x-a(1))/a(2))^2./2.d0)
	pder(*,2)=a(0)/a(2)^3*(x-a(1))^2*exp(-((x-a(1))/a(2))^2./2.d0)
	pder(*,3)=1.
endif

end
