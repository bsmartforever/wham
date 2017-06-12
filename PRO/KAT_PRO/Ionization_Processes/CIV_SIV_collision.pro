T=findgen(100000)*1000.+8000.
print,alog10(max(T))

lambdaC=1550e-10 ;meters
lambdaSi=1204e-10 ;meters
c=2.99792e8 ; m/s
h=6.58211928e-16 ; eV*s
k=8.617386e-5 ;eV/K


plot,alog10(T),8.91/16.0*exp(-(h*c/(lambdaC-lambdaSi))/(k*T))*(abundance('C',/quiet)/abundance('Si',/quiet)),$
	xtitle='log(T/K)',ytitle='N CIV / N SiIV',charsize=1.5,xrange=[4,7],xstyle=1,$
	title='Expected ratios from collisional exitation rate'

oplot,alog10(T),8.91/16.0*exp(-(h*c/(lambdaC-lambdaSi))/(k*T))*(abundance('C',/quiet,/dep)/abundance('Si',/quiet,/dep)),$
	linestyle=2

oplot,[1.,1.]*5.,!y.crange,linestyle=1
oplot,[1.,1.]*5.5,!y.crange,linestyle=1
oplot,[1.,1.]*6.0,!y.crange,linestyle=1

oplot,!x.crange,[1.,1.],linestyle=1

legend,['No Dust','MW Depletion Patterns'],linestyle=[0,2],charsize=1.5,box=0,/left,/top

T=[10.^4.,10.^4.5,10^5.0,10.^5.5]
print,8.91/16.0*exp(-(h*c/(lambdaC-lambdaSi))/(k*T))*(abundance('C',/quiet)/abundance('Si',/quiet))
print,8.91/16.0*exp(-(h*c/(lambdaC-lambdaSi))/(k*T))*(abundance('C',/quiet,/dep)/abundance('Si',/quiet,/dep))





T=findgen(100000)*1000.+5000.

plot,alog10(T),exp(-(h*c/(lambdaC))/(k*T))*abundance('C',/quiet),$
	xtitle='log(T/K)',ytitle='N CIV / N SiIV',charsize=1.5,xrange=[3,7],xstyle=1,$
	title='Expected ratios from collisional exitation rate'

oplot,alog10(T),exp(-(h*c/(lambdaSi))/(k*T))*abundance('Si',/quiet,/dep),$
	linestyle=2



end