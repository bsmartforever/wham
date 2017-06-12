restore,'$HOME/WHAM/MS/reducing/pointed_summary/ha.sav'
x=ha[2].vel
y=ha[2].data
err=sqrt(ha[0].data_var)


;initial guesses
height=[0.001,0.001]
center=[100,200]
width=[30,30]

;fix these variables. ALL VARIABLES CANNOT BE FIXED!
;fix_height=[1,1]
;fix_center=[1,1]
;fix_width=[1,1]

;Tolerances to those variables 
; *** Tolerance trumps the fixed variable keyword.
;tol_height=[.0001,0.0001]
;tol_center=[5,5]
;tol_width=[10,10]

fit=multi_gauss(x,y,err=err,$
	height=height,center=center,width=width,$
	fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
	tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
	fit_params=fit_params,/wham)

;ploterror,x,y,err

plot,x,y
oplot,fit.x,fit.y
oplot,!x.crange,[0,0],linestyle=1

end