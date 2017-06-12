;pro testing,quiet=quiet

restore,'$HOME/WHAM/MS/reducing/pointed_summary/ha.sav'
x=ha[2].vel
y=ha[2].data
err=sqrt(ha[0].data_var)

;remove error values that are zero; this will mess up the fitting
if keyword_set(err) then good=where(err ne 0) else good=indgen(n_elements(x))
XVALS=x[good]
YVALS=y[good]
if keyword_set(err) then ERRVALS=err[good]

;remove all infinity values
good=where(FINITE(ERRVALS, /NAN) eq 0)
XVALS=XVALS[good]
YVALS=YVALS[good]
if keyword_set(err) then ERRVALS=ERRVALS[good]

ploterror,XVALS,YVALS,ERRVALS

;initial guesses
height=[0.001,0.001]
center=[100,200]
width=[30,30]

;fix_height=[1,1]
;fix_center=[1,1]
;fix_width=[1,1]

;Tolerances 
;height_tol=[.0001,0.0001]
;center_tol=[5,5]
;width_tol=[10,10]

p=transpose([[height],[center],[width]])

;p=[[0.001,100,30,0],[0.0001,200,30,0]]
ngauss=2

;fix_width=fltarr(ngauss)+1
;fix_cen=fltarr(ngauss)+1
;fix_height=fltarr(ngauss)+1

for j=0,ngauss-1 do begin
	;fix variables
    ;ngauss * 3 parameter per guass
    num_p = ngauss*3.0
    parinfo = replicate({value:0.D, fixed:0, limited:[0,0], $
                       limits:[0.D,0]}, num_p) 
 
 	p_tmp=p[0:2,*]
    parinfo[*].value = reform(p_tmp,ngauss*3) ;starting values

    ; hold width fixed to 30 km/s
    fix_width_loc=fltarr(ngauss)
    for j=0,ngauss-1 do fix_width_loc[j]=2+3*j
    if (NOT keyword_set(width_tol)) then width_tol=fltarr(ngauss)-1
    if (NOT keyword_set(fix_width)) then fix_width=fltarr(ngauss)
    limited=[0,0]
    for j=0,ngauss-1 do begin
    	if width_tol[j] eq -1 then tol=0 else tol=width_tol[j]
    	parinfo[fix_width_loc[j]].limits=[width[j]-tol,width[j]+tol]
		if (tol eq 0) AND (fix_width[j] ne 1) then limited=[0,0] else limited=[1,1]
    	parinfo[fix_width_loc[j]].limited = limited
	endfor
    fix_width=[(width_tol[0] eq -1) OR (width_tol[0] eq 0) AND (fix_width[0] eq 1),$
    	        (width_tol[1] eq -1) OR (width_tol[0] eq 0) AND (fix_width[1] eq 1)]
	parinfo[fix_width_loc].fixed = fix_width

    ;limit the center 
    fix_center_loc=fltarr(ngauss)
    for j=0,ngauss-1 do fix_center_loc[j]=1+3*j
    if (NOT keyword_set(center_tol)) then center_tol=fltarr(ngauss)-1
    if (NOT keyword_set(fix_center)) then fix_center=fltarr(ngauss)
    limited=[0,0]
    for j=0,ngauss-1 do begin
    	if center_tol[j] eq -1 then tol=0 else tol=center_tol[j]
    	parinfo[fix_center_loc[j]].limits=[center[j]-tol,center[j]+tol]
		if (tol eq 0) AND (fix_center[j] ne 1) then limited=[0,0] else limited=[1,1]
    	parinfo[fix_center_loc[j]].limited = limited
	endfor
    fix_center=[(center_tol[0] eq -1) OR (center_tol[0] eq 0) AND (fix_center[0] eq 1),$
    	        (center_tol[1] eq -1) OR (center_tol[0] eq 0) AND (fix_center[1] eq 1)]
	parinfo[fix_center_loc].fixed = fix_center

    fix_height_loc=fltarr(ngauss)
    for j=0,ngauss-1 do fix_height_loc[j]=0+3*j
    if (NOT keyword_set(height_tol)) then height_tol=fltarr(ngauss)-1
    if (NOT keyword_set(fix_height)) then fix_height=fltarr(ngauss)
    limited=[0,0]
    for j=0,ngauss-1 do begin
    	if height_tol[j] eq -1 then tol=0 else tol=height_tol[j]
    	parinfo[fix_height_loc[j]].limits=[height[j]-tol,height[j]+tol]
		if (tol eq 0) AND (fix_height[j] ne 1) then limited=[0,0] else limited=[1,1]
    	parinfo[fix_height_loc[j]].limited = limited
	endfor
    fix_height=[(height_tol[0] eq -1) OR (height_tol[0] eq 0) AND (fix_height[0] eq 1),$
    	        (height_tol[1] eq -1) OR (height_tol[0] eq 0) AND (fix_height[1] eq 1)]
	parinfo[fix_height_loc].fixed = fix_height

endfor

p_out=arm_multgaussfit(XVALS,YVALS,p,err=ERRVALS,ngauss=ngauss,$
	  chisq=chisq,parinfo=parinfo,perror=p_err,quiet=quiet)

area=fltarr(ngauss,2)
for i=0,n_elements(p_out)/3.-1 do $
	area[i,*]=gauss_area(p_out[(3*i):(3*(i+1))-1],err=p_err[(3*i):(3*(i+1))-1])

p_in=fltarr(ngauss*3.)
p_in_err=fltarr(ngauss*3.)
for i=0,n_elements(p_out)/3.-1 do begin
	p_in[(3*i):(3*(i+1))-1]=[p_out[(i*3+1)],p_out[((i*3+2))],area[i,0]]
	p_in_err[(3*i):(3*(i+1))-1]=[p_err[(i*3+1)],p_err[((i*3+2))],area[i,1]]
endfor
;YVALS = GAUSS1(XVALS, [MEAN, SIGMA, AREA], SKEW=skew)

yvals_fit=fltarr(n_elements(YVALS))
for i=0,n_elements(p_in)/3.-1 do begin
	good=where(p_in[(3*i):(3*(i+1))-1] ne 0,count)
	if count eq 3 then yvals_fit=gauss1(x,p_in[(3*i):(3*(i+1))-1])+yvals_fit
endfor

fit_params=replicate({area:0.0,area_err:0.0,$
					  center:0.0,center_err:0.0,$
					  width:0.0,width_err:0.0,$
					  height:0.0,height_err:0.0},ngauss)
fit_params.area=area[*,0]
fit_params.area_err=area[*,1]
for i=0, ngauss-1 do begin
	fit_params[i].center=p_in[i*3.]
	fit_params[i].center_err=p_in_err[i*3.]
	fit_params[i].width=p_in[i*3+1]
	fit_params[i].width_err=p_in_err[i*3+1]
	fit_params[i].height=p_out[i*3.]
	fit_params[i].height_err=p_err[i*3.]
endfor

fit={x:XVALS,y:yvals_fit,chisq:chisq}

print,''
print,'** Reduced Chi-Squared: ',chisq,format='(A-25,f-8.2)'
for i=0,ngauss-1 do begin
    print,''
    print,'** GAUSS 1 **','FIXED:',format='(A-50,A-25)'
    if (max(abs(fit_params[i].Area)) gt 1e5) or (min(abs(fit_params[i].Area)) lt 1e-3) then $
    print,'** Area +/- error:',fit_params[i].Area,fit_params[i].Area_err,format='(A-25,2e-25.2)' else $
    print,'** Area +/- error:',fit_params[i].Area,fit_params[i].Area_err,format='(A-25,2f-10.2)' 
    print,'** Center +/- error:',fit_params[i].Center,fit_params[i].Center_err,fix_center[i],format='(A-25,2f-10.2,I8)'
    print,'** Width +/- error:',fit_params[i].Width,fit_params[i].Width_err,fix_width[i],format='(A-25,2f-10.2,I8)'
    if (max(abs(fit_params[i].Height)) gt 1e5) or (min(abs(fit_params[i].Height)) lt 1e-2) then $
    print,'** Height +/- error:',fit_params[i].Height,fit_params[i].Height_err,fix_height[i],format='(A-25,2e-10.2,I8)' else $
    print,'** Height +/- error:',fit_params[i].Height,fit_params[i].Height_err,fix_height[i],format='(A-25,2f-10.2,I8)'
endfor
print,''

oplot,xvals,yvals_fit
oplot,!x.crange,[0,0],linestyle=1

end 