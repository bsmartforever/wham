function intensity_error, params, num=num, min_width=min_width,$
	mR=mR, k2cm2=k2cm2, factor=factor, log=log, quiet=quiet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; intensity_error, params
;
; Purpose: To determine the systematic uncertainty by varying each parameter until 
;		   the chi^2 of the best fit is increased by one. 
;
;		   This is an automated program, it does not plot the fits. 
; Input:
;   params - Structure that is supplied by intensity.pro when calculating the intensity
;			 using the /gauss keyword.  
;   
;   
; Created by Dr. Kat Barger 06/2015
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if n_elements(whamip) ne 0 then whamip=1
	if (NOT keyword_set(num)) then num=10.

	if keyword_set(k2cm2) then factor=!k2cm2 else $
	if keyword_set(mR) then factor=1.0/22.8*1.0e3 else $
	if (NOT keyword_set(factor)) then factor=1

	if keyword_set(k2cm2) AND (NOT keyword_set(min_width)) then min_width=10 $
		else if (NOT keyword_set(min_width)) then min_width=20

	;Create parameter grids
	width_arr=makearr(num,min_width,100.)
	center_arr=makearr(num,params.center-25.,params.center+25.)
	;height_arr=makearr(num,params.height*0.5,params.height*1.5)

	height_image=fltarr(n_elements(width_arr),n_elements(center_arr))
	area_image=height_image ;Only initializing this array
	chisq_image=height_image ;Only initializing this array

   ;fix_height,fix_center,fix_width

   ;For now, not doing anything with this yet
	ngauss=n_elements(params)

	good=where((params.vel ge params.int_range[0]) and (params.vel le params.int_range[1]),count)
	xvar=params.vel[good]
	yvar=params.data[good]
	if (n_elements(params.var) NE 0) then errval=sqrt(params.var[good]) 
		;Integration range

	for i=0, n_elements(width_arr)-1 do begin
		width=width_arr[i]
		for j=0, n_elements(center_arr)-1 do begin
			center=center_arr[j]
			;This is only good for one Gaussian. Need to adjust if more than one.
			fix_width=[1]
			fix_center=[1]

			fit=multi_gauss(xvar,yvar-params.ycon[good],err=errval[good],ngauss=ngauss,$
			      height=height,center=center,width=width,$
			      fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
			      tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
			      fit_params=fit_params,wham=whamip,ip=ip,/quiet)

			;print,i,j,width,center,fit_params.height
			height_image[i,j]=fit_params.height
			area_image[i,j]=gauss_area([fit_params.height,fit_params.center,fit_params.width])
			chisq_image[i,j]=fit.chisq

		endfor
	endfor

	;Best chisq value is equal to the minimum value of chisq. However, if min(chisq_image) is less than 1.,
	;set to 1.
	chisq_best=min(chisq_image) > 1.0

	pause

	p = [0.02, 0.15, 0.9, 0.98]
	cgimage,height_image,minvalue=min(height_image),maxvalue=max(height_image),$
		xrange=[min(width_arr),max(width_arr)],yrange=[min(center_arr),max(center_arr)],$
		xtitle='width',ytitle='center',AXKEYWORDS={yminor:2},$
		/keep_aspect_ratio,/axis,Position=p
	CONTOUR, chisq_image,levels=chisq_best+1,/noerase,position=p,xstyle=4,ystyle=4
	cgColorbar, Position=[p[2]+0.05, p[1], p[2]+0.1, p[3]],/vertical,$
		title='height',/right,minrange=min(height_image),maxrange=max(height_image)

		pause

	p = [0.02, 0.15, 0.9, 0.98]
	cgimage,chisq_image,minvalue=min(chisq_image),maxvalue=max(chisq_image),$
		xrange=[min(width_arr),max(width_arr)],yrange=[min(center_arr),max(center_arr)],$
		xtitle='width',ytitle='center',AXKEYWORDS={yminor:2},$
		/keep_aspect_ratio,/axis,Position=p
	CGCONTOUR, chisq_image,levels=chisq_best+1.,/noerase,position=p,xstyle=4,ystyle=4,/onimage,label=0
	cgColorbar, Position=[p[2]+0.05, p[1], p[2]+0.1, p[3]],/vertical,$
		title='chisq',/right,minrange=min(chisq_image),maxrange=max(chisq_image)

		pause

	p = [0.02, 0.15, 0.9, 0.98]
	cgimage,area_image,minvalue=min(area_image),maxvalue=max(area_image),$
		xrange=[min(width_arr),max(width_arr)],yrange=[min(center_arr),max(center_arr)],$
		xtitle='width',ytitle='center',AXKEYWORDS={yminor:2},$
		/keep_aspect_ratio,/axis,Position=p
	CONTOUR, chisq_image,levels=chisq_best+1,/noerase,position=p,xstyle=4,ystyle=4
	cgColorbar, Position=[p[2]+0.05, p[1], p[2]+0.1, p[3]],/vertical,$
		title='area',/right,minrange=min(area_image),maxrange=max(area_image)

	image_index=where(chisq_image le chisq_best+1)

	   s = SIZE(chisq_image)
	   ncol = s(1)
	   row_index = image_index / ncol
	   col_index = image_index MOD ncol
	
	error_spread={area:[min(area_image[col_index,row_index]),max(area_image[col_index,row_index])],$
				  center:[center_arr[min(row_index)],max(center_arr[max(row_index)])],$
				  width:[width_arr[min(col_index)],max(width_arr[max(col_index)])]}


	if (NOT keyword_set(quiet)) then begin

		tags=[tag_loc(params,'area'),tag_loc(params,'center'),tag_loc(params,'width')]
		tags_err=[tag_loc(params,'area_err'),tag_loc(params,'center_err'),tag_loc(params,'width_err')]
		tags_spread=[tag_loc(error_spread,'area'),tag_loc(error_spread,'center'),tag_loc(error_spread,'width')]
	
		fmt='(f8.2)'
		num_vars=3
		string_arr=strarr(num_vars)
		params_names=['Intensity: ','Center: ', 'Width: ']
	
		for i=0, num_vars-1 do begin
	
			if i eq 0 then fac=factor else fac=1.
			if params.(tags[i])-error_spread.(tags_spread(i))[0] le 0 then lower=params.(tags[i]) $
				else lower=params.(tags[i])-error_spread.(tags_spread(i))[0]
			string_arr[i]=strcompress(string(params.(tags[i])*fac,format=fmt)+$
				'+/-'+string(params.(tags_err[i])*fac,format=fmt)+$
				'(-'+string(lower*fac,format=fmt)+$
				'+'+string(error_spread.(tags_spread(i))[1]*fac-params.(tags[i])*fac,format=fmt)+')',/re)
	
			print,params_names[i],string_arr[i],format='(A-12,A-35)'
		endfor 
	
	endif

	return, 0

end