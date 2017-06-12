pro str_image, str, data=data, error=error, column=column, wham=wham,$
	min=min, max=max, vrange=vrange,$
	factor=factor, title=title,$
	glon=glon,glat=glat,mlon=mlon,mlat=mlat,$
	radius=radius,ndiv=ndiv,smallest=smallest,$
	path=path,level=level,opath=opath

;
; Purpose:
;
; Input:
;	str     - Structure containing mapped coordinants in either Galactic or 
;				Magellanic Stream latitude and longitude with either data or error tags.
;   data 	- Sets to a data verses position map.
;	error 	- Sets to an error verses position map.
;	column 	- Sets data type to HI column density. Expects units of Kelvin and 
;				converts to cm^-2.				
;	wham	- Sets radius to wham beam if not otherwise stated through the 
;				radius keyword and sets the units of the output plot to Rayleighs. 
;	min 	- Minimum data value to plot in image. Default is min value in image.
;	max 	- Maximum data value to plot in image. Default is max value in image.
;	title	- Title for color bar
; 	glon 	- [Galactic Longitude Minimum, Galactic Longitude Minimum]
;	glat 	- [Galactic Latitude Minimum, Galactic Latitude Minimum]
;	mlon	- [Magellanic Stream Longitude Minimum, Magellanic Stream Longitude Minimum]
; 	mlat 	- [Magellanic Stream Latitude Minimum, Magellanic Stream Latitude Minimum]
;	radius 	- Radius to search for near points along the specified glon & glat OR 
;				mlon & mlat positions.
;	ndiv	- Number of divisions to divide the path between the specified glon & glat OR 
;				mlon & mlat positions.
;	smallest - Use the smallest radius possible that yields a near neighbor, starting at 0.01
;				and adding 0.01 for each iteration that yeilds no neighbor.
;	level	- Contour levels in data units.
;	path 	- Output of contour path position in a structure.
;	opath	- Overplot contour path contained in the output of the path
;				variable when running str_image.pro. This is useful for comparing the 
;				emission morphology of different lines.
;
;
; Example:
;	str_image,hi_map,vrange=[100,325],glon=[300,282],glat=[-43.0,-33],$
;		min=0.0,max=15,ndiv=5000,/smallest,level=[1,2],path=path
;
;	To overplot the extracted contour path stored in the "path" variable
;	in the example above:
;
;	str_image,hi_map,vrange=[100,325],glon=[300,282],glat=[-43.0,-33],$
;		min=0.0,max=15,ndiv=5000,/smallest,opath=path
;	
;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

str_mod=str

if ((n_elements(glon) eq 2) AND (n_elements(glat) eq 2)) OR $
   ((n_elements(mlon) eq 2) AND (n_elements(mlat) eq 2)) then begin

   remove_tags,str_mod,['NAME'],tmp
   str_mod=tmp

   if keyword_set(glon) then begin
   		glon=float(glon) & glat=float(glat)
   		if min(glon) lt min(str_mod.glon) then begin
   			tmp=min(glon,loc) 
   			glon[loc]=min(str_mod.glon)
   		endif
   		if max(glon) gt max(str_mod.glon) then begin
   			tmp=max(glon,loc) 
   			glon[loc]=max(str_mod.glon)
   		endif
   		if min(glat) lt min(str_mod.glat) then begin
   			tmp=min(glat,loc) 
   			glat[loc]=min(str_mod.glat)
   		endif
   		if max(glat) gt max(str_mod.glat) then begin
   			tmp=max(glat,loc) 
   			glat[loc]=max(str_mod.glat)
   		endif
   		lon_ave=avg(glon)
   		lat_ave=avg(glat)
   		lon=glon & lat=glat
   endif else begin
   		mlon=float(mlon) & mlat=float(mlat)
   		if min(mlon) lt min(str_mod.mlon) then begin
   			tmp=min(mlon,loc) 
   			mlon[loc]=min(str_mod.mlon)
   		endif
   		if max(mlon) gt max(str_mod.mlon) then begin
   			tmp=max(mlon,loc) 
   			mlon[loc]=max(str_mod.mlon)
   		endif
   		if min(mlat) lt min(str_mod.mlat) then begin
   			tmp=min(mlat,loc) 
   			mlat[loc]=min(str_mod.mlat)
   		endif
   		if max(mlat) gt max(str_mod.mlat) then begin
   			tmp=max(mlat,loc) 
   			mlat[loc]=max(str_mod.mlat)
   		endif
   	lon_ave=avg(mlon)
   	lat_ave=avg(mlat)
   	lon=mlon & lat=mlat & magellanic=1
   endelse

	if keyword_set(wham) then radius=sqrt(1./!dpi)

	if (NOT keyword_set(ndiv)) then $
		num=fix(dist_angle(lon[0],lat[0],lon[1],lat[1],/angle,magellanic=magellanic)/(radius+1.)) else $
		num=ndiv

	padding=0.25
	lon_arr=findgen(num)*(lon[0]-lon[1]+padding*2.)/num+min([lon[1],lon[0]])-padding
	
	fit=linfit([lon[0],lon[1]],[lat[0],lat[1]])
	lat_arr=fit[1]*lon_arr+fit[0]


	offset=fltarr(num)
	
	offset_str=replicate(str_mod[0],num)
	remove_tags,offset_str,['N','NAME'],tmp,/quiet
	offset_str=tmp
	
	for i=0,num-1 do begin
	
		offset[i]=sign(lon_ave-lon_arr[i])*dist_angle(lon_ave,lat_ave,lon_arr[i],lat_arr[i],/angle,magellanic=magellanic)
	
		if keyword_set(smallest) then begin
			if (NOT keyword_set(radius)) then radius=0.01
			subindex=-1 & dist=0
			while total(subindex) eq -1 do begin
				dist=radius+dist+0.01
				subindex=spectnear(str_mod,lon_arr[i],lat_arr[i],dist,magellanic=magellanic)
			endwhile
		endif else begin
			if (NOT keyword_set(radius)) then radius=0.01
			subindex=spectnear(str_mod,lon_arr[i],lat_arr[i],radius,magellanic=magellanic)
			if total(subindex) eq -1 then begin
				print,''
				print,'*** Not enough points to average. ***'
				print,'*** Try a larger radius. ***'
				print,''
				return
			endif
		endelse

		tmp=sparith(str_mod[subindex],/ave)
		remove_tags,tmp,['N','NAME'],tmp2,/quiet
		offset_str[i]=tmp2
	
	endfor

	str_mod=offset_str

endif

	if (NOT keyword_set(factor)) then factor=0.

	if keyword_set(error) then variable='var' else variable='data'
	tag_loc=where(strcmp(tag_names(str_mod),VARIABLE,/fold_case) eq 1)

		if (n_elements(vrange) ne 2) then $
			yrange=[min(str_mod.vel),max(str_mod.vel)] else $
			yrange=[min(vrange),max(vrange)]

		good_vel=where((str_mod[0].vel ge yrange[0]) AND (str_mod[0].vel le yrange[1]),count)

		num=n_elements(str_mod)
		if (NOT keyword_set(offset)) then xrange=[0,num] $
			else xrange=[min(offset),max(offset)]

		image=fltarr(num,count)
		image=(str_mod.(tag_loc))[good_vel,*]
		if keyword_set(error) then image=sqrt(image)
		
		if count eq 0 then begin
			print,''
			print,'*** Velocity range too small ***'
			print,'*** vmin: ',min(str_mod.vel)
			print,'*** vmax: ',max(str_mod.vel)
			print,''
			return
		endif	

		if keyword_set(min) then minvalue=min else minvalue=min(image)
		if keyword_set(max) then maxvalue=max else maxvalue=max(image)
		
		if (NOT keyword_set(lon)) AND (NOT keyword_set(lat)) then xtitle='index' else $
			xtitle='Angular Offset (Degrees)'

		@kat_color
		axis_format = {Xminor:2, Yminor:2}
		image=rotate(image,1)
		cgimage,image,top=256-1-2,/axis,$
			minvalue=minvalue,maxvalue=maxvalue,AXKEYWORDS=axis_format,$
			xrange=xrange,yrange=yrange,$
			ytitle='LSR Velocity (km s!U-1!N)',$
			xtitle=xtitle,/save

		position=[!x.window[0],!y.window[0],!x.window[1],!y.window[1]]

		;Check to make sure that the variable opath is defined and that it is a structure
		if (size(opath,/type) eq 8) then begin
			tag_x=where(strcmp(tag_names(opath),'x',/fold_case) eq 1)
			tag_y=where(strcmp(tag_names(opath),'y',/fold_case) eq 1)
			;make sure that the x and y tags exist
			if (tag_x[0] ne -1) AND (tag_y[0] ne -1) then begin
				;plot,[1,2,3],xrange=xrange,yrange=yrange,$
				;	xstyle=1,ystyle=1,$
				;	position=position,/nodata,/noerase
				for i=0,n_elements(opath)-1 do $
					plots,*opath[i].x,*opath[i].y,color=fsc_color('black')
			endif
		endif

		if keyword_set(PATH) OR keyword_set(level) then begin


			;For some reason, the 'save' option in contour isn't passing
			;the xrange and yrange values to !x.crange and !y.crange propertly.
			;So instead, of using /PATH_DATA_COORDS in contour as this only yeilds 
			;the index positions of the contours, I'm using the normalized 
			;poistions and converting them to data coordinants here and passing
			;x and y to contour:
            s=Size(image, /Dimensions)
            x = cgScaleVector(Findgen(s[0]), xrange[0], xrange[1])
			y = cgScaleVector(Findgen(s[1]), yrange[0],yrange[1])

 			CONTOUR, image, POSITION=position,level=level,/noerase,$
 			xstyle=13,ystyle=13,$
 			path_xy=path,path_info=info,/PATH_DATA_COORDS,x,y

 			add_tag,info,'x',ptr_new(),tmp1
 			add_tag,tmp1,'y',ptr_new(),tmp2

 			newpath=tmp2

 			loc_low=0
 			for i=0,n_elements(info)-1 do begin
 				loc_high=total(info[0:i].n)-1
 				*newpath[i].x=path[0,loc_low:loc_high]
 				*newpath[i].y=path[1,loc_low:loc_high]
 				loc_low=loc_high+1
 			endfor

 			;for i=0,n_elements(path)-1 do plots,*path[i].x,*path[i].y,color=fsc_color('green')
 			path=newpath

 			position=[!x.window[0],!y.window[0],!x.window[1],!y.window[1]]
 			CONTOUR, image, POSITION=position,level=level,/noerase,$
 			xstyle=13,ystyle=13

 		endif

		factor_string=strcompress(string(factor),/re)

	if (NOT keyword_set(title)) then begin
		if keyword_set(column) AND keyword_set(error) AND (factor ne 0.) then $
			title='HI Column Density Error (10!U'+factor_string+'!N cm!U-2!N)' else $
		if keyword_set(column) AND keyword_set(error) AND (factor eq 0.) then $
			title='HI Column Density Error (cm!U-2!N)' else $
		if keyword_set(column) AND (factor ne 0.) then $
			title='HI Column Density (10!U'+factor_string+'!N cm!U-2!N)' else $
		if keyword_set(column) AND (factor eq 0.) then $
			title='HI Column Density (cm!U-2!N)' else $
		if keyword_set(wham) AND keyword_set(error) AND (factor ne 0.) then $
			title='H'+cgsymbol('alpha')+' Intensity Error (10!U'+factor_string+'!N R)' else $
		if keyword_set(wham) AND keyword_set(error) AND (factor eq 0.) then $
			title='H'+cgsymbol('alpha')+' Intensity Error (R)' else $
		if keyword_set(wham) AND (factor ne 0.) then $
			title='H'+cgsymbol('alpha')+' Intensity (10!U'+factor_string+'!N R)' else $
		if keyword_set(wham) AND (factor eq 0.) then $
			title='H'+cgsymbol('alpha')+' Intensity (R)' else $
			title=''
	endif

		range=[minvalue,maxvalue]/10.^(factor)
		@kat_color
		colorbar,/vertical,ncolors=256-1-2,top=256-1-2,/right,$
		POSITION=[0.91, 0.1, 0.94, 0.9],$
		title=title,$
		yrange=range,charsize=!p.charsize*0.8

;yTICKINTERVAL=2













end