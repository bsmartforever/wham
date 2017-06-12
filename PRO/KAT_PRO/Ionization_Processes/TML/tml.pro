function tml,ion1,ion2,solar=solar,depleted=depleted,slow=slow,fast=fast,$
	plot=plot,xrange=xrange,yrange=yrange,ylog=ylog,line=line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: To calculate the ion column density ratio for turbulant mixing layer conditions.  
;
; 	Input:
;		ion1 - A string specifying the ion to extract column densities for. 
; 		ion2 - A string specifying the ion to extract column densities for.
;			   Only valid ions: CIV, NV, OVI, and SiIV
;
;		[Optional]:
;		solar - Specifies solar metallicities
;		depleted - Specifies metallicities depleted from solar. 
;			   Note: These depletions and metallicities are intrinsic to the model. 
;		slow - Specifies non-thermal gas velocity of 25 km/s
;		fast - Specifies non-thermal gas velocity of 100 km/s
;		
;
;		plot - plots the ion fractions and column density ratio as a function of temperature.
;		yrange - sets the yrange of the column density ratio plot, if plot is set.
;
; Model: Hot gas flowing past a sheet of cold or warm gas. Rayleigh-Taylor instabilities cause 
;		 the hot gas to punture through the cold sheet and create bubbles. Shear flows causes 
;		 Kelvin-Helmhold instabilities that grow into layers of turbulent mixing across the 
; 		 interface region. 
;
; Note: This model basically assumes Milky Way type conditions, but it's the model available. 
;
; Note: Model depends on two quantities: 1) the temperature attained by the gas immediately after 
;		mixing and 2) the ionization parameter in the layer due to self-radiation. 
;
; Referenes: Slavin, Shull, and Begelman, ApJ, 1993, 497, 83
;
; By Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


allowed_ions=['CIV','NV','OVI','SiIV']
if total(strmatch(allowed_ions,ion1)) eq 0 then begin
   print,' '
   print,'*** Variable name ',ion1,' does not exist!!! ***'
   print,'*** Allowed ions: CIV, NV, OVI, and SiIV ***'
   print,' '
   return,0
endif
if total(strmatch(allowed_ions,ion2)) eq 0 then begin
   print,' '
   print,'*** Variable name ',ion2,' does not exist!!! ***'
   print,'*** Allowed ions: CIV, NV, OVI, and SiIV ***'	
   print,' '
   return,0
endif

dir=file_dirname((routine_info('tml',/function,/source)).path)+'/'
;dir='$HOME/PRO/KAT_PRO/Ionization_Processes/TML/'
file_solar_slow='solar/v25.txt'
file_solar_fast='solar/v100.txt'
file_depleted_slow='depleted/v25.txt'
file_depleted_fast='depleted/v100.txt'

if (NOT keyword_set(slow)) AND (NOT keyword_set(fast)) AND $
   (NOT keyword_set(solar)) AND (NOT keyword_set(depleted)) then begin
   files=[file_solar_slow,file_solar_fast,file_depleted_slow,file_depleted_fast] 
   slow=1
   fast=1
   solar=1
   depleted=1
endif else if ((keyword_set(solar)) AND (NOT keyword_set(slow)) AND (NOT keyword_set(fast))) OR $
	((keyword_set(solar)) AND (keyword_set(slow)) AND (keyword_set(fast))) then begin
	files=[file_solar_slow,file_solar_fast] 
	slow=1
   	fast=1
endif else if ((keyword_set(depleted)) AND (NOT keyword_set(slow)) AND (NOT keyword_set(fast))) OR $
	((keyword_set(depleted)) AND (keyword_set(slow)) AND (keyword_set(fast))) then begin
	files=[file_depleted_slow,file_depleted_fast] 
	slow=1
   	fast=1
endif else if (keyword_set(solar)) AND (NOT keyword_set(depleted)) AND (keyword_set(slow)) AND (NOT keyword_set(fast)) then begin
	files=[file_solar_slow] 
endif else if (keyword_set(solar)) AND (NOT keyword_set(depleted)) AND (keyword_set(fast)) AND (NOT keyword_set(slow)) then begin
	files=[file_solar_fast]	
endif else if (keyword_set(depleted)) AND (NOT keyword_set(solar)) AND (keyword_set(slow)) AND (NOT keyword_set(fast)) then begin
	files=[file_depleted_slow] 
endif else if (keyword_set(depleted)) AND (NOT keyword_set(solar)) AND (keyword_set(fast)) AND (NOT keyword_set(slow)) then begin
	files=[file_depleted_fast]	
endif else if ((keyword_set(slow)) AND (NOT keyword_set(solar)) AND (NOT keyword_set(depleted))) OR $
	((keyword_set(slow)) AND (keyword_set(solar)) AND (keyword_set(depleted))) then begin
   files=[file_solar_slow,file_depleted_slow] 
endif else if ((keyword_set(fast)) AND (NOT keyword_set(solar)) AND (NOT keyword_set(depleted))) OR $
	((keyword_set(fast)) AND (keyword_set(solar)) AND (keyword_set(depleted))) then begin
   files=[file_solar_fast,file_depleted_fast]
endif 

num_files=n_elements(files)

all_ratios=replicate({name:'',T:fltarr(3),ratio:dblarr(3)},num_files)

for i=0,num_files-1 do begin 
	;These are log of temperature and log of column density values
	readcol,dir+files[i],T,CIV,NV,OVI,SiIV,/silent
	all_ratios[i].name=files[i]
	all_ratios[i].T=10^T
	all_ratios[i].ratio=10.^double(SCOPE_VARFETCH(ion1, /ENTER,level=0))/10.^double(SCOPE_VARFETCH(ion2, /ENTER,level=0))
endfor

if keyword_set(plot) then begin

	orig_p=!p
	orig_x=!x
	orig_y=!y		
	!p.thick=3
	!p.charthick=2
	!p.charsize=1.25
	!p.symsize=2
	!x.thick=2
	!y.thick=2

	!p.position=[0.15,0.15,0.95,0.90]

	ymax=max(all_ratios.ratio)
	if (NOT keyword_set(ylog)) then ymin=0 $
	else begin
		ymin=min(all_ratios.ratio)
		ytickformat='exponent_zero_one'
		;ytickformat='exponent_zero_one'
	endelse
	if (NOT keyword_set(yrange)) AND (NOT keyword_set(ylog)) then yrange=[ymin,ymax*1.25] $
	else if (NOT keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=[ymin,ymax] $
	else if (keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=10^float(yrange)
	
	if keyword_set(xrange) then begin
		xrange=float(xrange)
		if xrange[0] lt 10 then xrange=10^xrange
	endif

	plot,all_ratios[0].T,all_ratios[0].ratio,$
		xtitle='log(T/K)',ytitle='N '+ion1+' / '+'N '+ion2,$
		xrange=xrange,xstyle=xstyle,$
		yrange=yrange,ystyle=ystyle,ylog=ylog,ytickformat=ytickformat,$
		charsize=1.5,charthick=charthick,$
		/nodata

		if (where(all_ratios.name eq file_solar_slow) ne -1.) then $
			oplot,all_ratios[where(all_ratios.name eq file_solar_slow)].T,$
				all_ratios[where(all_ratios.name eq file_solar_slow)].ratio,$
				psym=-symcat(14,thick=!p.thick,color=fsc_color('blu7')),symsize=!p.symsize,$
				thick=!p.thick,linestyle=0,color=fsc_color('blu4')
		if (where(all_ratios.name eq file_solar_fast) ne -1.) then $
			oplot,all_ratios[where(all_ratios.name eq file_solar_fast)].T,$
				all_ratios[where(all_ratios.name eq file_solar_fast)].ratio,$
				psym=-symcat(4,thick=!p.thick,color=fsc_color('blu7')),symsize=!p.symsize,$
				thick=!p.thick,linestyle=1,color=fsc_color('blu4')
		if (where(all_ratios.name eq file_depleted_slow) ne -1.) then $
			oplot,all_ratios[where(all_ratios.name eq file_depleted_slow)].T,$
				all_ratios[where(all_ratios.name eq file_depleted_slow)].ratio,$
				psym=-symcat(52,thick=!p.thick,color=fsc_color('dark orchid')),symsize=!p.symsize,$
				thick=!p.thick,linestyle=0,color=fsc_color('orchid')
		if (where(all_ratios.name eq file_depleted_fast) ne -1.) then $
			oplot,all_ratios[where(all_ratios.name eq file_depleted_fast)].T,$
				all_ratios[where(all_ratios.name eq file_depleted_fast)].ratio,$
				psym=-symcat(51,thick=!p.thick,color=fsc_color('dark orchid')),symsize=!p.symsize,$
				thick=!p.thick,linestyle=1,color=fsc_color('orchid')

		if keyword_set(line) then $
		for i=0, n_elements(line)-1 do oplot,!x.crange,[1.,1.]*line[i],linestyle=1

		legend,['25 km s!U-1!N Solar','100 km s!U-1!N Solar','25 km s!U-1!N Depleted Solar','100 km s!U-1!N Depleted Solar'],$
		linestyle=[0,1,0,1],color=[fsc_color('blu7'),fsc_color('blu7'),fsc_color('dark orchid'),fsc_color('dark orchid')],$
		charsize=!p.charsize,charthick=!p.charthick,thick=!p.thick,box=0

;file_solar_slow='solar/v25.txt'
;file_solar_fast='solar/v100.txt'
;file_depleted_slow='depleted/v25.txt'
;file_depleted_fast='depleted/v100.txt'


	!p=orig_p
	!x=orig_x
	!y=orig_y

endif


return,all_ratios

end