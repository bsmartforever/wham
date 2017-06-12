function ionization, ion1, ion2, z=z, depleted=depleted, phase=phase,$
	temperature=temperature,time=time,$
	ratio=ratio, color=color, line=line,$
	plot=plot,yrange=yrange,xrange=xrange,ylog=ylog,$
	thick=thick,symsize=symsizeymsize,symthick=symthick ;,charsize=charsize,charthick=charthick

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: To calculate the ion column density ratio for multiple ionizaiton conditions, including 
;		collisional ionzation equilibrium (CIE) and time dependent non equilibrium collisional 
;		ionizaiton (NECI) conditions for both isobaric and isochoric, and turbulant mixing layers,
;		as a function of electron temperature. 
;
; 	Input:
;		ion1 - A string specifying the ion to extract ion fractions for. e.g., 'HI' or 'CIV'
; 		ion2 - A string specifying the ion to extract ion fractions for. e.g., 'HI' or 'CIV'
;		z - Metallicity. Specifies which NECI table to use as ion fractions depend on metallicity.
;			   Valid metallicities include -3, -2, -1, 1, 2
;
;		For NECI: 
;       isochoric - constant density
;		isobaric - constant pressure
;
;		[Optional]:
;		depletion - Applies Jankins 2009 MW solar depletion patterns. These are gas phase dependent.
;		phase - String specifying the gas phase. 'wim' is the default value. 
;			    Allowed values: 'wim', 'wnm', 'cnm', 'molecular' 
;				Note: This is only relevent if depletion=1
;
;		plot - plots the ion fractions and column density ratio as a function of temperature.
;		yrange - sets the yrange of the column density ratio plot, if plot is set.
;
;	Note: TML model is ony applicable to the CIV, NV, OVI, and SiIV ions.
;
; Referenes: Gnat & Sternberg 2007 ApJS, 168, 213 for the CIE and NECI models
;			 Gnat & Sternberg 2009 ApJ, 693, 151 for IS model
;			 Slavin, Shull, and Begelman, ApJ, 1993, 497, 83 for TML model
;			 Knauth et al., 2003, ApJ, 592, 964 for photoionization model
;			 Jenkins 2009 for deplention patterns (only if depletion is set to 1)
;			 Asplund M., Grevesse N., Sauval A.J. & Scott P. (2009, ARAA, 47, 481) for anundances 
;
; By Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

quiet_save = !quiet
!quiet = 1

RESOLVE_ROUTINE, 'depletion', /IS_FUNCTION
RESOLVE_ROUTINE, 'abundance', /IS_FUNCTION

if size(ion1,/type) ne 7 then begin
	print,''
	print,'*** ion1 must be a string ***'
	print,''
	return,1.0
endif 

if size(ion2,/type) ne 7 then begin
	print,''
	print,'*** ion2 must be a string ***'
	print,''
	return,1.0
endif

if (NOT keyword_set(phase)) OR (size(phase,/type) ne 7) then phase='wim'

;Assume solar metallicity if not specified
if (NOT keyword_set(z)) then z=1
if keyword_set(z) then begin
	CASE z OF 
	   2: file='z2.txt'
	   1: file='z1.txt'
	   -1: file='z-1.txt' 
	   -2: file='z-2.txt'
	   -3: file='z-3.txt'
	ELSE: file=' '
	ENDCASE 
endif
if file eq ' ' then begin
	print,' '
	print,'*** Valid metallicities include -3, -2, -1, 1, 2 ***'
	print,' '
	return,0
endif

CIE=cie(ion1,ion2,depleted=depleted,phase=phase)
isochoric=NECI(ion1,ion2,z=z,depleted=depleted,phase=phase,/isochoric)
isobaric=NECI(ion1,ion2,z=z,depleted=depleted,phase=phase,/isobaric)
si_noB=si(ion1,ion2,z=z,depleted=depleted,phase=phase,temperature=temperature,time=time)
si_strongB=si(ion1,ion2,z=z,depleted=depleted,phase=phase,temperature=temperature,b=1,time=time)

;problem 1: number of elements in CIE array is larger than isochoric and isobaric models
;pronlem 2: the isochoric and isobaric arrays are backwards
;ynew=interpol(yold,xold,xnew)
isobaric_ratio=interpol(reverse(isochoric.ratio),reverse(isochoric.T),CIE.T)
isbaric_ratio=interpol(reverse(isobaric.ratio),reverse(isobaric.T),CIE.T)
si_noB_ratio=interpol(reverse(si_noB.ratio),reverse(si_noB.T),CIE.T)
si_strongB_ratio=interpol(reverse(si_strongB.ratio),reverse(si_strongB.T),CIE.T)

isochoric={T:cie.T,ratio:isobaric_ratio}
isobaric={T:cie.T,ratio:isbaric_ratio}
si_noB={T:cie.T,ratio:si_noB_ratio}
si_strongB={T:cie.T,ratio:si_strongB_ratio}

photo=PI(ion1,ion2,/quiet)

allowed_ions=['CIV','NV','OVI','SiIV']
if (total(strmatch(allowed_ions,ion1)) eq 1) AND (total(strmatch(allowed_ions,ion2)) eq 1) AND (size(photo,/type) eq 8) then begin
	if ((ion1 eq 'CIV') OR (ion1 eq 'SiIV')) AND ((ion2 eq 'CIV') OR (ion2 eq 'SiIV')) then begin
		tml=tml(ion1,ion2)
		all_ratios={T:cie.T,cie:cie.ratio,neci_isochoric:isochoric.ratio,neci_isobaric:isobaric.ratio,$
				si_noB:si_noB.ratio,si_strongB:si_strongB.ratio,$
				T_tml:tml[0],tml_solar_slow:tml[0].ratio,tml_solar_fast:tml[1].ratio,$
				tml_deplete_slow:tml[2].ratio,tml_deplete_fast:tml[3].ratio,$
				T_HII:photo.T_HII,HII:photo.HII,T_DRF:photo.T_DRF,DRF:photo.drf} 
	endif else begin 
		tml=tml(ion1,ion2)
		all_ratios={T:cie.T,cie:cie.ratio,neci_isochoric:isochoric.ratio,neci_isobaric:isobaric.ratio,$
				si_noB:si_noB.ratio,si_strongB:si_strongB.ratio,$
				T_tml:tml[0],tml_solar_slow:tml[0].ratio,tml_solar_fast:tml[1].ratio,$
				tml_deplete_slow:tml[2].ratio,tml_deplete_fast:tml[3].ratio,$
				T_DRF:photo.T_DRF,DRF:photo.drf} 
	endelse
endif else if (total(strmatch(allowed_ions,ion1)) eq 1) AND (total(strmatch(allowed_ions,ion2)) eq 1) then begin
	tml=tml(ion1,ion2)
	all_ratios={T:cie.T,cie:cie.ratio,neci_isochoric:isochoric.ratio,neci_isobaric:isobaric.ratio,$
			si_noB:si_noB.ratio,si_strongB:si_strongB.ratio,$
			T_tml:tml[0],tml_solar_slow:tml[0].ratio,tml_solar_fast:tml[1].ratio,$
			tml_deplete_slow:tml[2].ratio,tml_deplete_fast:tml[3].ratio}
endif else if (size(photo,/type) eq 8) then $
all_ratios={T:cie.T,cie:cie.ratio,neci_isochoric:isochoric.ratio,neci_isobaric:isobaric.ratio,$
			si_noB:si_noB.ratio,si_strongB:si_strongB.ratio,$
			T_HII:photo.T_HII,HII:photo.HII,T_DRF:photo.T_DRF,DRF:photo.drf} $
else $
all_ratios={T:cie.T,cie:cie.ratio,neci_isochoric:isochoric.ratio,neci_isobaric:isobaric.ratio,$
			si_noB:si_noB.ratio,si_strongB:si_strongB.ratio}

if keyword_set(plot) then begin
	orig_p=!p
	orig_x=!x
	orig_y=!y		
	if (NOT keyword_set(thick)) AND (!d.name ne 'PS') then !p.thick=3 else $
	if (NOT keyword_set(thick)) AND (!d.name eq 'PS') then !p.thick=5 
	!p.charthick=2
	if !d.name eq 'X' then begin
		!x.thick=2 
		!y.thick=2 
	endif else begin
		!x.thick=5 
		!y.thick=5 
	endelse	
	if (NOT keyword_set(symsize)) then symsize=2

	if !d.name eq 'X' then !p.position=[0.15,0.15,0.95,0.90]

	T=cie.T

	good_CIE=WHERE(FINITE(cie.ratio))
	infinity_CIE=WHERE(FINITE(cie.ratio,/infinity),num_cie_infinity)
	
	good_isochoric=WHERE(FINITE(isochoric.ratio))
	infinity_isochoric=WHERE(FINITE(isochoric.ratio,/infinity),num_isochoric_infinity)
	
	good_isobaric=WHERE(FINITE(isobaric.ratio))
	infinity_isobaric=WHERE(FINITE(isobaric.ratio,/infinity),num_isobaric_infinity)

	good_si_noB=WHERE(FINITE(si_noB.ratio))
	infinity_si_noB=WHERE(FINITE(si_noB.ratio,/infinity),num_si_noB_infinity)

	good_si_strongB=WHERE(FINITE(si_strongB.ratio))
	infinity_si_strongB=WHERE(FINITE(si_strongB.ratio,/infinity),num_si_strongB_infinity)

	if (NOT keyword_set(ylog)) then ymin=0 $
	else begin
		ymin=0.01
		ytickformat='exponent_zero_one'
	endelse
	ymax=max([max(cie.ratio[good_CIE]),max(isochoric.ratio[good_isochoric]),max(isobaric.ratio[good_isobaric]),$
			  max(si_noB.ratio[good_si_noB]),max(si_strongB.ratio[good_si_strongB])])
	if (NOT keyword_set(yrange)) AND (NOT keyword_set(ylog)) then yrange=[ymin,ymax*1.25] $
	else if (NOT keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=[ymin,ymax*10.] $
	else if (keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=10^float(yrange)

	if keyword_set(xrange) then begin
		xrange=float(xrange)
		if xrange[0] lt 10 then xrange=10^xrange
	endif

	if (NOT keyword_set(ylog)) then yminor=2

	plot,T,cie.ratio,/xlog,ytitle='N '+ion1+' / '+'N '+ion2,$
		xtitle='Electron Temperature (K)',$
		;title='z = '+strcompress(metallicity,/re),$
		charsize=1.5,yrange=yrange,ystyle=1,/nodata,$
		xrange=xrange,xstyle=1,ylog=ylog,ytickformat=ytickformat,$
		yminor=yminor

	dim=size(ratio,/dim)
	if (dim[0] lt 2.) AND keyword_set(ratio) then begin
		print,'*** Please specify ratio and uncertanty ***'
	endif else if (dim[0] ge 2.) AND keyword_set(ratio) then begin

		if (NOT keyword_set(color)) then $
			color=[fsc_color('blk4'),fsc_color('PUR4'),fsc_color('TAN7'),$
				   fsc_color('GRN3'),fsc_color('BLU3'),fsc_color('ORG2')]

		for i=0, n_elements(ratio[0,*])-1 do begin
			polyfill,10^[!x.crange[0],!x.crange[1],!x.crange[1],!x.crange[0]],$
					 [ratio[0,i]-ratio[1,i],ratio[0,i]-ratio[1,i],ratio[0,i]+ratio[1,i],ratio[0,i]+ratio[1,i]],$
					 color=color[i],/noclip
			oplot,10^!x.crange,[1.,1.]*ratio[0,i],thick=!p.thick,linestyle=2 
		endfor
	endif



	oplot,T[good_cie],cie.ratio[good_cie],color=fsc_color('Hot Pink')
	max_cie=max(cie.ratio[good_CIE])
	if num_cie_infinity ne 0 then $
	oplot,T[infinity_cie],max_cie*(fltarr(n_elements(infinity_cie))+1),$
		psym=symcat(48,thick=!p.thick-1),symsize=symsize,color=fsc_color('Hot Pink')

	oplot,T[good_isochoric],isochoric.ratio[good_isochoric],color=fsc_color('forest green')
	max_isochoric=max(isochoric.ratio[good_isochoric])
	if num_isochoric_infinity ne 0 then $
	oplot,T[infinity_isochoric],max_isochoric*(fltarr(n_elements(infinity_isochoric))+1),$
		psym=symcat(48,thick=!p.thick-1),symsize=symsize,color=fsc_color('forest green')

	oplot,T[good_isobaric],isobaric.ratio[good_isobaric],color=fsc_color('Aquamarine')
	max_isobaric=max(isobaric.ratio[good_isobaric])
	if num_isobaric_infinity ne 0 then $
	oplot,T[infinity_isobaric],max_isobaric*(fltarr(n_elements(infinity_isobaric))+1),$
		psym=symcat(48,thick=!p.thick-1),symsize=symsize,color=fsc_color('Aquamarine')

	oplot,T[good_si_noB],si_noB.ratio[good_si_noB],color=fsc_color('orange')
	max_si_noB=max(si_noB.ratio[good_si_noB])
	if num_si_noB_infinity ne 0 then $
	oplot,T[infinity_si_noB],max_si_noB*(fltarr(n_elements(infinity_si_noB))+1),$
		psym=symcat(48,thick=!p.thick-1),symsize=symsize,color=fsc_color('orange')

	oplot,T[good_si_strongB],si_strongB.ratio[good_si_strongB],color=fsc_color('brown')
	max_si_strongB=max(si_strongB.ratio[good_si_strongB])
	if num_si_strongB_infinity ne 0 then $
	oplot,T[infinity_si_strongB],max_si_strongB*(fltarr(n_elements(infinity_si_strongB))+1),$
		psym=symcat(48,thick=!p.thick-1),symsize=symsize,color=fsc_color('brown')

	if keyword_set(line) then $
		for i=0, n_elements(line)-1 do oplot,10^!x.crange,[1.,1.]*line[i],linestyle=1,thick=!p.thick+2

	if keyword_set(depletion) then depleted_string=', depleted'	else depleted_string=''
	legend,['CIE'+depleted_string,$
			'NECI isochoric, z = '+strcompress(string(z),/re)+depleted_string,$
			'NECI isobaric, z = '+strcompress(string(z),/re)+depleted_string,$
			'SI no B, z = '+strcompress(string(z),/re)+depleted_string,$
			'SI strong B, z = '+strcompress(string(z),/re)+depleted_string],$
		linestyle=[0,0,0,0,0],$
		color=[fsc_color('Hot Pink'),fsc_color('forest green'),fsc_color('Aquamarine'),$
		fsc_color('orange'),fsc_color('brown')],box=0,/right,/bottom,charsize=1.2


	;TML models
	if (total(strmatch(allowed_ions,ion1)) eq 1) AND (total(strmatch(allowed_ions,ion2)) eq 1) then begin

		file_solar_slow='solar/v25.txt'
		file_solar_fast='solar/v100.txt'
		file_depleted_slow='depleted/v25.txt'
		file_depleted_fast='depleted/v100.txt'

		oplot,tml[where(tml.name eq file_solar_slow)].T,$
			tml[where(tml.name eq file_solar_slow)].ratio,$
			psym=-symcat(14,thick=!p.thick,color=fsc_color('blu7')),symsize=symsize,$
			thick=!p.thick,linestyle=0,color=fsc_color('blu4')
		oplot,tml[where(tml.name eq file_solar_fast)].T,$
			tml[where(tml.name eq file_solar_fast)].ratio,$
			psym=-symcat(4,thick=!p.thick,color=fsc_color('blu7')),symsize=symsize,$
			thick=!p.thick+2,linestyle=1,color=fsc_color('blu4')
		oplot,tml[where(tml.name eq file_depleted_slow)].T,$
			tml[where(tml.name eq file_depleted_slow)].ratio,$
			psym=-symcat(52,thick=!p.thick,color=fsc_color('dark orchid')),symsize=symsize,$
			thick=!p.thick,linestyle=0,color=fsc_color('orchid')
		oplot,tml[where(tml.name eq file_depleted_fast)].T,$
				tml[where(tml.name eq file_depleted_fast)].ratio,$
				psym=-symcat(51,thick=!p.thick,color=fsc_color('dark orchid')),symsize=symsize,$
				thick=!p.thick+2,linestyle=1,color=fsc_color('orchid')

		if keyword_set(line) then $
		for i=0, n_elements(line)-1 do oplot,!x.crange,[1.,1.]*line[i],linestyle=1

		legend,['TML:','25 km s!U-1!N Solar','100 km s!U-1!N Solar',$
				'25 km s!U-1!N Depleted Solar','100 km s!U-1!N Depleted Solar'],$
			linestyle=[0,0,1,0,1],thick=[0,0,2,0,2]+!p.thick,$
			color=[!p.background,fsc_color('blu7'),fsc_color('blu7'),$
			fsc_color('dark orchid'),fsc_color('dark orchid')],$
			charsize=1.25,charthick=!p.charthick,box=0,/top,/left

	endif

	;photoionization models
	if (size(photo,/type) eq 8) then begin
		T_min=min(photo([0,1,3]).T_drf) ;Third model assumes 10x normal x-ray flux
		T_max=max(photo([0,1,3]).T_drf) ;Third model assumes 10x normal x-ray flux

		;size of arrow, if linear scale. This is one-tenth of the plotting window
		asize=(!y.crange[1]-!y.crange[0])*0.1

		pos=convert_coord(10^(!x.crange[0]),max(photo([0,1,3]).drf),/to_norm)
		plots,[pos[0],pos[0]+0.125],pos[1]*[1.,1.],linestyle=0,/norm,thick=!p.thick*2.0

		   ;convert coordinants to normal units. Then plot downwards arrow one-tenth the window size. 
		   tmp=convert_coord(avg([T_min,T_max]),max(photo([0,1,3]).drf),/to_norm)
		   if !d.name eq 'PS' then hsize=500 else hsize=10
		   arrow,pos[0]+0.0625,tmp[1],pos[0]+0.0625,tmp[1]-0.05,/norm,thick=!p.thick,hthick=!p.thick,hsize=hsize

		   xyouts,pos[0]+0.0625,tmp[1]+0.015,'   PI: star + wind',/norm,align=0.5,charsize=1.1

		if ((ion1 eq 'CIV') OR (ion1 eq 'SiIV')) AND ((ion2 eq 'CIV') OR (ion2 eq 'SiIV')) then begin
			pos=convert_coord(10^(!x.crange[0]),max(photo([0,1,3]).hii),/to_norm)
			plots,[pos[0],pos[0]+0.125],pos[1]*[1.,1.],linestyle=0,/norm,thick=!p.thick*2.0
	
			   ;convert coordinants to normal units. Then plot downwards arrow one-tenth the window size. 
			   tmp=convert_coord(10000.,max(photo([0,1,3]).hii),/to_norm)
			   arrow,pos[0]+0.0625,tmp[1],pos[0]+0.0625,tmp[1]-0.05,/norm,thick=!p.thick,hthick=!p.thick,hsize=hsize
	
	   		   xyouts,pos[0]+0.0625,tmp[1]+0.015,' PI: stars',/norm,align=0.5,charsize=1.1
	    endif

	endif

	!p=orig_p
	!x=orig_x
	!y=orig_y
endif

!quiet = quiet_save 

	return,all_ratios

end