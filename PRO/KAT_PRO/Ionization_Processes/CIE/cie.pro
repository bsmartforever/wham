function cie,ion1,ion2,depleted=depleted,phase=phase,plot=plot,$
	yrange=yrange,ylog=ylog,xrange=xrange,line=line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: To calculate the ion column density ratio for collisional ionizaiton equalibrium 
;	    conditions as a function of electron temperature. 
;
; 	Input:
;		ion1 - A string specifying the ion to extract ion fractions for. e.g., 'HI' or 'CIV'
; 		ion2 - A string specifying the ion to extract ion fractions for. e.g., 'HI' or 'CIV'
;		depletion - Applies Jankins 2009 solar depletion patterns. These are gas phase dependent.
;		phase - String specifying the gas phase. 'wim' is the default value. 
;			    Allowed values: 'wim', 'wnm', 'cnm', 'molecular' 
;				Note: This is only relevent if depletion=1
;
;		plot - plots the ion fractions and column density ratio as a function of temperature.
;		yrange - sets the yrange of the column density ratio plot, if plot is set.
;
;	Output: structure containing a temperature and ion column density ratio array 
;
;	Example: ratio=cie('CIV','SIV')
;	IDL> help,ratio,/str
;	** Structure <158f188>, 2 tags, length=1608, data length=1608, refs=1:
;	   T               FLOAT     Array[201]
;	   RATIO           FLOAT     Array[201]
;	
; Note: The ion fraction values of the CIE are independent of both electron density 
;       and metallicity. The ion fractions ONLY depend on the temperature of the gas.
; 		The relative abundance of each ion does still matter when calculating the 
;		column density, but the overall metallicity term cancels out. Only the
;		relative depletion term remains. 
;
; Note: Model assuming dust-free and optically thin conditions with no external radiation.
;
; Referenes: Gnat & Sternberg 2007 ApJS, 168, 213 for the CIE model
;			 Jenkins 2009 for deplention patterns (only if depletion is set to 1)
;			 Asplund M., Grevesse N., Sauval A.J. & Scott P. (2009, ARAA, 47, 481) for anundances 
;
; By Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

quiet_save = !quiet
!quiet = 1

;This is really annoying, but abundance and depletion aren't 
;compliling correctly, even though they are in the idl path,
;with proper naming convention. depletion has to be compiled 
;first or else abundance wont compile propertly. 
RESOLVE_ROUTINE, 'depletion', /IS_FUNCTION
RESOLVE_ROUTINE, 'abundance', /IS_FUNCTION

;Make sure ion1 is a string
if size(ion1,/type) ne 7 then begin
	print,''
	print,'*** ion1 must be a string ***'
	print,''
	return,1.0
endif 

;Make sure ion2 is a string and make sure it exists
if (size(ion2,/type) ne 7) AND (size(ion2,/type) ne 0) then begin
	print,''
	print,'*** ion2 must be a string ***'
	print,''
	return,1.0
endif

dir=file_dirname((routine_info('cie',/function,/source)).path)+'/'
;dir='$HOME/PRO/KAT_PRO/Ionization_Processes/CIE/' 

readcol,dir+'CIE_Gnat.txt',$
		T,HI,HII,HeI,HeII,HeIII,CI,CII,CIII,CIV,$
		CV,CVI,CVII,NI,NII,NIII,NIV,NV,NVI,NVII,$
		NVIII,OI,OII,OIII,OIV,OV,OVI,OVII,OVIII,OIX,$
		NeI,NeII,NeIII,NeIV,NeV,NeVI,NeVII,NeVIII,NeIX,NeX,$
		NeXI,MgI,MgII,MgIII,MgIV,MgV,MgVI,MgVII,MgVIII,MgIX,$
		MgX,MgXI,MgXII,MgXIII,SiI,SiII,SiIII,SiIV,SiV,SiVI,$
		SiVII,SiVIII,SiIX,SiX,SiXI,SiXII,SiXIII,SiIXV,SiXV,SI,$
		SII,SIII,SIV,SV,SVI,SVII,SVIII,SIX,SX,SXI,$
		SXII,SXIII,SIXV,SXV,SXVI,SXVII,FeI,FeII,FeIII,FeIV,$
		FeV,FeVI,FeVII,FeVIII,FeIX,FeX,FeXI,FeXII,FeXIII,FeIXV,$
		FeXV,FeXVI,FeXVII,FeXVIII,FeIXX,FeXX,FeXXI,FeXXII,FeXXIII,FeIXXV,$
		FeXXV,FeXXVI,FeXXVII,/silent

H=HI+HII
He=HeI+HeII+HeIII
C=CI+CII+CIII+CIV+CV+CVI+CVII
N=NI+NII+NIII+NIV+NV+NVI+NVII+NVIII
O=OI+OII+OIII+OIV+OV+OVI+OVII+OVIII+OIX
;Not Ne as that's is 'NOT EQUAL TO' in IDL
Neon=NeI+NeII+NeIII+NeIV+NeV+NeVI+NeVII+NeVIII+NeIX+NeX+NeXI  
Mg=MgI+MgII+MgIII+MgIV+MgV+MgVI+MgVII+MgVIII+MgIX+$
		MgX+MgXI+MgXII+MgXIII
Si=SiI+SiII+SiIII+SiIV+SiV+SiVI+$
		SiVII+SiVIII+SiIX+SiX+SiXI+SiXII+SiXIII+SiIXV+SiXV
S=SI+SII+SIII+SIV+SV+SVI+SVII+SVIII+SIX+SX+SXI+$
		SXII+SXIII+SIXV+SXV+SXVI+SXVII
Fe=FeV+FeVI+FeVII+FeVIII+FeIX+FeX+FeXI+FeXII+FeXIII+FeIXV+$
		FeXV+FeXVI+FeXVII+FeXVIII+FeIXX+FeXX+FeXXI+FeXXII+FeXXIII+FeIXXV+$
		FeXXV+FeXXVI+FeXXVII

if n_elements(SCOPE_VARFETCH(ion1, /ENTER,level=0)) eq 0 then begin
   print,' '
   print,'*** Variable name ',ion1,' does not exist!!! ***'
   print,' '
   return,0
endif 

;Check to see if ion2 is defined
if (size(ion2,/type) ne 0) then begin
	if (n_elements(SCOPE_VARFETCH(ion2, /ENTER,level=0)) eq 0) then begin
	   print,' '
	   print,'*** Variable name ',ion2,' does not exist!!! ***'
	   print,' '
	   return,0
	endif
endif

;Set to depletion to zero if not set, essentially assuming no dust
if (NOT keyword_set(depletion)) then depletion=0.
;Set to WIM depletion pattern if not set
if (NOT keyword_set(phase)) OR (size(phase,/type) ne 7) then phase='wim'

;Although metallicity is an option in abundance, this essentially is canceled out in the ratio
;Note that in realtity, different metallicity will effect the cooling and hence the ratios, 
;but the CIE_Gnat.txt table is only good for Z=1.0

element1=strsplit(ion1,'I,V,X',/extract)
abundance1=abundance(element1,depleted=depleted,phase=phase,/quiet)
;ion ionfraction for numerator 
top=SCOPE_VARFETCH(ion1, /ENTER,level=0)*abundance1[0]

;If ion2 doesn't exist, then calculate the column density for ion1
if (size(ion2,/type) eq 0) then begin
	NH=10.0^20.0
	if z lt 1. then z=10.^z 
	column1=top*abundance1*z*NH

	if (NOT keyword_set(ylog)) then yminor=1
	if keyword_set(plot) then $
		plot,T,column1,$
		/xlog,ylog=ylog,xrange=xrange,yrange=yrange,$
		yminor=yminor,$
		xtitle='log T/K',ytitle='N '+ion1+' cm!U-2!N'

	return,{T:T,column:column1}
endif 

element2=strsplit(ion2,'I,V,X',/extract)
abundance2=abundance(element2,depleted=depleted,phase=phase,/quiet)
;ion ionfraction for denominator 
bottom=SCOPE_VARFETCH(ion2, /ENTER,level=0)*abundance2[0]

;Only select ion fraction values where data exists
non_zero=where((top ne 0) AND (bottom ne 0))
top=top[non_zero]
bottom=bottom[non_zero]
T=T[non_zero]

ratio=top/bottom

good_values=WHERE(FINITE(top/bottom))
infinity_values=WHERE(FINITE(top/bottom,/infinity),num_infinity)

if keyword_set(plot) then begin
	orig_pos=!p.position
	!p.position=[0.15,(0.95-0.15)/2.0+0.15,0.95,0.95]

	if (NOT keyword_set(ylog)) then ymin=0 $
	else begin
		ymin=0.01
		ytickformat='exponent_zero_one'
		;ytickformat='exponent_zero_one'
	endelse
	ymax=max([max(ratio[good_values])])
	if (NOT keyword_set(yrange)) AND (NOT keyword_set(ylog)) then yrange=[ymin,ymax*1.25] $
	else if (NOT keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=[ymin,ymax*10.] $
	else if (keyword_set(yrange)) AND (keyword_set(ylog)) then yrange=10^float(yrange)

	if keyword_set(xrange) then begin
		xrange=float(xrange)
		if xrange[0] lt 10 then xrange=10^xrange
	endif

	plot,T,ratio,/xlog,ytitle='N '+ion1+' / '+'N '+ion2,charsize=1.5,$
		xtickname=replicate(' ',10),yrange=yrange,ystyle=1,xrange=xrange,$
		xstyle=1,/nodata,ytickformat=ytickformat
	oplot,T[good_values],ratio[good_values]

	if keyword_set(line) then $
	for i=0, n_elements(line)-1 do oplot,10^!x.crange,[1.,1.]*line[i],linestyle=1

	if num_infinity ne 0 then $
	oplot,T[infinity_values],ymax*(fltarr(n_elements(infinity_values))+1),psym=symcat(48),symsize=2
	plots,10^!x.crange,[1,1]*max(ratio[good_values]),linestyle=1
	oplot,10^!x.crange,[1,1]*min(ratio[good_values]),linestyle=1
	xyouts,!p.position[2]*0.825,!p.position[3]*0.85,'Max: '+string(max(ratio[good_values]),format='(e8.2)'),charsize=1.25,/norm
	xyouts,!p.position[2]*0.825,!p.position[3]*0.80,'Min: '+string(min(ratio[good_values]),format='(e8.2)'),charsize=1.25,/norm

	!p.position=[0.15,0.15,0.95,(0.95-0.15)/2.0+0.15]	
	ymin=min([min(bottom),min(top)])*0.75
	ymax=max([max(bottom),max(top)])*1.25
	plot,T,top,/xlog,xtitle='log(T/K)',ytitle='Ion Frac',charsize=1.5,/noerase,/nodata,$
		yrange=[ymin,ymax],ystyle=1,/ylog,xrange=xrange,xstyle=1
	oplot,T,top,color=fsc_color('orchid')
	oplot,T,bottom,color=fsc_color('forest green')

	legend,[ion1,ion2],linestyle=[0,0],color=[fsc_color('orchid'),fsc_color('forest green')],box=0,/right,/center,charsize=1.25
	!p.position=orig_pos
endif

!quiet = quiet_save 

return,{T:T,ratio:ratio}

end