function PI,ion1,ion2,$
	plot=plot,xrange=xrange,yrange=yrange,ylog=ylog,line=line,$
	quiet=quiet

;Reference: Knauth et al., 2003, ApJ, 592, 964
;Assumes solar, anders98 (Model 1 of drf) and MW WIM, anders98 depleted metallicities (Models 2-4 of drf and HII region)
;	Linearly corrects the metallicities from anders98 to asplund09
;hii.txt: This HII region model is what Lehner 2011 refers to as hot stars
;drf.txt: This diffuse radiation field model is what Lehner 2011 refers to as a hot plasma 

dir=file_dirname((routine_info('pi',/function,/source)).path)+'/'
;dir='$HOME/PRO/KAT_PRO/Ionization_Processes/Photoionization/'

drf_file='diffuse_radiation_field/drf.txt'
hii_file='hii/hii.txt'

if ((ion1 eq 'CIV') OR (ion1 eq 'SiIV')) AND ((ion2 eq 'CIV') OR (ion2 eq 'SiIV')) then begin
;Linear correct ratio from anders98 to asplund09
	readcol,dir+hii_file,T,CIV_SiIV,skipline=1,/silent
	CIV_SiIV=CIV_SiIV*abundance('C',/asplund09,/quiet)/abundance('Si',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('Si',/anders98,/quiet))^(-1.0)
	if (ion1 eq 'CIV') AND (ion2 eq 'SiIV') then begin
		ratio=replicate({T_hii:T[0],hii:CIV_SiIV[0],T_drf:0.0,drf:0.0},4)
		ratio[0:2].T_hii=T & ratio[0:2].hii=CIV_SiIV 
	endif else begin
		ratio=replicate({T_hii:T[0],hii:(CIV_SiIV[0])^(-1.),T_drf:0.0,drf:0.0},4)
		ratio[0:2].T_hii=T & ratio[0:2].hii=(CIV_SiIV)^(-1.) 
	endelse
endif else ratio=replicate({T_drf:0.0,drf:0.0},4)

readcol,dir+drf_file,T,CIV_SiIV,CIV_NV,CIV_OIV,CIV_AlIII,CIV_FeIII,AlIII_FeIII,/silent

;Linear correct ratio from anders98 to asplund09
CIV_SiIV=CIV_SiIV*abundance('C',/asplund09,/quiet)/abundance('Si',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('Si',/anders98,/quiet))^(-1.0)
CIV_NV=CIV_NV*abundance('C',/asplund09,/quiet)/abundance('N',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('N',/anders98,/quiet))^(-1.0)
CIV_OIV=CIV_OIV*abundance('C',/asplund09,/quiet)/abundance('O',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('O',/anders98,/quiet))^(-1.0)
CIV_AlIII=CIV_AlIII*abundance('C',/asplund09,/quiet)/abundance('Al',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('Al',/anders98,/quiet))^(-1.0)
CIV_FeIII=CIV_FeIII*abundance('C',/asplund09,/quiet)/abundance('Fe',/asplund09,/quiet)*(abundance('C',/anders98,/quiet)/abundance('Fe',/anders98,/quiet))^(-1.0)
AlIII_FeIII=AlIII_FeIII*abundance('C',/asplund09,/quiet)/abundance('Fe',/asplund09,/quiet)*(abundance('Al',/anders98,/quiet)/abundance('Fe',/anders98,/quiet))^(-1.0)

ratio.T_drf=T

if ion1 eq 'CIV' then begin
	if ion2 eq 'SiIV' then ratio.drf=CIV_SiIV else $
	if ion2 eq 'NV' then ratio.drf=CIV_NV else $ 
	if ion2 eq 'OIV' then ratio.drf=CIV_OIV else $ 
	if ion2 eq 'AlIII' then ratio.drf=CIV_AlIII else $ 
	if ion2 eq 'FeIII' then ratio.drf=CIV_FeIII else $ 
	if ion2 eq 'NV' then ratio.drf=CIV_NV else begin
		if (NOT keyword_set(quiet)) then begin
			print,''
			print,'    *** No ion found. Allowed ions: ***'
			print,'*** CIV, SiIV, NV, OIV, AlIII, and FeIII ***'
			print,''
		endif
		return,0
	endelse
endif else if ion2 eq 'CIV' then begin
	if ion1 eq 'SiIV' then ratio.drf=(CIV_SiIV)^(-1.) else $
	if ion1 eq 'NV' then ratio.drf=(CIV_NV)^(-1.) else $ 
	if ion1 eq 'OIV' then ratio.drf=(CIV_OIV)^(-1.) else $ 
	if ion1 eq 'AlIII' then ratio.drf=(CIV_AlIII)^(-1.) else $ 
	if ion1 eq 'FeIII' then ratio.drf=(CIV_FeIII)^(-1.) else $ 
	if ion1 eq 'NV' then ratio.drf=(CIV_NV)^(-1.) else begin
		if (NOT keyword_set(quiet)) then begin
			print,''
			print,'    *** No ion found. Allowed ions: ***'
			print,'*** CIV, SiIV, NV, OIV, AlIII, and FeIII ***'
			print,''
		endif
		return,0
	endelse
endif else if (ion1 eq 'AlIII') AND (ion2 eq 'FeIII') then ratio.drf=(AlIII_FeIII) $
else if (ion2 eq 'AlIII') AND (ion2 eq 'FeIII') then ratio.drf=(AlIII_FeIII)^(-1.) else begin
	if (NOT keyword_set(quiet)) then begin
		print,''
		print,'    *** No ion found. Allowed ions: ***'
		print,'*** CIV, SiIV, NV, OIV, AlIII, and FeIII ***'
		print,''
	endif
	return,0
endelse

return,ratio

end