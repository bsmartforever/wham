function extinct_corr, wave=wave, line=line, glon=glon, glat=glat, vrange=vrange, $
	     column=column, radius=radius, EBV=EBV, lmc=lmc, mw=mw

; k_lambda => A(lambda)/E(B-V)
;
;

if (NOT keyword_set(column)) then begin
	;Calculate HI column density from LAB survey
	
	;set to WHAM beam size if unset. 
	if (NOT keyword_set(radius)) then radius=sqrt(1./!pi)
	if (NOT keyword_set(vrange)) and keyword_set(mw) then vrange=[-150.,150]
	if (NOT keyword_set(vrange)) and keyword_set(lmc) then vrange=[200.,400.]
	vrange=[min(vrange),max(vrange)]
	
	if (n_elements(glon) eq 0) OR (n_elements(glat) eq 0) then begin
		print,'*** Please specify glon & glat or column ***'
		print,'*** NO correction calculated ***'
		return,1
	endif
	spectra=hi_spectra(glon,glat,radius=radius)
	
	good_vel=where((spectra.vel ge vrange[0]) and (spectra.vel le vrange[1]))
	
	column=int_tabulated(spectra.vel[good_vel],spectra.data[good_vel])*1.8224e18
	
endif

if (keyword_set(lmc)) then begin

	if (NOT keyword_set(wave)) then begin
		print,'*** Please specify wave ***'
		print,'*** NO correction calculated ***'
		return,1
	endif

	if (NOT keyword_set(Rv)) then Rv=3.41
	x=1./(wave*1e-10/1e-6)

	c1=-0.890
	c2=0.998
	c3=2.719
	c4=0.400
	x0=4.579
	gamma=0.934

	D=x^2./((x^2.-x0^2.)^2.+x^2.*gamma^2.)
	if x lt 5.9 then F=0 else F=0.5392*(x-5.9)^2.+0.05644*(x-5.9)^3.

	ExV_EBV=c1+c2*x+c3*D+c4*F

	Av_column=1./3.25e21
	Av=Av_column*column

	Ax=Av*ExV_EBV*(1./Rv+1)

	print,column,Ax,x
	return,exp(Ax/2.5)

endif


If keyword_set(mw) then begin

	;Cardelli et al., 1989 for a diffuse ISM
	lambda=6562.8
	IF keyword_set(line) THEN BEGIN
		CASE line OF
			'hb': BEGIN
				lambda=4861.3 &  line='Hb'
			END
			'oiii': BEGIN
				lambda=5006.9 &  line='[O III]'
			END
			'blue_nii': BEGIN
				lambda=5754.6 & line='blue [N II]'
			END
			'hei': BEGIN
				lambda=5875.6 &  line='He I'
			END
			'oi': BEGIN
				lambda=6300.3 &  line='[O I]'
			END
			'ha': BEGIN
				lambda=6562.8 & line='Ha'
			END
			'nii': BEGIN
				lambda=6583.4 & line='[N II]'
			END
			'sii': BEGIN
				lambda=6716.4 & line='[S II]'
			END
			'oii': BEGIN
				lambda=7320.0 &  line='[O II]'
			END
		ELSE: message, 'Unknown line ', line, ' Assuming H-alpha'
		ENDCASE
	    wave=lambda
	ENDIF ELSE IF (NOT keyword_set(line)) AND (NOT keyword_set(wave)) then begin
		print,'*** wave or line not specified ***'
		print,'    *** Assuming H-alpha ***'
		wave=lambda
	endif
    	

	if (NOT keyword_set(Rv)) then Rv=3.1
	x=1./(wave*1e-10/1e-6)
	y=(x-1.82)
	
	a=1.+0.17699*y-0.50447*y^2.-0.02427*y^3.+0.72085*y^4.+0.01979*y^5.-0.77530*y^6.+0.32999*y^7.
	b=1.41338*y+2.28305*y^2.+1.07233*y^3.-5.38434*y^4.-0.62251*y^5.+5.30260*y^6.-2.09002*y^7.
	
	Ax_column=(Rv*a+b)/4.93e21
	
	Ax=Ax_column*column
	
	return,exp(Ax/2.5)

endif


end