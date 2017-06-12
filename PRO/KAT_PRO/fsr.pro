; function fsr, line, obs=obs
;
; Purpose: To determine pressure difference of the free spectral range for switching orders while tuning. 
;          Switching orders is both chamber dependent and wavelength dependent. Remember, you'll want to 
;          switch orders if pressure in chamber A exceeds 200 or if pressure in chamber B exceeds 225. 
;          Pressures above these values can damage the etalons. 
;
; Line: name of the line of interest. Expects a string.
;       obs='kpno' accepts 'ha','hb', 'sii', 'nii', 'oiii', 'hei', 'blue_nii'
;       obs='ctio' accepts 'ha','hb', 'sii', 'nii', 'oi', 'oii', 'oiii', 'hei', 'blue_nii'
;
;       note that the fsr difference between kpno and ctio is small, so using ctio for 'oi' and 'oii'
;       for obs='kpno' is probably fine and will get you pretty close. 
;
; Examples:
;
;   IDL> test=fsr('ha',obs='ctio')
;   FSR Chamber A:        69.652303
;   FSR Chamber B:        162.83640
;
;   IDL> test=fsr('ha',obs='kpno')
;   FSR Chamber A:        69.652037
;   FSR Chamber B:        162.84786
;
;   IDL> test=fsr('hb',obs='ctio')
;   FSR Chamber A:        51.568227
;   FSR Chamber B:        120.56915
;

FUNCTION OPT_INDEX, l, P, Space = Space

  ;; Returns optical index as a fun. of l,P
  ;; Applies translation shift 
  ;; This is Q=1/(2*n*l)
  ;; with l=0.0471 cm (spacing of chamber A)
  ;; the other l is lambda...

  ;;  Spacings from Tufte PhD
  ;;  Space = 0.0471  ;; Spacing in Chamber A [cm]
  ;;  Space = 0.0201  ;; Spacing in Chamber B [cm]

  ;; Spacings from 'best fit'
  ;; Space = 0.04707
  ;; Space = 0.02013

  Result = 1.0/(2*Space*DOUBLE(N_NON_LIN(P) + N_WAVE(l)-N_NON_LIN(76.0))) 
  RETURN, Result[0]

END


function fsr, line, obs=obs

if NOT keyword_set(obs) then print,"Specify obs=ctio or obs=kpno"

if keyword_set(help) then begin
   print,''
   print,'final_wavelength=wavelength(center_velocity,wavelength=rest_wavelength,filter=filter,help=help)'
   print,'H-alpha is the default rest wavelength'
   print,' '
   print,'Rest wavelengths [Angstroms]:'
   print,'H-beta: 4861.3'
   print,'[OIII]: 5006.9'
   print,'HeI: 5875.6'
   print,'[NII]_blue: 5754.6'
   print,'[OI]: 6300.3'
   print,'H-alpha: 6562.8'
   print,'[NII]: 6583.4'
   print,'[SII]: 6716.4'
   print,'[OII]: 7320.0'
endif


spaceA=0.04707
spaceB=0.02013

  obs = keyword_set(obs) ? obs : 'ctio'
IF obs EQ 'kpno' THEN BEGIN
	CASE line OF
		'hb': BEGIN
			lambda=4861.3 & P_0=[146.0, 150.0] & line='Hb'
		END
		'oiii': BEGIN
			lambda=5006.9 & P_0=[109.1, 160.6] & line='[O III]'
		END
		'blue_nii': BEGIN
			lambda=5754.6 & P_0=[126.6, 80.6] & line='blue [N II]'
		END
		'hei': BEGIN
			lambda=5875.6 & P_0=[105.4, 136.4] & line='He I'
		END
		'ha': BEGIN
			lambda=6562.8 & P_0=[94.7, 107.2] & line='Ha'
		END
		'nii': BEGIN
			lambda=6583.4 & P_0=[131.5, 97.5] & line='[N II]'
		END
		'sii': BEGIN
			lambda=6716.4 & P_0=[86.4, 123.4] & line='[S II]'
		END
	ELSE: message, 'Unknown line ', line
	ENDCASE
ENDIF ELSE IF obs EQ 'ctio' THEN BEGIN
    readcol, '/d/wham/pro/calibration/tunes.txt', filter, l_arr, pa, pb, $
        format='A,F,F,F', silent = silent
    idx = where(line EQ filter)
    IF array_equal(idx, -1) THEN message, 'Unknown line ' + line
    lambda = l_arr[idx]
    P_0 = [pa[idx], pb[idx]]
endif



     FSR_A = (Opt_Index(lambda, P_0[0], Space = spaceA) * 3.0e5 / (1/(lambda*1.0e-8)) )/3.0
     FSR_B = (Opt_Index(lambda, P_0[1], Space = spaceB) * 3.0e5 / (1/(lambda*1.0e-8)) )/3.0

print,'FSR Chamber A: ',FSR_A
print,'FSR Chamber B: ',FSR_B

return,[FSR_A,FSR_B]

end
