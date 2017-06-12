function z,NHI,NX,ion,depleted=depleted,phase=phase,log=log

; Purpose:
;
; Inputs:
;	NHI - HI Column Density. Error optional. 
;			E.g., column or [column, column_err]
;	NX  - Ion Column Density. Error optional. 
;			E.g., column or [column, column_err]
;	ion - Ion element name as string.
;			E.g., 'O' for Oxygen
;
; Outputs:
;	The 'metallicity' of the ion with respect to HI in log units, e.g., [X/HI].
;	
;
; Optional:
;	depleted - Use ISM depletion patterns of Jenkins 2009 [Default phase='wim']
;	phase    - Specifies which gas phase depletion patterns to use.
;				E.g., 'wim', 'wnm', 'cnm', or 'molecular' [Default 'wim']
;	log      - Passed column densities are in logrithmic units. 
;			  	If column densities are less than 100., log units are assumed. 
;
; Created by Dr. Kat Barger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (NHI[0] le 100.) OR keyword_set(log) then begin
		if (n_elements(NHI) eq 2) then NHI[1]=log_err(NHI[0],NHI[1],/reverse)
		NHI[0]=10.^NHI[0]
	endif
	if (NX[0] le 100.) OR keyword_set(log) then begin
		if (n_elements(NX) eq 2) then NX[1]=log_err(NX[0],NX[1],/reverse)
		NX[0]=10.^NX[0]
	endif

	metals=alog10(NX[0]/NHI[0])-alog10(abundance(ion,phase=phase,depleted=depleted,/quiet))

	if (n_elements(NHI) eq 2) AND (n_elements(NX) eq 2) then begin
	
		dNHI=0.434/NHI[0]
		dNX=0.434/NX[0]
	
		dmetals=sqrt((dNX*NX[1])^2.+(dNHI*NHI[1])^2.)*abs(metals)

		return, [metals, dmetals]

	endif

	return, metals

end