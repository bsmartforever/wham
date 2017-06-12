function galex2hst, flux, wave

; Purpose:
; 	Converts flux units given by GALEX to those used
; 	in the exposure time calculator for COS/HST.
; 	Convert micro Janskys to (erg cm^-2 s^-1 Angstrom^-1)
;	
;	Note that read_galex supplies FUV in micro Janskys
;
; Created by Dr. Kat Barger 03/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

If (NOT keyword_set(wave)) then wave = 1550.

	flux_out=double(3.33e-19*(wave)^2.)^(-1)*flux*1e-29

	return, flux_out

end