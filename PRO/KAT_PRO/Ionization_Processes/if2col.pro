function if2col,ionfrac1,element1,ionfrac2,element2,phase=phase,depletion=depletion,log=log

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: To determine the ion column density ratio from ion fractions.
;
; Input:
;
;	ionfrac1 - The fraction of ions in an ionization state
;	ionfrac2 - The fraction of ions in an ionization state
;	element1 - Specifies which element ionfrac1 is for. Used to determine the abundance.
;				e.g., 'C' or 'N'
;	element2 - Specifies which element ionfrac2 is for. Used to determine the abundance.
;				e.g., 'C' or 'N'
;
; [Optional]:
;	phase - specified the gas phase. phase='wim' is assumed if not specified. Used to 
;			determine the depletion pattern.
;			Allowed values include 'wim', 'wnm', 'cnm', 'molecules'
;	depletion - specifies whether or not depletion patterns should be used.
;			Allowed values are 0 and 1
;			Note: depletion patterns are from Janskins 2009
;	log - specifies that ionfrac1 and ionfrac2 are in units of -alog10.
;			This is assumed if either ionfrac1 or ionfrac2 is greater than 1.
;			Note: many ionization process tables list their ion fractions in these units
;			Note: the negative in front of "alog10" is intentional
;
;	NOTE: Ion fractions produced for CIE are independent of metallicity and electron density.
;		  These values only depend on electron temperature. The resultant column density ratios
;		  will only depend on the relative abundances of each ion and their depletion patterns, 
;		  they will not depend on a global metallicity value as that factor will cancel out. 
; 
; By Dr. Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if n_params() eq 0 then begin
	print,''
	print,'   Usage:'
	print,'   ionfrac2colratio,ionfrac1,element1,ionfrac2,element2,$''
	print,'      phase=phase,depletion=depletion,log=log'
	print,''
	return,0
endif

if size(element1,/type) ne 7 then begin
	print,''
	print,'*** element1 must be a string ***'
	print,''
	return,1.0
endif 

if size(element1,/type) ne 7 then begin
	print,''
	print,'*** element1 must be a string ***'
	print,''
	return,1.0
endif 

if (max(ionfrac1) ge 1.0) OR (max(ionfrac2) ge 1.0) then log=1
if keyword_set(log) then begin
	ionfrac1=10^(-ionfrac1)
	ionfrac2=10^(-ionfrac2)
endif


;Set to depletion to zero if not set, essentially assuming no dust
if (NOT keyword_set(depletion)) then depletion=0.
;Set to WIM depletion pattern if not set
if (NOT keyword_set(phase)) then phase='wim'

abundance1=abundance(element1,depletion=depletion,phase=phase,/quiet)
abundance2=abundance(element2,depletion=depletion,phase=phase,/quiet)

	return,(ionfrac1*abundance1[0])/(ionfrac2*abundance2[0])

end