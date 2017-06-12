function abundance,element,z,$
	depleted=depleted,phase=phase,$
	anders98=anders98,asplund09=asplund09,$
	feldman92=feldman92,anders92=anders92,grevess98=grevess98,$
	wilms00=wilms00,lodders03=lodders03, help=help,quiet=quiet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; +
; Purpose: To return the abundance of a specified element. 
;
; Input:
; 	element - specifies which element to determine the abundance for.
;				Case insensitive. 
;				e.g., element='C' returns the carbon abundance. 
;	
; 	[Optional]
;	z - specifies the percentatge of solar metalicity to return
;				e.g., z=0.5 is for half solar metalicity
;	depleted   - If set, reduce abundance by the solar depletion pattern of 
;				  Jenkins 2009, see Draine ISM book table 9.5 on pg. 91
;	phase 		- Specifies the phase for the depletion pattern.
;				  If not set, 'wim' is assumed. 
;				  options: wim, wnm, cnm, molecular
;				  only values for C, N, O, Na, Mg, Al, Si, S, Ca, Ti, Fe, Ni are availible
;
;	NOTE: If no abundance set keyword is specified, assume Asplund et al., 2009
;	NOTE: See /help for references
;
;   NOTE: Total column density of an element equals Nx,total=NH,total*abundance
;
;	NOTE: Uncertanties in abundances are not included
;
; Created by Dr. Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;This makes sure that funciton depletion is compiled before running.
RESOLVE_ROUTINE, 'depletion', /IS_FUNCTION

if keyword_set(help) then begin
	print,'function abundance,element,z'
	print,''
	print,'Keywords to call up different abundace sets'
	print,'anders98 - Anders E. & Grevesse N. (1989, Geochimica et Cosmochimica Acta 53, 197)'
	print,'asplund09 - Asplund M., Grevesse N., Sauval A.J. & Scott P. (2009, ARAA, 47, 481)'
	print,'feldman92 - Feldman U.(1992, Physica Scripta 46, 202 except for elements not listed which are given grsa abundances)'
	print,'anders92 - Anders E. & Ebihara (1982, Geochimica et Cosmochimica Acta 46, 2363)'
	print,'grevess98 - Grevesse, N. & Sauval, A.J. (1998, Space Science Reviews 85, 161)'
	print,'wilms00 - Wilms, Allen & McCray (2000, ApJ 542, 914 except for elements not listed which are given zero abundance)'
	print,'lodders03 - Lodders, K (2003, ApJ 591, 1220)'
	return,0
endif

if size(element,/type) ne 7 then begin
	print,''
	print,'*** element must be a string ***'
	print,'*** function abundance,element ***'
	print,''
	return,0
endif 

if (NOT keyword_set(z)) then z=1.0 
if (z lt 0.0) then z=10.^(z)

readcol,'$HOME/PRO/KAT_PRO/abundances/abundances.txt',$
	elements,angr,aspl,feld,neb,grsa,wilm,lodd,$
	format='A,F,F,F,F,F,F,F',skipline=9,/silent

loc_arr=strmatch(elements,element,/fold_case)
loc=where(loc_arr eq 1,count)

if count eq 0 then begin
	print,''
	print,'*** no element found ***'
	print,'*** try again ***'
	print,''
	return,0
endif 

if (keyword_set(depleted)) OR (keyword_set(phase)) then depletion_factor=depletion(element,phase,/quiet) $
	else depletion_factor=1.0

if (NOT keyword_set(quiet)) then print,'Abundance of',elements[loc]

if keyword_set(anders98) then return,(angr[loc])[0]*z[0]*depletion_factor[0]
if keyword_set(feldman92) then return,(feld[loc])[0]*z[0]*depletion_factor[0]
if keyword_set(anders92) then return,(neb[loc])[0]*z[0]*depletion_factor[0]
if keyword_set(grevess98) then return,(grsa[loc])[0]*z[0]*depletion_factor[0]
if keyword_set(wilms00) then return,(wilm[loc])[0]*z[0]*depletion_factor[0]
if keyword_set(lodders03) then return,(lodd[loc])[0]*z[0]*depletion_factor[0]
;If no abundance set is specified, use Asplund et al., 2009
asplund09=1
if keyword_set(asplund09) then return,(aspl[loc])[0]*z[0]*depletion_factor[0]

end