function depletion,element,phase,wim=wim,wnm=wnm,cnm=cnm,molecular=molecular,quiet=quiet

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; +
; Purpose: To return the depletion pattern of a specifed element in a 
;		   specified phase. Based on Jenkins 2009. 
;          See Draine ISM book table 9.5, page 91 for details.
;
; Input:
; 	element - String specifying which element to determine the depletion for.
;				Case insensitive. 
;				e.g., element='C' returns the carbon depletion. 
;	phase   - String specifying the phase for the depletion pattern.
;				If not set, 'wim' is assumed. 
;				options: wim, wnm, cnm, molecular
;				only values for C, N, O, Na, Mg, Al, Si, S, Ca, Ti, Fe, Ni are availible
;
;	Note: Set the depletion of oxygen in the wim to zero. The trend in Jenkins 2009 would yield 
;		  an unphysical value that enhances oxygen in this gas phase. This is really just an 
;		  unrealistic extrapolation of a trend to F* = -1, a region where they have no observational 
;		  data to confirm. 
;
;	Reference: Jenkins 2009
;
; Created by Dr. Kat Barger 08/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

compile_opt hidden

if size(element,/type) ne 7 then begin
	print,''
	print,'*** element must be a string ***'
	print,'*** function abundance,element ***'
	print,''
	return,1.0
endif 

if (NOT keyword_set(phase)) AND (NOT keyword_set(wnm)) AND $
   (NOT keyword_set(cnm)) AND (NOT keyword_set(molecular)) then phase='wim'
if keyword_set(wnm) then phase='wnm'
if keyword_set(cnm) then phase='cnm'
if keyword_set(molecular) then phase='molecular'

if size(phase,/type) ne 7 then begin
	print,''
	print,'*** phase must be a string ***'
	print,'*** function abundance,element ***'
	print,''
	return,1.0
endif 

readcol,'$HOME/PRO/KAT_PRO/abundances/depletion.txt',$
	elements,solar,wim_arr,wnm_arr,cnm_arr,molecular_arr,$
	format='A,F,F,F,F,F',/silent

loc_arr=strmatch(elements,element,/fold_case)
loc=where(loc_arr eq 1,count)

if count eq 0 then begin
	print,''
	print,'*** no element found ***'
	print,'*** only values for C, N, O, Na, Mg, Al, Si, S, Ca, Ti, Fe, Ni availible ***'
	print,'*** try again ***'
	print,''
	return,1.0
endif 

wim_arr=double(wim_arr)/double(solar)
wnm_arr=double(wnm_arr)/double(solar)
cnm_arr=double(cnm_arr)/double(solar)
molecular_arr=double(molecular_arr)/double(solar)

if (NOT keyword_set(quiet)) then print,'Depletion of',elements[loc]

if (phase eq 'wim')  OR (phase eq 'WIM') then return,(wim_arr[loc])[0]
if (phase eq 'wnm')  OR (phase eq 'WNM') then return,(wnm_arr[loc])[0]
if (phase eq 'cnm')  OR (phase eq 'CNM') then return,(cnm_arr[loc])[0]
if (phase eq 'molecular') OR (phase eq 'MOLECULAR') OR (phase eq 'Molecular') then return,(molecular_arr[loc])[0]

print,''
print,'       *** Please specify a valid phase ***'
print,'*** Valid phases are WIM, WNM, CNM, and molecular ***'
print,'               *** Returning 0 ***'

end