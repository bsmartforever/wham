function rebin_reduce_res, str, bin_factor=bin_factor, $
         res_factor=res_factor, tag=tag, STIS2COS=STIS2COS,$
         quiet=quiet
;
; Purpose - To rebin smooth the velocity resolution and binning of 
;			HST data such that STIS data is smoothed to match COS data.
;
; str - Structure, expected to contain vel, wave, flux, norm, column
; bin_factor - old velocity bin size / new velocity bin size
; res_factor - sqrt((new vel resolution)^2 - (old vel resolution)^2)
;
;
; Created by Dr. Kat Barger 07/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;modified structure
str_mod=str

;Assume smoothing from STIS velocity resolution to COS
if (NOT keyword_set(res_factor)) OR (keyword_set(STIS2COS)) then begin
	;velocity resoution
	COS_width=17.0
	STIS_width=6.5
	broadening_factor=sqrt(abs(COS_width^2.0-STIS_width^2.0))	
	if str_mod.instrument ne 'COS' then begin
		print,' '
		print,'*** Warning: Smoothing from STIS resolution to COS ***'
		print,'*** Structure lists '+str_mod.instrument+', not COS ***'
		print,' '
	endif
endif else if keyword_set(res_factor) then broadening_factor=res_factor $
else broadening_factor=1.

;Smooth fluxes to the same velocity resolution
tag_loc=where(strcmp(tag_names(str_mod),'flux',/fold_case) eq 1,check)
if check ne 0 then begin
	mod_flux=gaussfold(str_mod.vel,str_mod.(tag_loc),broadening_factor) 
	;Replace the old flux element in str_mod with the smoothed flux.
	str_mod=mod_str(str_mod,'flux',mod_flux)
endif else begin
	print,' '
	print,'*** Structure contains no flux tag ***'
	print,'*** Returning unmodified structure ***'
	print,' '
	return,str
endelse & undefine,check

;Apply velocity rebin
;COS velocity spacing: 7.2 km/s
;HST velocity spacing: 3.2 km/s

cos_bin=7.2
hst_bin=3.2

tag_loc=where(strcmp(tag_names(str_mod),'vel',/fold_case) eq 1,check)
if check eq 0 then begin
	print,' '
	print,'*** Structure contains no flux tag ***'
	print,'*** Returning unmodified structure ***'
	print,' '
	return,str
endif & undefine,check

num=n_elements(str_mod.(tag_loc))
if (NOT keyword_set(bin_factor)) or (keyword_set(STIS2COS)) then $
	new_num=num*(hst_bin/cos_bin) $
else new_num=num*bin_factor
vel_new=frebin(str_mod.(tag_loc),new_num)

tag_loc=where(strcmp(tag_names(str_mod),'var',/fold_case) eq 1,check) & if check ne 0 then variance=1 & undefine,check
tag_loc=where(strcmp(tag_names(str_mod),'err',/fold_case) eq 1,check) & if check ne 0 then error=1 & undefine,check

;tags to rebin
tags=['vel','wave','flux','column','continuum']
str_mod=binstr_interpol(str_mod,vel_new,'vel',tags,variance=variance,error=error)

;Must recalculate apparent column densities

column=3.768e14*alog((str_mod.continuum)/(str_mod.flux))/(str_mod.wavc*str_mod.fval)
   ;Replace all -NaN values with 0
   bad_index=where(finite(column) eq 0.0,bad_values)
   vel=str_mod.vel
   if bad_values ne 0 then remove,bad_index,column,vel ;column[bad_index]=0.0

;Replace the column density in str_mod with the new column density.

str_mod=mod_str(str_mod,'column',column)
str_mod=mod_str(str_mod,'vel',vel)

return, str_mod

end 