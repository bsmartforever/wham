function find_ions, variable, ions=ions, wave=wave, low=low, high=high, flag=flag
;
; Purpose - To through active spectra structures and extract the indices that match 
;           specified ions and wavelengths. This is different than find_ion_order, 
;           which takes a specified ions and wave list and extracts the indices that 
;           would order variable to that input. This program instead spits the indices 
;           out in no particular. However, this program is more flexible in the ability 
;           quickly extract all spectra of a given ion as wave is optional.
;
; Variable - Either a string or a structure.
;	     Structure to pass line properties, such as integrated quantities,
;               equivalent widths, line centers of each component, 
;               associated uncertainties, flags, and notes, etc. 
;            String array to pass variable names of the data structures
;               containing vel, wave, flux, normalized flux, 
;               apparent column densities, ion, and wavelength, etc.
;               that are active variables in the main level, a.k.a., level=1.
; ions     - String array of ions to locate, e.g., ['CII','SII'] 
; wave     - Number array of wavelengths to locate, e.g., [1238,1334]
;
; e.g., Finds locations with ion 'SiII'
;       loc=find_ions(CAL_F_variables,ions=['SiII'])
;       In this case 4 indices are returned.
; e.g., loc=find_ions(CAL_F_variables,ions=['SiII'],wave=[1304])
;       In this case 1 indices are returned, specifying the location of SiII 1304
;
; Created by Dr. Kat Barger 04/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


low_ions=['OI','CII','SII','SiII']
high_ions=['SiIII','AlII','FeII','NiII','SiIV','CIV','NV']

n_low=n_elements(low_ions)
n_high=n_elements(high_ions)

all=strarr(n_low+n_high)
all[0:n_low-1]=low_ions
all[n_low:n_low+n_high-1]=high_ions

;Don't actually use this code for anything...hum..
;wavelengths=[1320.17,1334.2,1250.58,1253.81,$
;            1193.29,1260.42,1304.37,1526.71,$
;            1206.50,1670.79,1608.45,1317.22,$
;            1370.13,1393.76,1402.77,1548.20,$
;            1550.78,1238.82,1242.80]

if keyword_set(low) then ions=low_ions
if keyword_set(high) then ions=high_ions
;Convert to a string array in case only one string is passed.
if keyword_set(ions) then ions=strarr(n_elements(ions))+ions
good_ions=0

if keyword_set(ions) then begin

   ;Check to see if structure.
   if size(variable,/type) eq 8 then begin
      n_variable=n_elements(variable)
      indices=intarr(n_variable)
      flag_arr=intarr(n_variable)
      str=variable
      ion_loc=where(strcmp(tag_names(str),'ION',/fold_case) eq 1,count)
   
      j=0
      for i=0,n_variable-1 do begin
          indices=where(str[i].(ion_loc) eq ions,count_match)
          if count_match ne 0 then begin
             indices[j]=i
             flag_arr[j]=1
          j=j+1
       endif
   endfor

   endif
   
   ;Check to see if string.
   if size(variable,/type) eq 7 then begin
      ;Convert to a string array in case only one string is passed.
      n_variable=n_elements(variable)
      variable=strarr(n_variable)+variable
      indices=intarr(n_variable)
      flag_arr=intarr(n_variable)
      j=0
      for i=0, n_variable-1 do begin
   
          str=(SCOPE_VARFETCH(variable[i], /ENTER,level=1))
          ion_loc=where(strcmp(tag_names(str),'ION',/fold_case) eq 1,count)

          tmp=where(str.(ion_loc) eq ions,count_match)
   
          if count_match ne 0 then begin
             indices[j]=i
             flag_arr[j]=1
             j=j+1
          endif
   
      endfor
   
   endif
   
   ion_indices=indices[0:j-1]
endif

if keyword_set(wave) then begin
   variable_wave=fltarr(n_elements(variable))

      str=(SCOPE_VARFETCH(variable[0], /ENTER,level=1))
      tag_loc=where(strcmp(tag_names(str),'wavc',/fold_case) eq 1)

      for i=0, n_elements(variable)-1 do $
          variable_wave[i]=(SCOPE_VARFETCH(variable[i], /ENTER,level=1)).(tag_loc)

   wave_loc=fltarr(n_elements(wave))
   for i=0, n_elements(wave)-1 do begin
       junk=min(abs(double(wave[i])-variable_wave),loc)
       wave_loc[i]=loc
   endfor

   wave_indices=wave_loc
endif
   
if (n_elements(ion_indices) gt 0) AND (n_elements(wave_indices) gt 0) then $
   match,ion_indices,wave_indices,sub_ion,sub_wave,count=good_ions $
else if (n_elements(ion_indices) gt 0) then indices = ion_indices $
else if (n_elements(ion_wave) gt 0) then indices = wave_indices $
else good_ions = 0

if (good_ions NE 0) AND (n_elements(ion_indices) NE 0) AND (n_elements(wave_indices) NE 0) then begin
   if (good_ions NE 0) then begin
      indices=ion_indices[sub_ion] 
      flag_arr=fltarr(n_variable)
      flag_arr[indices]=1.0
   endif
endif else if (n_elements(ion_indices) eq 0) AND (n_elements(wave_indices) eq 0) then begin
   print,''
   print,'*** NO ION AND WAVELENGTH MATCHES ***'
   print,''
endif 

   if keyword_set(flag) then return, flag_arr; 
   return, indices;
  
end