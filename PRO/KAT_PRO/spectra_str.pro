function spectra_str, v, d, var=var, glon=glon, glat=glat, name=name, LAB=LAB

; Purpose - To create a structure array from velocity and data arrays.
; 
;    v - velocity array. If in the form [[v1],[v2]], then a multiple element 
;        spectra structure will be produced.
;    d - data array. If in the form [[d1],[d2]], then a multiple element 
;        spectra structure will be produced.
;    var - [OPTIONAL] variance array. Default is an array populated with zeros
;           unless passed. If /LAB flag is set, then the variance of the LAB HI
;           survey will be calculated.
;    glon - [OPTIONAL] Galactic longitude. Can be an array if creating a 
;           multiple element spectra structure. [DEFAULT] glon=0.      
;    glat - [OPTIONAL] Galactic latitude. Can be an array if creating a 
;           multiple element spectra structure. [DEFAULT] glat=0. 
;    name - [OPTIONAL] Name of the spectra. Can be an array if creating a 
;           multiple element spectra structure. [DEFAULT] name=''.
;    LAB - [OPTIONAL] Set to calculate the variance of LAB HI survey data.
;           Checks if the data is in units of K or cm^-2.
;
; By Dr. Kat Barger in March 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   if (NOT keyword_set(glon)) then num=1.0 else num=n_elements(glon)
   
   dim_d=size(d,/dimensions)
   if n_elements(dim_d) eq 2.0 then num=dim_d[1]

   if (NOT keyword_set(var)) then var=v*0.0 

   if (keyword_set(glon) and (NOT keyword_set(glat))) or $
      ((NOT keyword_set(glon)) and keyword_set(glat)) then begin

      print,'*** Must specify both glon and glat or not at all. ***'
      print,'         *** Assuming both are zero. ***'
      glon=fltarr(num)
      glat=fltarr(num)     

   endif

   if (NOT keyword_set(glon)) or (NOT keyword_set(glat)) then begin
      glon=fltarr(num)
      glat=fltarr(num)
   endif

   if (NOT keyword_set(name)) then name=strarr(num)
   if n_elements(name) ne num then name=strarr(num)+name

   if (n_elements(glon) ne num) or (n_elements(glat) ne num) $
      or (n_elements(name) ne num) then begin
      print,'*** glon, glat, and name arrays must be same length ***'
      return,0;
   endif

   spectra=replicate({name:name[0],vel:v[*,0],data:d[*,0],var:var[*,0],$
                      glon:glon[0],glat:glat[0]},num)

   spectra.name=name
   spectra.glon=glon
   spectra.glat=glat

   dim_d=size(d,/dimensions)
   if n_elements(dim_d) eq 2.0 then begin
      if dim_d[1] eq num then for i=0, num-1 do spectra[i].data=d[*,i] 
   endif

   dim_v=size(v,/dimensions)
   if n_elements(dim_v) eq 2.0 then begin
      if dim_v[1] eq num then for i=0, num-1 do spectra[i].vel=v[*,i] 
   endif

   if keyword_set(var) then begin
      dim_var=size(var,/dimensions)
      if n_elements(dim_var) eq 2.0 then begin
         if dim_var[1] eq num then for i=0, num-1 do spectra[i].var=var[*,i]
      endif 
   endif

   if keyword_set(LAB) then begin
      spectra[0].var=spectra[0].data*0+0.0389^2.0
      if n_elements(dim_var) eq 2.0 then begin
         if dim_var[1] eq num then for i=0, num-1 do $
            spectra[i].var=spectra[0].data*0+0.0389^2.0
      endif 
      if (min(spectra[0].data) lt -1.0e16) or (max(spectra[0].data) gt 1.0e16) then $
         spectra.var=spectra.var*(1.8224e18)^2.0
   endif

   return, spectra;

end