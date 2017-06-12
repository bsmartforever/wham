function sub_column, str1, str2, factor=factor
;
; Purpose - To calculate str1.column - str2.column.
;           To do this, all rebining and smoothing must 
;           first occur in flux, then converted into apparent 
;           column density.
;
; Procedure: 1: Smooth flux so that the both str1 and str2 have 
;            similar velocity resolution.
;            2: Rebin velocities of str1 and str2 so they have
;            the same number of elements.
;            3: Interpolate the of fluxes str2 so they lie at the 
;            exact same velocities of str1.
;            4: Convert fluxes to column densities.
;            5: calculate and return str1.column - str2.column.
;
; str1 - structure containing vel, flux, continuum, fval, and wavc.
; str2 - structure containing vel, flux, continuum, fval, and wavc.
; factor - smoothing factor to transform str1 and str2 such that they 
;          have similar velocity resolution. This is done by convolving 
;          the flux spectrum with a Gaussian of width=sqrt(width1^2+width2^2),
;          where width1 and width2 are the respective velocity resolutions. 
;          [DEFAULT]: instrument tag = COS => width = 17.0 km/s
;                                      STIS => width = 6.5 km/s
;
; Returns: str1.column - str2.column
;
; Created by Dr. Kat Barger 04/22/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

str1_orig=str1
str2_orig=str2

COS_width=17.0
STIS_width=6.5

   tag_loc1=where(strcmp(tag_names(str1),'vel',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'vel',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      print,''
      print,'*** Structures are missing vel tags!  ***'
      print,'*** returning first structure ***'
      print,''
      return,str1_orig;
   end

   tag_loc1=where(strcmp(tag_names(str1),'continuum',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'continuum',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      print,''
      print,'*** Structures are missing continuum tags!  ***'
      print,'*** returning first structure ***'
      print,''
      return,str1_orig;
   end

   wavc_tag_loc1=where(strcmp(tag_names(str1),'wave',/fold_case) eq 1,check1)
   wavc_tag_loc2=where(strcmp(tag_names(str2),'wavc',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      print,''
      print,'*** Structures are missing fvals tags!  ***'
      print,'*** returning first structure ***'
      print,''
      return,str1_orig;
   end

   tag_loc1=where(strcmp(tag_names(str1),'fval',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'fval',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      print,''
      print,'*** Structures are missing wavc tags!  ***'
      if check1 eq 0 then add_tag,str1,'fval',FIND_FVAL(str1.(wavc_tag_loc)),str1
      if check2 eq 0 then add_tag,str2,'fval',FIND_FVAL(str2.(wavc_tag_loc)),str2
      print,''
      print,'fval1: ', str1.fval
      print,'fval2: ', str2.fval
   end

;If factor is not passed then determine based on instrument tags. 
;If no instrument tag, then assume a factor of [1.,1.]
if (NOT keyword_set(factor)) then begin
   factor=[0.,0.]

   tag_loc1=where(strcmp(tag_names(str1),'INSTRUMENT',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'INSTRUMENT',/fold_case) eq 1,check2)

   if (check1 eq 0) or (check2 eq 0) then factor=[1.,1.] $
   else begin 

      if str1.(tag_loc1) eq 'COS' then width1=COS_width $
      else if str1.(tag_loc1) eq 'STIS' then width1=STIS_width $
      else factor=[1.,1.]

      if (str2.(tag_loc2) eq 'COS') AND (factor[0] ne 1.0) AND (factor[1] ne 1.0) then width2=COS_width $
      else if (str2.(tag_loc2) eq 'STIS') AND (factor[0] ne 1.0) AND (factor[1] ne 1.0) then width2=STIS_width $
      else factor=[1.,1.]

   endelse

   if (factor[0] ne 1.0) AND (factor[1] ne 1.0) then begin
      broadening_factor=sqrt(abs(width1^2.0-width2^2.0))
      if width1 gt width2 then factor=[1.,broadening_factor] else factor=[broadening_factor,1.]
   endif   

endif

zeros=where(factor eq 0.,count_zeros)
if count_zeros ne 0 then factor[zeros]=1.

;;;Smooth fluxes to same velocity resolution ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;Check for flux tag
   tag_loc1=where(strcmp(tag_names(str1),'flux',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'flux',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      print,''
      print,'*** Structures are missing flux tags!  ***'
      print,'*** returning first structure ***'
      print,''
      return,str1_orig;
   end

   mod_flux1=gaussfold(str1.vel,str1.(tag_loc1),factor[0])
   mod_flux2=gaussfold(str2.vel,str2.(tag_loc2),factor[1])

   mod_str1=mod_str(str1,tag_loc1,mod_flux1)
   mod_str2=mod_str(str2,tag_loc2,mod_flux2)

;;;Apply velocity rebinning and velocity interpolation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   tags=['flux']
   error=0
   variance=0

   ;Check for error array
   tag_loc1=where(strcmp(tag_names(str1),'flux_err',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'flux_err',/fold_case) eq 1,check2)
   if (check1 eq 1) OR (check2 eq 1) then begin
      tags=['flux','flux_err']
      error=1
      variance=0
   end

   ;Check for varience array
   tag_loc1=where(strcmp(tag_names(str1),'flux_var',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'flux_var',/fold_case) eq 1,check2)
   if (check1 eq 1) OR (check2 eq 1) then begin
      tags=['flux','flux_var']
      error=0
      variance=1
   end

   if n_elements(mod_str1.vel) gt n_elements(mod_str2.vel) then mod_str1=binstr_interpol(mod_str1,mod_str2,'vel',tags,variance=variance,error=error)

   if n_elements(mod_str2.vel) gt n_elements(mod_str1.vel) then mod_str2=binstr_interpol(mod_str2,mod_str1,'vel',tags,variance=variance,error=error)

;;;Calculate apparent column densities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

column1=3.768e14*alog((mod_str1.continuum)/(mod_str1.flux))/(mod_str1.wavc*mod_str1.fval)
   ;zeros=where(mod_str1.flux le 0,count_zeros)
   ;if count_zeros ne 0 then column1[zeros]=0

   bad_index=where(finite(column1) eq 0.0,bad_values)
   if bad_values ne 0 then column1[bad_index]=0.0

column2=3.768e14*alog((mod_str2.continuum)/(mod_str2.flux))/(mod_str2.wavc*mod_str2.fval)
   ;zeros=where(mod_str2.flux le 0,count_zeros)
   ;if count_zeros ne 0 then column2[zeros]=0

   bad_index=where(finite(column2) eq 0.0,bad_values)
   if bad_values ne 0 then column2[bad_index]=0.0

   ;Check to see if column tag exists. If not, add it. 
   tag_loc1=where(strcmp(tag_names(mod_str1),'column',/fold_case) eq 1,check1)
   if check1 ne 0 then mod_str1=mod_str(mod_str1,'column',column1) $
   else add_tag,mod_str1,'column',column1,mod_str1

   ;Check to see if column tag exists. If not, add it. 
   tag_loc2=where(strcmp(tag_names(mod_str2),'column',/fold_case) eq 1,check2)
   if check2 ne 0 then mod_str2=mod_str(mod_str2,'column',column2) $
   else add_tag,mod_str2,'column',column2,mod_str2

;;;Calculate str1.column-str2.column ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;Check for names
   tag_loc1=where(strcmp(tag_names(str1),'name',/fold_case) eq 1,check1)
   tag_loc2=where(strcmp(tag_names(str2),'name',/fold_case) eq 1,check2)
   if (check1 eq 0) OR (check2 eq 0) then begin
      if check1 eq 0 then add_tag,mod_str1,'name','str1',mod_str1
      if check2 eq 0 then add_tag,mod_str2,'name','str2',mod_str2
   end

   ; Uncertanties:
   ; delta_column = abs(d_column / d_flux) * delta_flux
   ; d_column / d_flux is the partial column density derivative with respect to flux.
   ; column = 3.768e14/(wavc*fval)*alog(continuum/flux)
   ; abs(d ln(a/x) / d x) = 1/x
   ; delta_column = 3.768e14/(wavc*fval) * delta_flux/flux

   if (error eq 1) or (variance eq 1) then begin
      if error eq 1 then begin
         column_err1 = 3.768e14/(mod_str1.wavc*mod_str1.fval)*mod_str1.flux_err/mod_str1.flux
         column_err2 = 3.768e14/(mod_str2.wavc*mod_str2.fval)*mod_str2.flux_err/mod_str2.flux

         ;Check to see if column_err tag exists. If not, add it. 
         tag_loc1=where(strcmp(tag_names(mod_str1),'column_err',/fold_case) eq 1,check1)
         if check1 ne 0 then mod_str1=mod_str(mod_str1,'column_err',column_err1) $
         else add_tag,mod_str1,'column_err',column_err1,mod_str1

         ;Check to see if column_err tag exists. If not, add it. 
         tag_loc2=where(strcmp(tag_names(mod_str2),'column_err',/fold_case) eq 1,check2)
         if check2 ne 0 then mod_str2=mod_str(mod_str2,'column_err',column_err2) $
         else add_tag,mod_str2,'column_err',column_err2,mod_str2

         str1_str2 = REPLICATE({$
              name:mod_str1.name+'-'+mod_str2.name,$
              vel:mod_str1.vel,$ ;mod_str1.vel & mod_str2.vel should be identical by this point.
              column:mod_str1.column-mod_str2.column,$
              column_err:sqrt((mod_str1.column_err)^2.0+(mod_str2.column_err)^2.0)},1)
      endif

      if variance eq 1 then begin
         column_err1 = 3.768e14/(mod_str1.wavc*mod_str1.fval)*sqrt(mod_str1.flux_var)/mod_str1.flux
         column_err2 = 3.768e14/(mod_str2.wavc*mod_str2.fval)*sqrt(mod_str2.flux_var)/mod_str2.flux

         ;Check to see if column_var tag exists. If not, add it. 
         tag_loc1=where(strcmp(tag_names(mod_str1),'column_var',/fold_case) eq 1,check1)
         if check1 ne 0 then mod_str1=mod_str(mod_str1,'column_var',(column_err1)^2.0) $
         else add_tag,mod_str1,'column_var',(column_err1)^2.0,mod_str1

         ;Check to see if column_err tag exists. If not, add it. 
         tag_loc2=where(strcmp(tag_names(mod_str2),'column_err',/fold_case) eq 1,check2)
         if check2 ne 0 then mod_str2=mod_str(mod_str2,'column_var',(column_err2)^2.0) $
         else add_tag,mod_str2,'column_var',(column_err2)^2.0,mod_str2

         str1_str2 = REPLICATE({$
              name:mod_str1.name+'-'+mod_str2.name,$
              vel:mod_str1.vel,$ ;mod_str1.vel & mod_str2.vel should be identical by this point.
              column:mod_str1.column-mod_str2.column,$
              column_var:(mod_str1.column_var)+(mod_str2.column_var)},1)
      endif

   endif else $
         str1_str2 = REPLICATE({$
              name:mod_str1.name+'-'+mod_str2.name,$
              vel:mod_str1.vel,$ ;mod_str1.vel & mod_str2.vel should be identical by this point.
              column:mod_str1.column-mod_str2.column},1)
      
str1=str1_orig
str2=str2_orig

   return, str1_str2; 
   
end