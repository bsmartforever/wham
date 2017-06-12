function binstr_interpol, str, new_xarr, xtag, tags, error=error, variance=variance, help=help
;
; Purpose - To rebin specified xarr and yarr data arrays pairs within a structure to match new_xarr spacing. 
;           This is done using linear interpolation with interpol.pro and linear_interpol_err.pro.
;           If str array length differs from new_xarr, then binstr.pro is first called to frebin.pro structure.
; 
; str - Structure.
; new_xarr - New values for xarr. Can pass an xarr or a structure containing that array. 
; xtag - String containing xtag name.
; tags - String array containing structure tag to bin. 
; Error - Error flag. If set, then pass ytagtags=[['y1tag','y1tag_errors']['y2tag','y2tag_errors']].
;         If y*tag_errors='' then no uncertainty will be calculated.  
; Variance - Variance flag. If set, then pass ytagtags=[['y1tag','y1tag_variance']['y2tag','y2tag_variance']].
;         If y*tag_variance='' then no uncertainty will be calculated.  
;
; Created by Dr. Kat Barger 04/15/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF (N_PARAMS() EQ 0) OR (Keyword_set(help)) THEN BEGIN 
   print, 'function binstr_interpol, str, new_xarr, xtag, tags, error=error, variance=variance, help=help' & RETURN,0; 
ENDIF

new_xarr_orig=new_xarr

if size(new_xarr,/type) eq 8 then begin
   xtag_loc=where(strcmp(tag_names(new_xarr),xtag,/fold_case) eq 1,check)

   if check ne 0 then begin
      new_xarr = new_xarr.(xtag_loc) 
   endif else begin
      print,'*** The structure defining the new xarr does not contain the right tag ***'
      print,'*** returning unmodified structure ***'
      new_xarr_orig=new_xarr
      return,str;
   endelse

endif

xtag_loc=where(strcmp(tag_names(str),xtag,/fold_case) eq 1,check)
if check ne 0 then xarr = str.(xtag_loc) else begin
    print,''
    print,'*** '+xtag+' is an invalid tag ***'
    print,'*** returning unmodified structure ***'
    print,''
    new_xarr_orig=new_xarr
    return,str;
endelse

if keyword_set(error) or keyword_set(variance) then begin
 
   if keyword_set(error) then check=size(tags,/n_dim)
   if keyword_set(variance) then check=size(tags,/n_dim)

   if (check ne 2) AND ((n_elements(tags) ne 2) AND (check ne 1)) then begin
      print,''
      print,'*** '+tags+' are invalid tags, pass either ***'
      print,'*** [[y1tag,y1tag_error],[y2tag,y2tag_error]] ***'
      print,'*** OR ***'
      print,'*** [[y1tag,y1tag_variance],[y2tag,y2tag_variance]] ***'
      print,'*** returning unmodified structure ***'
      print,''
      new_xarr_orig=new_xarr
      return,str;
   endif

   tag_err_var=tags[1,*]
   tags=tags[0,*]

endif else tag_err_var=replicate('',n_elements(tags))

interp_str=str

for i=0, n_elements(tags)-1 do begin

    if (keyword_set(error) OR keyword_set(variance)) and (tag_err_var[i] ne '') then begin

       var_err_tag_loc=where(strcmp(tag_names(interp_str),tag_err_var[i],/fold_case) eq 1,check)
       if n_elements(new_xarr) ne n_elements(interp_str.(var_err_tag_loc)) then begin
 
          if keyword_set(error) then $
             interp_str=binstr(interp_str,tag_err_var[var_err_tag_loc],length=n_elements(new_xarr),/error)
          if keyword_set(variance) then $
             interp_str=binstr(interp_str,tag_err_var[var_err_tag_loc],length=n_elements(new_xarr),/variance)

       endif

    endif

    tag_loc=where(strcmp(tag_names(interp_str),tags[i],/fold_case) eq 1,check) 

    xold = frebin(interp_str.(xtag_loc),n_elements(new_xarr))    

    if n_elements(new_arr) ne n_elements(interp_str.(tag_loc)) then $
       interp_str=binstr(interp_str,tags[i],length=n_elements(new_xarr))

    if n_elements(new_arr) ne n_elements(interp_str.(tag_loc)) then $
       interp_str=binstr(interp_str,tags[i],length=n_elements(new_xarr))


    if keyword_set(error) then $
    interp_str=mod_str(interp_str,tags[i],linear_interpol_err(xold,new_xarr,interp_str.(tag_loc),interp_str.(var_err_tag_loc)))
   
    if keyword_set(variance) then $
    interp_str=mod_str(interp_str,tags[i],(linear_interpol_err(xold,new_xarr,interp_str.(tag_loc),sqrt(interp_str.(var_err_tag_loc))))^2.0)

    interp_str=mod_str(interp_str,tags[i],interpol(interp_str.(tag_loc),xold,new_xarr))

endfor

    interp_str=mod_str(interp_str,xtag,xold)

    new_xarr=new_xarr_orig

return, interp_str;

end