function binstr, str, tags, factor=factor, length=length, error=error, variance=variance, help=help
;
; Purpose - To rebin specified data arrays within a structure.
;
; str - Structure
; tags - String array containing structure tag to bin. 
; factor - Specifies factor to reduce the number of elements of data array by.
;              e.g., new array size = old array size / factor
; length - Specifies length to resize array.
; error - uncertainty flag array, with 0 = data array and 1 = uncertainty array. 
; variance - variance flag array, with 0 = data array and 1 = variance array.
;
; *** Either factor or length must be specified ***
;
; Created by Dr. Kat Barger 04/14/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF (N_PARAMS() EQ 0) OR (Keyword_set(help)) THEN BEGIN 
   print, 'function binstr, str, tags, factor=factor, length=length, help=help' & RETURN,0; 
ENDIF

if (NOT keyword_set(factor)) AND (NOT keyword_set(length)) then begin

   print,''
   print,'*** Must specify either factor or length! ***'
   print,'*** returning unmodified structure ***'
   print,''
   return,str;

endif

modified_str=str

;convert to arrays
tags=strarr(n_elements(tags))+tags

if ((keyword_set(error)) AND (n_elements(error) ne n_elements(tags))) OR $ 
   ((keyword_set(variance)) AND (n_elements(variance) ne n_elements(tags))) then begin
    print,''
    print,'*** Error and Variance array sizes must be equal to the size of Tags. ***'
    print,'*** returning unmodified structure ***'
    print,''
    return,str;
endif

if keyword_set(error) then error=intarr(n_elements(error))+error else error=intarr(n_elements(tags))
if keyword_set(variance) then variance=intarr(n_elements(variance))+variance else variance=intarr(n_elements(tags))

for i = 0, n_elements(tags) - 1 do begin

    tag_loc=where(strcmp(tag_names(modified_str),tags[i],/fold_case) eq 1,count)

    if count ne 0 then begin

       num = n_elements(modified_str.(tag_loc))
       if keyword_set(factor) then num_bin=fix(num/factor)
       if keyword_set(length) then num_bin=length
       if (NOT keyword_set(factor)) then factor=num/num_bin

       if num_bin eq 0 then begin

          print,''
          print,'*** Bin factor is too large. ***'
          print,'*** Must be smaller than '+string(num,format='(I2)')+' ***'
          print,'*** returning unmodified structure ***'
          print,''
          return,str;

       endif

       if error[i] eq 1 then $
          modified_str=mod_str(modified_str,tags[i],frebin(modified_str.(tag_loc),num_bin)/factor) $
       else if variance[i] eq 1 then $
          modified_str=mod_str(modified_str,tags[i],(frebin(sqrt(modified_str.(tag_loc)),num_bin)/factor)^2.0) $
       else $
          modified_str=mod_str(modified_str,tags[i],frebin(modified_str.(tag_loc),num_bin))

    endif else begin

       print,''
       print,'*** '+tags[i]+' is an invalid tag ***'
       print,'*** returning unmodified structure ***'
       print,''
       return,str;

    endelse

endfor


   return,modified_str;   

end