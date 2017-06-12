function validate_extension,filename,extension
;
; Purpose - Check to see if file name contains a valid extension or not.
;
; Input:
; filename - String containing filename to test.
; extension - String containing extension name.
;
; Output:
;   filename with a valid file extension.
;
; Created by Dr. Kat Barger 04/16/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

tmp=strsplit(filename,'.',/extract)

;File doesn't contain a '.', add the extension.
if n_elements(tmp) eq 1 then begin
   if strmatch(extension,['.*']) eq 1 then $
      valid_name=filename+extension $
   else valid_name=filename+'.'+extension
   return,valid_name;
endif else begin
;File does contain a '.', validate the extension.
   if (tmp[1] eq extension) OR $
      (strmatch(extension,['.'+tmp[1]]) eq 1) then $
         return, filename $
      else valid_name=filename+'.'+extension
endelse
   
   return,valid_name;

end