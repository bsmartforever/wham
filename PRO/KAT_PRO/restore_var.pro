function restore_var, file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; Purpose:
;	To restore a save file, while enabling easy renaming the 
;	enclosed variable. 
;
; Input: 
;	file - A string containing the file name of the 
;		   containing the structure.
;
; Output:
;	sNames - A restored variables in a save file.
;		   If the save file contains multiple names,
;		   then only restore the first variable. 
;
; Example:
;	new_variable_name=restore_var(file)
;
; Created by Dr. Kat Barger 09/2013
; 
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    sObj = OBJ_NEW('IDL_Savefile', file) 
     
    ; Get the variable name of the structure 
    ; contained in the save file
    sNames = sObj->Names() 

    if n_elements(sNames) gt 1 then begin
    	print,''
    	print,'*** Save file contains multiple variables ***'
    	print,'  *** Only restoring the first variable ***'
    	print,''
    endif

    restore,file

	return,SCOPE_VARFETCH(sNames[0], LEVEL=0)

end