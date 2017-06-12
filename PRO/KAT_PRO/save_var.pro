function save_var, file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; Purpose:
;	To extract the variable names contained within
;	a save file.
;
; Input: 
;	file - A string containing the file name of the 
;		   containing the structure.
;
; Output:
;	sNames - A string array containing the variable
;			names contained within a structure. 
;
; Tip: restore,file
;      new_variable_name=SCOPE_VARFETCH(sNames[0], LEVEL=0)
;
; Created by Dr. Kat Barger 09/2013
; 
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    sObj = OBJ_NEW('IDL_Savefile', file) 
     
    ; Get the variable name of the structure 
    ; contained in the save file
    sNames = sObj->Names() 

return,sNames

end