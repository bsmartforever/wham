function program_path

;+
; Purpose: To acquire the path directory of an active parent program or function. 
;	This is especially useful if the parent program reads in data files in either
;	the same directory or a branch of the directory. 
;
;	Input: none
;
;	Output: Directory path of the parent program or function.		
;
;	Example:
;
;	pro chicken
;		dir=program_path()
;		print,dir
;	end
;	
;	IDL> $HOME/Desktop/
;
;	In this case, the chicken program exists on the desktop
;
; Note: The meat of this code was borrowed from whocalledme.pro
;
; Created by Dr. Kat Barger 09/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ; Return to caller on error.
   On_Error, 2

   ; Get the call stack and the calling routine's name.
   callStack = Scope_Traceback()
   
   ; Find where I am in the call stack. The calling program is up
   ; two levels from there. Unless, of course, I am close to $MAIN$.
   index = Where(StrMid(callstack, 0, 12) EQ 'PROGRAM_PATH', count)
   IF count GE 1 THEN index = (Reverse(index))[0] 
   callingRoutine = (StrSplit(StrCompress(callStack[(index-1) > 0])," ", /Extract))[0]
   
   dir=file_dirname((routine_info(callingRoutine,/function,/source)).path)+'/'

return,dir

end