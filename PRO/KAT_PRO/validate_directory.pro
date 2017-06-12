function validate_directory, dir

;+
; Purpose: 
;	To ensure that the supplied directory name ends in a '/'.
;	If not, add a '/'.
;
; Input:
;	dir - directory 
;
; Output:
; 	dir - directory with a trailing '/' at the end.
;
;
; Created by Dr. Kat Barger 05/2013
;-

tmp=strmatch(dir,'*/')

if tmp eq 0 then dir=dir+'/' 

	return,dir;

end