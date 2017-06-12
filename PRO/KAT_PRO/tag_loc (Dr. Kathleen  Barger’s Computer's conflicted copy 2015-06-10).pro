function tag_loc, struct, tag

; Purpose:
;	To determine the index location of a specified tag
;
; Input:
;	struct - structure to search for tag
;	tag    - tag name (string)
;
; Example:
;	struct={tree:0.1,bee:0.9}
;	data=struct.(tag_loc(struct,'tree'))
;
; Created by Dr. Kat Barger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if (size(struct[0],/type) ne 8) OR (size(tag[0],/type) ne 7) then begin
		print,''
		print,'** Invalid input'
		print,''
		return,-1. 
	endif

	tnames=TAG_NAMES(struct[0])
	tindex=fltarr(n_elements(tnames))-1
	for i=0,n_elements(tnames)-1 do $
		tindex[i]=(strcmp(tnames[i],tag,/fold_case) eq 1)

	good_tag=where(tindex ne 0,count)
	if count eq 0 then tindex=-1

	return,good_tag

end