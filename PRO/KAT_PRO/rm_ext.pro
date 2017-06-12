function rm_ext, filename, ext, dash=dash

; Purpose:
;	To remove the file extension from a file name.
;
; Input:
;	filename - String containing the file name.
;	ext 	 - String containing the file extension.
;			   Period before extension name is optional.
;	dash	 - Flag to convert dashes to underscores in filename.
;
; e.g., filename=rm_ext('testing.txt','txt')
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

name=filename

ext=(strsplit(ext,'.',/extract))[0]

if keyword_set(dash) then begin
	;; If the name contains a dash replace it with an underscore.
	;if (strpos(name, '-') NE -1) then begin
	;   pieces = str_sep(name, '-')
	;   n_pieces = n_elements(pieces)
	;   name = pieces(0)
	;   for i=1,n_pieces-1 do begin
	;      name = strcompress(name+"_"+pieces(i))
	;   endfor
	;endif
	name=strjoin(strsplit(name,'-',/extract),'_')
endif

	name=strsplit(name,'\.'+ext,/extract,/regex,/fold_case)
 	name=name[0]
return,name


;;;;; Ignore:

tmp=strsplit(name,'.',/extract)

num=n_elements(tmp)

;File doesn't contain a '.', add the ext.

if strmatch(ext,tmp[num-1]) eq 1 then begin
	if n_elements(tmp[0:num-1]) gt 2 then tmp[0:num-3]=tmp[0:num-3]+'.'
	string_array=tmp[0:num-2]
	one_string = string(string_array, $
			format = '(' + string(n_elements(string_array)) + 'A)')
	return, one_string;

endif else return, name;

end