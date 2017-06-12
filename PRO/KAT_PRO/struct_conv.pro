;+
; NAME:
;       STRUCT_CONV
;
;
; PURPOSE:
;       Convert a structure containing array fields of the same length
;       N_el into an N_el array of structures
;
; CALLING SEQUENCE:
;
;       new_struct = struct_conv( old_struct)
;
; RESTRICTIONS:
;
; You'll need the Goddard routine create_struct:
;   http://astro.berkeley.edu/~johnjohn/idl.html#CREATE_STRUCT
;
; INPUTS:
;       old_struct: The original structure containing arrays of the
;                   same length in each of its fields
;
; OUTPUTS:
;       new_struct: An array of structures 
;
; EXAMPLE:
;
;  old = {a: fltarr(100), b: fltarr(100)}
;  help, old
;  OLD             STRUCT    = -> <Anonymous> Array[1]
;  help, old, /str
;  ** Structure <5fe1f8>, 2 tags, length=800, data length=800, refs=1:
;   A               FLOAT     Array[100]
;   B               FLOAT     Array[100]
;  new = struct_conv(old)
;  help, new
;  NEW             STRUCT    = -> <Anonymous> Array[100]
;  help, new, /str
;  ** Structure <5d5c58>, 2 tags, length=8, data length=8, refs=1:
;   A               FLOAT           0.00000
;   B               FLOAT           0.00000
;
; MODIFICATION HISTORY:
;   Written by JohnJohn sometime in early ought 5.
;-

function struct_conv, struct
ntags = n_tags(struct)
type = strarr(ntags)
len = fltarr(ntags)
for i = 0, ntags-1 do begin
    t = size(struct.(i), /type)
    len[i] = n_elements(struct.(i))
    case t of 
        1 : type[i] = 'i'
        2 : type[i] = 'i'
        3 : type[i] = 'i'
        4 : type[i] = 'f'
        5 : type[i] = 'd'
        6 : type[i] = 'c'
        7 : type[i] = 'a'
        9 : type[i] = 'm'
        13: type[i] = 'j'
        14: type[i] = 'k'
        15: type[i] = 'k'
        else: $
          message, 'Structure field '+struct.(i)+' contains unsuppored data type'
    endcase
endfor
if mean(len) ne len[0] then $
  message,'All structure fields must have the same length',/ioerr
create_struct,new,'',tag_names(struct),type
nel = n_elements(struct.(0))
newst = replicate(new, nel)
for i = 0, ntags-1 do begin
    newst.(i) = struct.(i)
endfor
return, newst
end