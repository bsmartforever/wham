PRO add_tag, struct, tagname, tagtype, newstr, structyp=structyp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
;
; NAME:
;    ADD_TAG
;       
; PURPOSE:
;    Add a new tag to the structure. NOTE: if you want to add more
;    than one tag at once, use ADD_TAGS
;
; CALLING SEQUENCE:
;    add_tag, oldstruct, tagname, tagtype, newstruct, structyp=structyp
;
; INPUTS: 
;    oldstruct: The original structure (or array of structures)
;    tagname: string containing the new tag name
;    tagtype: the initial value of the new tag, e.g. fltarr(5)
;           or [3,5,6], or 0L, etc.
;
; KEYWORD PARAMETERS:
;   structyp: a string with the name of the new structure.
;     if already defined the program will crash.
;
; OUTPUTS: 
;    newstruct: The structure with the new tag it it.
;
; OPTIONAL OUTPUT
;    NONE
;
; CALLED ROUTINES:
;    COMBINE_STRUCTS
; 
; PROCEDURE: 
;    
;	EXAMPLE:
;    struct={tmp:'1',tree:6}
;    add_tag,struct,'test','',new
;    IDL> help,new,/str
;    ** Structure <310aa78>, 3 tags, length=40, data length=34, refs=1:
;       TMP             STRING    '1'
;       TREE            INT              6
;       TEST            STRING    ''
;
; REVISION HISTORY:
;    25-OCT-2000, Judith Racusin.
;    21-APR-2013, Dr. Kat Barger
;       Added ability to not only add a tag name, but to save the values 
;       in that tag. Previously, the values got substituted with 
;       structure.tag=structure[0].tag in the new structure with the new tag.   
;    24-NOV-2013, Dr. Kat Barger
;       Added the ability to add a valid pointer through tagtype.
;       To do this, set tagtype=ptr_new() or ptr_new(/allocate_heap);
;       Through either, the tag will be set to a valid pointer, 
;       e.g., ptr_valid(str.pointer_tag) = 1.
;       This allows the user to freely define the pointer tag to 
;       any value or array of an arbitrary size and type. This is 
;       done by *str.pointer_tag=arbitrary data.
;       
;                                   
;-                                       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  IF n_params() LT 3 THEN BEGIN 
      print,'Syntax - add_tag,struct, tagname, tagtype, newstr, structyp=structyp'
      print,'Use doc_library,"add_tag"  for more help.'  
      return
  END

  original_struct=struct

  t=tag_names(struct)
  w=where(t EQ strupcase(tagname),nw)
  IF nw NE 0 THEN BEGIN 
      print,'Tag ',tagname,' Already Exists'
      return
  END

  if size(tagtype,/type) eq 10 then tagtype=ptr_new(/allocate_heap)

  tmpstr=create_struct(tagname,tagtype)
  tmpstr=replicate(tmpstr,n_elements(struct))

  combine_structs,struct,temporary(tmpstr),newstr, structyp=structyp

  tag_loc=where(strcmp(tag_names(newstr),tagname,/fold_case) eq 1)
  if size(tagtype,/type) eq 10 then $
     newstr.(tag_loc)=PtrArr(n_elements(newstr), /ALLOCATE_HEAP) 

  ;adding the nozero tag so that pointers remain valid (non NULL)
  if size(tagtype,/type) eq 10 then $ struct_assign,original_struct,newstr,/nozero

  return
END