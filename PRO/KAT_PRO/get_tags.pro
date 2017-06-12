FUNCTION get_tags, structure, rootname

; This function returns the names of all structure fields
; in the structure as a string array. The names are given
; as valid structure names from the root structure name,
; which can be passed in along with the structure itself.

On_Error, 1

   ; Check parameters.

CASE N_Params() OF

   0: BEGIN
      Message, 'Structure argument is required.'
      ENDCASE
      
   1: BEGIN
      rootname = ''
      s = Size(structure)
      IF s[s[0]+1] NE 8 THEN $
         Message, 'Structure argument is required.'
      ENDCASE
      
   2: BEGIN
      s = Size(structure)
      IF s[s[0]+1] NE 8 THEN $
         Message, 'Structure argument is required.'
      s = Size(rootname)
      IF s[s[0]+1] NE 7 THEN $
         Message, 'Root Name parameter must be a STRING'
      ENDCASE
      
ENDCASE

tags = All_Tags(structure, rootname)

   ; Extract and free the first pointer.

retval = [*tags[0,0]]
Ptr_Free, tags[0,0]

   ; Extract and free the the rest of the pointers.
   
s = Size(tags)
FOR j=1,s[2]-1 DO BEGIN
   retval = [retval, *tags[0,j]]
   Ptr_Free, tags[0,j]
ENDFOR
Ptr_Free, tags

   ; Return the structure names.
   
RETURN, retval
END


   ; Main-level program to exercise Get_Tags.

d = {dog:'spot', cat:'fuzzy'}
c = {spots:4, animals:d}
b = {fast:c, slow:-1}
a = {cars:b, pipeds:c, others:'man'}
tags = Get_Tags(a)
s = Size(tags)
For j=0,s[1]-1 Do Print, tags[j]
END
