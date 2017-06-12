Function all_tags, structure, rootname

; This is a function that recursively searches through
; a structure tree, finding ALL of the structure's field names.
; It returns a pointer to an array of pointers, each pointing
; to the names of structure fields.

IF N_Elements(rootname) EQ 0 THEN rootname = '.' ELSE $
   rootname = StrUpCase(rootname) + '.'
names = Tag_Names(structure)
retValue = Ptr_New(rootname + names)

   ; If any of the fields are structures, report them too.

FOR j=0,N_Elements(names)-1 DO BEGIN
   ok = Execute('s = Size(structure.' + names[j] + ')')
   IF s[s[0]+1] EQ 8 THEN BEGIN
      newrootname = rootname + names[j]
      theseNames = Call_Function('All_Tags', $
         structure.(j), newrootname)
      retValue = [[retValue],[theseNames]]
   ENDIF
ENDFOR

RETURN, retValue
END