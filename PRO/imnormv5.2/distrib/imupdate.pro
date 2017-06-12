;+
;Kenneth Sembach
;				IMUPDATE.PRO
;				Version 5.2
;Created: 10/06/92
;Last Revision: 05/02/99
;
;Program Description:
;	This procedure inserts a comment update into the array updates.
;
;Screen Output:
;	None
;
;Use:
;	IMUPDATE,updates,comment
;
;On Input:
;		updates	:= array containing header updates
;		comment	:= comment to be inserted into array
;
;On Output:
;		updates	:= revised updates array	
;
;Common Blocks / Structures:
;	None
;
;Latest Update Comments:
;	10/07/92  KRS	- Version 5.0, 1st version
;	04/07/97  KRS	- Add /create keyword.
;	05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;	None
;------------------------------------------------------------------------------
PRO IMUPDATE,updates,comment,create=create

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imupdate' & RETURN & ENDIF

	IF KEYWORD_SET(create) NE 0 THEN BEGIN
	   updates    = STRARR(80,1,2)
           updates(0) = ';Update History:'
           updates(1) = ';IMUPDATE (v5.2)::  Newly created update comments ' $
		+ !stime
	ENDIF

	sz = SIZE(updates)  
	sz0 = sz(0)   
	sz1 = sz(1)  &  sz2 = sz(2)  &  sz3 = sz(3)

	new = STRARR(sz1,sz2,sz3+1)
	new(0) = updates
	new(sz3) = comment
	updates = new

	RETURN
	END
