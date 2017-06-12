;+
;Kenneth Sembach
;                               IMOPLOT.PRO
;                               Version 5.2
;Created: Unknown
;Last Revision: 05/02/99
;
;Program Description:
;       This procedure overplots a file by calling IMREAD to read it.
;	Nothing fancy!
;
;Restrictions:
;       Too simple to be really useful.
;
;Screen Output: 
;       Graphics
;
;Use:
;       IMOPLOT,root
;
;On Input:
;               root    :== root of file name to be read (.dat assumed)
;On Output:
;	None
;
;Common Blocks / Structures:
;       None
;
;Latest Update Comments:
;       05/02/99  KRS   - Version 5.2, documentation updated for distribution
;
;External Routines Called:
;       IMREAD
;------------------------------------------------------------------------------
PRO IMOPLOT,root

        IF N_PARAMS() EQ 0 THEN BEGIN MAN,'imoplot' & RETURN & ENDIF

	IMREAD,root,x,y,object,comment,ion,wavc,mapi,order,rflags,updates
	OPLOT,x,y,psym=-1
	RETURN
	END