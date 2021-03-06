;+
; Function to check whether /dev/tty can be opened.  Some of the
; output routines use that to page their output using "more".  This is
; also used in the guide startup to set !g.interactive, the assumption
; being that if /dev/tty can be opened, its an interactive session.
;
; @returns 1 on success, 0 on failure.
;
; @version $Id: hastty.pro,v 1.1 2005/04/20 17:22:41 bgarwood Exp $
;-
function hastty
    result = 0
    on_ioerror,doreturn
    openw,out,'/dev/tty',/get_lun,/more
    free_lun,out
    result = 1
    
    doreturn: return, result
end
