;+
; Turns off write protection for nsave numbers.  When off, rows in
; index file with nsave numbers can be changed.
;
; @version $Id: sprotect_off.pro,v 1.2 2006/05/17 07:11:14 bgarwood Exp $
;-
PRO sprotect_off
    compile_opt idl2
    !g.sprotect = 0
END
