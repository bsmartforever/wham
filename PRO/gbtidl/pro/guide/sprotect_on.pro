;+
; Turns on write protection for nsave numbers.  When on, rows in index
; file with nsave numbers cannot be changed.
;
; @version $Id: sprotect_on.pro,v 1.2 2006/05/17 07:11:14 bgarwood Exp $
;-
PRO sprotect_on
    compile_opt idl2
    !g.sprotect = 1
END
