;+
; Append entries to the stack. 
;
; <p>See also <a href="addstack.html">addstack</a>.
;
; @param index {in}{required}{type=long integer} Entries to add
;
; @examples
; <pre>
;   addstack,10,14,2
;   appendstack,20
;   appendstack,[25,28]
;   tellstack
;   ; The stack now contains [ 10,  12,  14,  20,  25, 28]
; </pre>
;
; @version $Id: appendstack.pro,v 1.4 2006/05/15 19:20:55 bgarwood Exp $
;-
PRO appendstack, index
    compile_opt idl2

    ntoadd = n_elements(index)
    if (ntoadd eq 0) then begin
       message,"Usage: appendstack, index", /info
       return
    end

    newcount = !g.acount + ntoadd
    if (newcount gt n_elements(*!g.astack)) then begin
        oldstackptr = !g.astack
        newsize = ceil(float(newcount) / 5120.) * 5120.
        !g.astack = ptr_new(lonarr(newsize))
        if (!g.acount gt 0) then begin
            (*!g.astack)[0:(!g.acount-1)] = (*oldstackptr)[0:(!g.acount-1)]
        endif
        ptr_free, oldstackptr
    endif

    (*!g.astack)[!g.acount:(newcount-1)] = index
    !g.acount = newcount
END
