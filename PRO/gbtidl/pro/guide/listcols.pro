;+
; Lists the columns in the index file (either spectral line or
; continuum, depending on value of !g.line).
;
; <p>These column names also serve as search keywords.
;
; @version $Id: listcols.pro,v 1.2 2006/05/17 07:11:14 bgarwood Exp $
;-
pro listcols
    compile_opt idl2
    
    print, "Here is the list of columns in the index file. "
    print, "These also serve as search keywords. "

    if (!g.line) then begin
        print, "Columns in spectral line index file: "
        !g.lineio->list_available_columns 
    endif else begin
        print, "Columns in continuum index file: "
        !g.contio->list_available_columns
    endelse
    
end

