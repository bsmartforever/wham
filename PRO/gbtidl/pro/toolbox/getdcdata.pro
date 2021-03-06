;+ 
; Convenience function for retrieving data from a data container.  Can retrieve
; the entire data array, just a range, or a single element of the data.
;
; @param dc {in}{required}{type=struct} data container (spectrum or continuum)
; @param elements {in}{optional}{type=long array} elements of data to return, one integer, or two element array specifiying range
; @returns either the entire data array of data container, or part of
; it
;
; @examples
; <pre>
;    ; dc already exists
;    ; all of it
;    thedata = getdcdata(dc)
;    ; some of it
;    somedata = getdcdata(dc,[100:400])
; </pre>
;
; @uses <a href="data_valid.html">data_valid</a>
;
; @version $Id: getdcdata.pro,v 1.3 2005/10/11 20:47:27 bgarwood Exp $
;-
FUNCTION GETDCDATA, dc, elements
    compile_opt idl2

    if data_valid(dc) le 0 then message, "Data container is empty or invalid",/info
    
    if n_elements(elements) ne 0 then begin
        ; try to return part of the array
        if n_elements(elements) gt 2 then begin
            message, "elements keyword must be one index, or a two element index specifiying the range"
        endif else begin
            if n_elements(elements) eq 1 then begin
                ; return one data point
                if (elements[0] lt 0) or (elements[0] gt (n_elements(*dc.data_ptr)-1)) then message, "element out of data range"
                return, (*dc.data_ptr)[elements[0]]
            endif else begin
                ; return a range of data points
                b = elements[0]
                e = elements[1]
                if (b lt 0) or (b gt (n_elements(*dc.data_ptr)-1)) or (e lt b) or (e gt (n_elements(*dc.data_ptr)-1)) then $
                    message, "elements out of range"
                return, (*dc.data_ptr)[b:e]
            endelse 
        endelse ; if elemnts has 2 or less length    
    endif else begin
        ; return the entire data array
        return, *dc.data_ptr
    endelse

END    
