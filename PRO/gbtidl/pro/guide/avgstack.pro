;+
; Average the records listed in the stack.
;
; <p>The data retrieval is done using <a href="getrec.html">getrec</a>.  See the documentation 
; there for a longer discussion on the useflag and skipflag keywords
; also found here.
;
; @keyword noclear {in}{optional}{type=boolean} If this is set, the
; accum buffer is not cleared prior to averaging the records
; @keyword useflag {in}{optional}{type=boolean or string}{default=true}
; Apply all or just some of the flag rules?
; @keyword skipflag {in}{optional}{type=boolean or string} Do not apply
; any or do not apply a few of the flag rules?
; @keyword keep {in}{optional}{type=boolean} If this is set, the
; records are fetched from the keep file.
;
; @examples
;  Add index number 25, 30 through 39, and the odd indexes from 41
;  through 51 to the stack, and average them.
; <pre>
;    addstack, 25
;    addstack, 30, 39
;    addstack, 41, 51, 2
;    avgstack
; </pre>
; <p>
; An example showing the use of the /noclear keyword.
; <pre>
;    addstack, 25
;    addstack, 30, 39
;    avgstack, /noclear      ; see the result so far, do not clear it
;    emptystack
;    addstack, 50, 90, 2 
;    avgstack                ; builds on the previous result
;                            ; cleared after this use of avgstack
; </pre>
;
; @version $Id: avgstack.pro,v 1.10 2007/04/12 16:22:52 bgarwood Exp $
;-

pro avgstack,noclear=noclear,useflag=useflag,skipflag=skipflag,keep=keep
    compile_opt idl2
    if not !g.line then begin
        message,'accum only works on spectral line data, can not avgstack continuum data, sorry',/info
	return
    endif
    oldFrozen = !g.frozen
    freeze
    if not keyword_set(noclear) then sclear
    accumCountStart = !g.accumbuf[0].n
    if keyword_set(keep) then begin
        for i=0,(!g.acount-1) do begin
            kgetrec,astack(i),useflag=useflag,skipflag=skipflag
            accum
        endfor
    endif else begin
        for i = 0,(!g.acount-1) do begin
            getrec,astack(i),useflag=useflag,skipflag=skipflag
            accum
        endfor
    endelse
    if not oldFrozen then unfreeze
    accumCount = !g.accumbuf[0].n-accumCountStart
    if accumCount eq 0 then begin
        message,'All of the records retrieved using the stack were blanked.',/info
        if !g.accumbuf[0].n ne 0 then begin
            ave, noclear=noclear
        endif else begin
            if not !g.frozen then show
        endelse
    endif else begin
        if accumCount ne !g.acount then begin
            message,'Skipped '+strtrim(!g.acount - accumCount,2)+' records in average due to blanked data',/info
        endif
        ave, noclear=noclear
    endelse
end
