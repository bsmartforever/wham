;+
; This procedure decimates a spectrum (i.e. thins the spectrum by
; paring every nth channel).  The dc argument is modified in place.
;
; <p>The frequency interval and reference channel are adjusted
; appropriately so that the x-axis labels for the decimated data are
; still appropriate.
;
; <p>This only works for spectral line data containers.
;
; @param dc {in}{out}{required}{type=data container} data container (spectrum or continuum)
; @param nchan {in}{required}{type=integer} choose every nth channel
; starting at startat.
; @keyword startat {in}{optional}{type=integer}{default=0} The starting channel.
; @keyword ok {out}{optional}{type=boolean} Returns 1 if everything
; went ok, 0 if it did not (missing parameters, invalid or empty dc,
; bad startat)
;
; @examples
; <pre>
;    get,index=1
;    a = data_new()
;    data_copy,!g.s[0],a
;    show
;    dchanning,a
;    show,a
;    dcdecimate,a,3
;    show,a
;    dcdecimate,a,3,startat=1
;    show,a
; </pre>
;
; @uses <a href="data_valid.html">data_valid</a>
; @uses <a href="seq.html">seq</a>
;
; @version $Id: dcdecimate.pro,v 1.5 2005/11/01 22:07:03 bgarwood Exp $
;-

pro dcdecimate, dc,nchan,startat=startat,ok=ok
    compile_opt idl2

    ok = 0
    if n_params() ne 2 then begin
        usage,'dcdecimate'
        return
    endif

    nels = data_valid(dc,name=name)
    if nels le 0 then begin
        message,'dc is empty or invalid',/info
        return
    endif

    if nchan le 0 or nchan gt nels then begin
        message,'nchan must be > 0 and  <= number of channels',/info
        return
    endif

    if n_elements(startat) eq 0 then startat = 0

    if startat lt 0 or startat gt (nels-1) then begin
        message,'startat is < 0 or > (number of channels - 1)',/info
        return
    endif

    newdc = dcextract(dc,startat,(nels-1),nchan)

    data_copy,newdc,dc

    data_free, newdc

    ok = 1
end
