;+
; Fill in the indicated data container at modelbuffer using the
; primary data container and the most recently fit parameters in
; !g.polyfit and !g.nfit.  Optionally use a smaller nfit than
; !g.nfit. 
;
; <p>Note that bmodel does not do any fitting.  Use <a href="baseline.html">baseline</a> or <href="bshape.html">bshape</a>
; to generate a new fit.
;
; <p>Since orthogonal polynomials are used internally, using up
; to any nfit less than or equal to the value of nfit used when the
; polynomials was generated is itself the fit that would have resulted
; had that smaller nfit been used in the first place.  In this way,
; bmodel can be used to generate the model fit for any value of nfit
; up to the nfit actually used to generate the most recently fit
; polynomial. 
;
; @keyword modelbuffer {in}{optional}{type=integer}{default=1} The
; buffer number of the data container to use to hold the model.
; Defaults to 1 if not supplied.
;
; @keyword nfit {in}{optional}{type=integer} Only use at most nfit
; parameters.  If !g.nfit is less then nfit, then only !g.nfit parameters will
; be used and a warning will be issued.
; 
; @keyword ok {out}{optional}{type=boolean} 1 on success, 0 on failure.
;
; @uses <a href="../toolbox/data_valid.html">data_valid</a>
; @uses <a href="getbasemodel.html">getbasemodel</a>
;
; @examples
; <pre>
; ; put the model in !g.s[1]
; bmodel
; ; put the model in !g.s[10], using nfit=5
; nfit,5
; bmodel, modelbuffer=10
; ; put the model with nfit=2 into buffer 11
; bmodel modelbuffer=11, nfit=2
; </pre>
;
; @version $Id: bmodel.pro,v 1.7 2006/05/16 19:26:42 bgarwood Exp $
;-
pro bmodel, modelbuffer=modelbuffer, nfit=nfit, ok=ok
    compile_opt idl2

    if (n_elements(modelbuffer) eq 0) then modelbuffer = 1

    maxbuffer = !g.line ? n_elements(!g.s) : n_elements(!g.c)

    if  (modelbuffer lt 0 or modelbuffer gt maxbuffer) then begin
        message, 'requested modelbuffer does not exist',/info
        return
    endif

    model_data = getbasemodel(nfit=nfit,ok=ok)

    if ok then begin
        copy,0,modelbuffer
        if (!g.line) then begin
            *!g.s[modelbuffer].data_ptr = model_data
        endif else begin
            *!g.c[modelbuffer].data_ptr = model_data
        endelse
    endif
end
