;+
; Add a data container into an ongoing accumulation in a given
; accum_struct structure.
;
; <p>If this is the first item to be "accum"ed, it will also be used as a template,
; stored in accumbuf.template, to be used by
; <a href="accumave.html">accumave</a>.
;
; <p>If the polarization of any items being accumed does not match
; that of template, the polarization of the template is changed to 'I'.
;
; <p>This combines the UniPOPS functionality of ACCUM and SUM.  The SUM
; name is already in use in IDL.
;
; <p>This is primarily for use inside procedures and functions where
; it is useful to average several data containers without disturbing
; the public data containers in the guide structure.  Most users will
; find using <a href="../guide/accum.html">accum</a> preferable to
; using dcaccum.
;
; <p>The data are  : sum(weight*data)
; <p>The times are : sum(duration), sum(exposure)
; <p>The weight is : sum(weight) one weight sum per channel.
; <p>The tsys is   : sqrt(sum(max(weight)*Tsys^2))
; <p>The frequency resolution is the maximum of all f_res values used
; during the accumulation.
;
; <p>A warning message is shown if either the frequency_resolution or
; the frequency_interval do not match that in an already on-going
; accumulation.  If the quiet flag is on, then this message is
; suppressed.  In either case, the accumulation proceeds.
;
; <p>If a weight is not supplied, it will be exposure*frequency_resolution/tsys^2
;
; <p>weight can either be a scalar or it can be a vector having the same
; number of elements as the data in dc.
;
; <p>If all of data is blanked (not a number) then it is completely
; ignored and the accumulated weight, times, and system temperatures
; are unchanged.  If individual regions are blanked then the weight at
; those channels is 0.  When an average is requested (accumave) this
; weight array is used to rescale the data.  That weight array is also
; available when the average is requested.  If that weight array is
; used as input in a future average, the averaging can continue from
; the same point as before.
;
; @param accumbuf {in}{out}{required}{type=accum_struct}  The
; structure containing the accumulation that you want to add to.
;
; @param dc {in}{required}{type=spectrum} The data container to
; accum.
;
; @keyword quiet {in}{optional}{type=boolean} If set, suppress warning
; messages about frequency resolution and interval not matching values
; in accumbuf.
;
; @keyword weight {in}{optional}{type=float} The weight to use for this
; data.  If this is not set, a weight of exposure*frequency_resolution/tsys^2 
; will be used.  Weight can either be a scalar (uniform weight across
; at all channels) or it can be an array having the same number of
; elements as the data in dc.
;
; @examples
;    average some data
; <pre>
;   a = {accum_struct}
;   accumclear,a  ; not necessary here, but a good habit to follow
;   ; get several records at once
;   s = !g.lineoutio->get_spectra(index=0)
;   dcaccum,a,s
;   data_free,s ; be sure to clean up, else leaks memory
;   s = !g.lineoutio->get_spectra(index=1)
;   dv = dcvshift, a, s ; align in velocity
;   dcshift, a, dv ; actually do the shift to align
;   dcaccum,a,s
;   data_free, s
;   accumave,a,s
;   show, s
;   data_free, s  
; </pre>
; See <a href="ave.html">ave</a> for additional examples.
;
; @uses <a href="../../devel/toolbox/accumulate.html">accumulate</a>
; @uses <a href="data_valid.html">data_valid</a>
; @uses <a href="data_free.html">data_free</a>
;
; @version $Id: dcaccum.pro,v 1.11 2006/02/23 22:07:51 bgarwood Exp $
;-
pro dcaccum, accumbuf, dc, weight=weight, quiet=quiet
    compile_opt idl2

    on_error, 2

    if n_params() ne 2 then begin
        usage,'dcaccum'
        return
    endif

    if (size(accumbuf,/type) ne 8 or tag_names(accumbuf,/structure_name) ne "ACCUM_STRUCT") then begin
        message,"accumbuf is not an accum_struct structure",/info
        return
    endif

    dataOk = data_valid(dc,name=name)
    if dataOk le 0 then begin
        message,'dc is empty or invalid, can not continue.',/info
        return
    endif
    if name ne 'SPECTRUM_STRUCT' then begin
        message,'data container is not a SPECTRUM_STRUCT, only spectral line data can be accumed, sorry.',/info
        return
    endif

    if (accumbuf.n eq 0) then begin
        data_free, accumbuf.template
        accumbuf.template = dc
        ; now copy the pointer values, making new pointers here
        accumbuf.template.data_ptr = ptr_new(*dc.data_ptr)
    endif else begin
        if dc.polarization ne accumbuf.template.polarization then accumbuf.template.polarization = 'I'
    endelse

    ; watch for duration of 0.0, early data has that
    tdur = dc.duration
    if dc.duration le 0.0 then tdur = dc.exposure

    accumulate, accumbuf, *dc.data_ptr, dc.exposure, dc.tsys, dc.frequency_resolution,$
                tdur, dc.frequency_interval, wt=weight, quiet=quiet
end
