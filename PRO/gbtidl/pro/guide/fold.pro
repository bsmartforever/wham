;+
; Average two parts of in-band frequency switching (signal and
; reference phases, with the cal-switching phases already calibrated).
;
; <p>Typically this happens during or after getfs.  The two data
; containers are assumed to be in 0 and 1.  It does not matter which
; data container contains which part since their relative distance in
; frequency is used to determine how one is shifted to align with the
; other. The result is always placed in data container 0.  If the two
; data containers do not overlap in frequency, then there is nothing
; to fold and an error message will be printed.
;
; <p>The "ref" data container is shifted to align in sky frequency
; with the "sig" data container using  <a href="../toolbox/dcshift.html">dcshift</a> and the two data 
; containers are averaged - weighting each by the inverse of square of
; their system temperatures.  The system temperature in the result is
; the weighted average of the two system temperatures.
;
; @param sig {in}{optional}{type=integer}{default=0} The global buffer
; number to use as the signal part in the average.  The result will
; contain a copy of this data container's header information except
; for tsys as described above.  The data at sig is overwritten by the result
; if sig is 0 (the default)
;
; @param ref {in}{optional}{type=integer}{default=1} The global buffer
; number to use as the reference part in the average.
; This data is shifted using <a href="../toolbox/dcshift.html">dcshift</a> to align with
; "sig" before averaging.  The data in ref are never altered by this procedure.
;
; @keyword ftol {in}{optional}{type=double}{default=0.005} The fractional
; channel shift tolerance.  If the fractional part of the channel
; shift necessary to align the two parts is less than this value, no
; fractional shift as described in the documentation for 
; <a href="../toolbox/dcshift.html">dcshift</a> will be done.  It might be useful to
; turn off the fractional shift because it can cause aliases and
; ringing in the case of very strong lines or other sharp features.
; If ftol > 0.5 no fractional shifts will be done.
;
; @examples
; <pre>
;    getfs, 64, /nofold
;    fold, ftol=1.0       ; no fractional shifting is done
; </pre>
;
; @uses <a href="../toolbox/dcfold.html">dcfold</a>
; @uses <a href="../toolbox/data_free.html">data_free</a>
; @uses <a href="set_data_container.html">set_data_container</a>
;
; @version $Id: fold.pro,v 1.10 2006/05/16 19:26:42 bgarwood Exp $
;-
pro fold, sig, ref, ftol=ftol
    compile_opt idl2

    if not !g.line then begin
        message,'FOLD does not work on continuum data, sorry.',/info
        return
    endif

    catch, error_status
    if (error_status ne 0) then begin
        ; print out the error and return
        help,/last_message,output=errtext
        print,errtext[0]
        return
    endif

    if n_elements(sig) eq 0 then sig=0
    if n_elements(ref) eq 0 then ref=1
    if n_elements(ftol) eq 0 then ftol=0.005

    if (sig eq ref) then begin
        message,'Sig and ref must be different buffers',/info
        return
    endif

    nmaxind = n_elements(!g.s)
    if (sig lt 0 or sig gt nmaxind or ref lt 0 or ref gt nmaxind) then begin
        message,string(nmaxind,format='("sig and ref must be >= 0 and < ",i2)'),/info
        return
    endif

    new = dcfold(!g.s[sig],!g.s[ref])
    set_data_container,new
    data_free,new
end
