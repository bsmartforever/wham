;+
; Average two parts of in-band frequency switching (signal and
; reference phases, with the cal-switching phases already calibrated).
; Typically this happens after getfs.  It does not matter which data container
; containers which part since their relative distance in frequency
; is used to determine how one is shifted to align with the other.
; The returned result is a new data container that the user must
; eventually free using <a href="data_free.html">data_free</a> in order
; to avoid any memory leaks.  sig and ref must have the same number of
; channels and the same spacing between channels.  If sig and ref are
; already aligned, then this is a simple average (a warning message
; will be printed).  If the shift necessary to align ref with sig is
; more than the total number of channels, there is no overlap and this
; procedure will print an error message and return without altering
; sig and ref.
;
; <p> The "ref" data container is shifted to align in sky
; frequency with the "sig" data container using 
; <a href="dcshift.html">dcshift</a> and the two data containers are averaged
; - weighting each by the inverse of square of their system temperatures.  The
; system temperature in the result is the weighted average of the two system
; temperatures.
;
; @param sig {in}{out}{required}{type=spectrum} The data
; container to use as the signal part in the average.
;
; @param ref {in}{out}{required}{type=spectrum} The data container to 
; use as the reference part in the average.
; This data is shifted using <a href="dcshift.html">dcshift</a> to align with
; "sig" before averaging. 
;
; @keyword ftol {in}{optional}{type=double}{default=0.005} The fractional
; channel shift tolerance.  If the fractional part of the channel
; shift necessary to align the two parts is less than this value, no
; fractional shift as described in the documentatio for 
; <a href="dcshift.html">dcshift</a> will be done.  It might be useful to
; turn off the fractional shift because it can cause aliases and
; ringing in the case of very strong lines or other sharp features.
; If ftol > 0.5 no fractional shifts will be done.
;
; @returns data container.  The user is responsible for freeing this.
; returns -1 on error.
;
; @uses <a href="dcshift.html">dcshift</a>
; @uses <a href="data_free.html">data_free</a>
; @uses <a href="data_valid.html">data_valid</a>
;
; @version $Id: dcfold.pro,v 1.8 2006/04/10 16:32:15 bgarwood Exp $
;-
function dcfold, sig, ref, ftol=ftol
    compile_opt idl2

    catch, error_status
    if (error_status ne 0) then begin
        ; print out the error and return
        help,/last_message,output=errtext
        print,errtext[0]
        return, -1
    endif

    if n_params() ne 2 then begin
        usage,'dcfold'
        return, -1
    endif

    if n_elements(ftol) eq 0 then ftol=0.005

    nsig = data_valid(sig)
    if (nsig le 0) then begin
        message, 'sig data is empty or invalid'
    endif

    nref = data_valid(ref)
    if (nref le 0) then begin
        message, 'ref data is empty or invalid'
    endif

    if (nref ne nsig) then begin
        message,'sig and ref have different numbers of channels, can not fold',/info
        return,-1
    endif

    if (sig.frequency_interval ne ref.frequency_interval) then begin
        message,'sig and ref have different channel spacings, can not fold',/info
        return,-1
    endif

    sigF0 = chantofreq(sig,0.d)
    refF0 = chantofreq(ref,0.d)
    chan_shift = (refF0-sigF0)/sig.frequency_interval
    
    if chan_shift eq 0.0 then begin
        message,'Frequency switch is 0 channels - result is an average of sig and ref',/info
    endif

    if abs(chan_shift) ge nsig then begin
        message, 'Frequency switch is > number of channels, no overlap'
    endif

    ; dcshift shifts things in place
    ; copy ref to where the result will go and shift it
    data_copy,ref,result
    dcshift,result,chan_shift,ftol=ftol

    ; average them together
    a = {accum_struct}
    dcaccum,a,sig
    dcaccum,a,result
    accumave,a,result,/quiet,count=count
    if (count ne 2) then begin
        if count lt 0 then begin
            message,'unexpected problems in averaging 2 parts of data during fold',/info
            if data_valid(result) ge 0 then data_free, result
            return,-1
        endif else begin
            ; one of these was all NaNs, return it as the result!
            if not finite((*sig.data_ptr)[0]) then begin
                data_copy, sig, result
            endif else begin
                data_copy, ref, result
            endelse
        endelse
    endif
        
    accumclear,a

    return, result
end

