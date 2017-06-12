;+
; Interpolate across blanked channels in a data container.
;
; <p>This uses the IDL INTERPOL function to replace
; blanked values in the data container with unblanked values according
; to the interpolation method selected.
;
; <p>You can limit the range of channels to consider using bchan and
; echan.  When not supplied, all of the channels are used.
;
; <p>The default interpolation method is linear. The other
; interpolations may not be particularly useful across large gaps.
;
; <p>If all of the data in the requested range is good (unblanked)
; then no interpolation is done and this routine silently returns
; without changing anything in dc.
;
; <p>It is an error to request more than one interpolation method.
;
; @param dc {in}{required}{type=data container} The data container to
; use.
; @keyword bchan {in}{optional}{type=integer} The starting channel
; number.  If not specified, bchan=0.
; @keyword echan {in}{optional}{type=integer} The last channel number.
; If not specified use all channels from bchan to the end.
;
; @keyword linear {in}{optional}{type=boolean} When set, use the
; linear interpolation provided by INTERPOL.  This is the default
; interpolation when no other method is specified.
;
; @keyword quadratic {in}{optional}{type=boolean} When set, use the
; quadratic interpolation provided by INTERPOL.
;
; @keyword lsquadratic {in}{optional}{type=boolean} When set, use the
; lsquadratic (lest squares quadratic) interpolation provided by
; INTERPOL.
;
; @keyword spline {in}{optional}{type=boolean} When set, use the
; spline interpolation provided by INTERPOL.
;
; @keyword ok {out}{optional}{type=boolean} This is set to 1 on
; success or 0 on failure (e.g. bad arguments).
;
; @uses <a href="data_valid.html">data_valid</a>
;
; @version $Id: dcinterp.pro,v 1.1 2006/04/11 22:26:09 bgarwood Exp $
;-
pro dcinterp, dc, bchan=bchan, echan=echan, linear=linear, quadratic=quadratic, $
              lsquadratic=lsquadratic, spline=spline, ok=ok
    compile_opt idl2

    ok = 0
    if n_params() lt 1 then begin
        usage,'dcinterp'
        return
    endif

    nch = data_valid(dc)

    if nch le 0 then begin
        message,'No data in data container',/info
        return
    endif

    if n_elements(bchan) gt 0 then begin
        thisBchan = bchan
    endif else begin
        thisBchan = 0
    endelse

    if n_elements(echan) gt 0 then begin
        thisEchan = echan
    endif else begin
        thisEchan = nch-1
    endelse

    if thisBchan lt 0 or thisBchan ge (nch-1) then begin
        message,'bchan is out of range',/info
        return
    endif

    if thisEchan le 0 or thisEchan ge nch then begin
        message,'echan is out of range',/info
        return
    endif
    
    if thisEchan le thisBchan then begin
        message,'echan must be > bchan',/info
        return
    endif

    doLinear = keyword_set(linear)
    doQuad = keyword_set(quadratic)
    doLsquad = keyword_set(lsquadratic)
    doSpline = keyword_set(spline)

    doSum = doLinear + doQuad + doLsquad + doSpline
    if doSum gt 1 then begin
        message,'Only one of /linear, /quadratic, /lsquadratic and /spline can be requested at one time.',/info
        return
    endif
    if doSum eq 0 then doLinear = 1

    indx = lindgen(thisEchan-thisBchan+1) + thisBchan
    okLoc = where(finite((*dc.data_ptr)[indx]),okCount,complement=blankedLoc)
    if okCount le 0 then begin
        message,'No good channels found in the region of interest',/info
        return
    endif
    
    if okCount ne n_elements(indx) then begin
        ; only do this if there are bad channels
        okChans = indx[okLoc]
        blankedChans = indx[blankedLoc]

        minOK = 2 ; linear
        if doSpline or doLsquad then minOK = 4
        if doQuad then minOK = 3

        if okCount lt minOK then begin
            message,'Not enough good points in the region of interest, can not interpolate',/info
            return
        endif

        ; default for INTERPOL is linear
        (*dc.data_ptr)[blankedChans] = $
          interpol((*dc.data_ptr)[okChans],float(okChans),float(blankedChans),$
                   spline=doSpline, quadratic=doQuad, $
                   lsquadratic=doLsquad)
    endif else begin
        print,'nothing to interpolate'
    endelse
    ok = 1
end
 