;+
; Procedure to shift data in a data container by a given number of
; channels.
;
; <p>Shift the data in the data container by the given number of channels 
; (which may be a floating point number containing fractional
; channels) such that data in channel i before the shift is found in 
; channel i+offset after the shift.  The data's reference channel is
; also shifted so that features should remain stationary when
; displayed except when the x-axis is channels.  Shifted data is
; replaced with zeros unless the /wrap keyword is set in which case
; the data wraps around as necessary during the shift. Data are first
; shifted by the nearest integer number of channels and then the
; result is shifted by the remaining fractional channel if that
; fraction channel is more than ftol.  
;
; <p>Currently, the default fractional shifting is
; done using an FFT windowed using a Welch function to reduce ringing
; as a result of the FFT.  Set ftol to a number >= 1 to turn off all
; fractional shifting.  See below for alternative methods.  FFT is the
; default because that is the method that UniPOPS used in its 
; SHIFT verb.
;
; <p> Use one of xshift, vshift, or fshift to calculate the offset to
; align this data with data in an ongoing accumulation.
;
; <p> Windowing using the Welsh function reduces ringing at the
; expense of the high-order terms in the FFT.  This very slightly
; broadens features.  This can be turned off using the /nowelsh
; keyword.
;
; <p> The data are padded with 0s to the next higher power of two as
; an intermediate step (the final result will have the original size
; of the data).  This is done to reduce ringing due to any
; discontinuities between the two ends of the data.  This can be
; turned off using the /nopad keyword. If there are any bad data
; points, they are interpolated using a linear interpolation prior to
; the FFT and then reblanked after the FFT.
;
; <p> linear, quadratic, least squares quadratic, spline, and cubic
; interpolation are all available as alternatives to the default FFT
; based interpolation for the fractional part of the shift.  The first
; 4 of these methods use the INTERPOL function and the cubic method
; uses the INTERPOLATE functions.  Please see the IDL help on those
; functions for more information about them.  The cubic function is a
; close approximation to a sinc function.
;
; <p> In some limited testing with GBT data, all of these
; interpolation methods agree reasonably well with each other. The
; linear interpolation is, not surprisingly, the fastest since it only
; uses 2 data points for each interpolated point.  The quadratic and
; cubic methods both use 3 points for each interpolated point and both
; of those methods are not quite 2 times slower than the linear
; method.  The FFT method (the default) is about another factor of 2
; slower.  Spline and least squares quadratic both use 4 data points
; for each interpolated point and they are substantially slower.
; Spline takes about 20 times longer and lsquadratic takes about 100
; times as long as the linear interpolation.
;
; <p>If none of /linear, /spline, /quadratic, /lsquadratic, or /cubic
; are specified then an FFT is used for the fractional shift.  It is
; an error to use more than one of these flags in the same call.
;
; @param dc {in}{out}{required}{type=spectrum} The data container to
; shift.  The shift is done in place.
;
; @param offset {in}{required}{type=floating point} The number of
; channels to shift the data (positive shifts things towards higher
; channels, negative shifts things towards lower channels).
;
; @keyword wrap {in}{optional}{type=boolean} Data shifted off one end
; of the array appears on the other end of the array (it wraps around
; as a result of the shift) when this is set.  Otherwise, as data is
; shifted it is blanked (replaced by NaNs) and data shifted off the 
; end is lost.
; 
; @keyword ftol {in}{optional}{type=floating point}{default=0.01}
; Fractional shifts (the non-integer portion of offset) are only done
; when they are larger than ftol.  Set this value to >= 1.0 to turn
; off all fractional shifts.
;
; @keyword linear {in}{optional}{type=boolean} When set, use the
; linear interpolation provided by INTERPOL for any fractional shift
; larger than ftol.
;
; @keyword quadratic {in}{optional}{type=boolean} When set, use the
; quadratic interpolation provided by INTERPOL for any fractional
; shift larger than ftol.
;
; @keyword lsquadratic {in}{optional}{type=boolean} When set, use the
; lsquadratic (lest squares quadratic) interpolation provided by
; INTERPOL for any fractional shift larger than ftol.
;
; @keyword spline {in}{optional}{type=boolean} When set, use the
; spline interpolation provided by INTERPOL for any fractional shift
; larger than ftol.
;
; @keyword cubic {in}{optional}{type=boolean} When set, use the cubic
; interpolation provided by INTERPOLATE for any fractional shift
; larger than ftol.  The value of the CUBIC keyword in the INTERPOLATE
; call is set to -0.5.
; 
; @keyword nowelsh {in}{optional}{type=boolean} When set, the shifted
; data is NOT windowed using the Welsh function.  This is ignored when
; a non-FFT-based fraction shift is done
;
; @keyword nopad {in}{optional}{type=boolean} When set, the data is
; NOT padded with 0s to the next higher power of 2 prior to the FFT
; and shift.  The data are never padded for the non-FFT-based
; fractional shifts.
;
; @keyword ok {out}{optional}{type=boolean} This is set to 1 on
; success or 0 on failure (e.g. bad arguments).
;
; @uses <a href="data_valid.html">data_valid</a>
;
; @version $Id: dcshift.pro,v 1.5 2005/11/10 20:28:46 bgarwood Exp $
;-
pro dcshift, dc, offset, wrap=wrap, ftol=ftol, linear=linear, $
             quadratic=quadratic, lsquadratic=lsquadratic, spline=spline, $
             cubic=cubic, nowelsh=nowelsh, nopad=nopad, ok=ok
    compile_opt idl2

    on_error, 2

    ok = 0

    if n_params() ne 2 then begin
        usage, 'dcshift'
        return
    endif

    nch = data_valid(dc,name=name)
    if name ne "SPECTRUM_STRUCT" then begin
        message,"dcshift can not be used with continuum data, sorry.",/info
        return
    endif
    
    if nch le 0 then begin
        message,'No data in data container',/info
        return
    endif

    if abs(offset) ge nch then begin
        message,'offset is more than the number of channels, can not shift',/info
        return
    endif

    if not keyword_set(nowelsh) then nowelsh = 0
    if not keyword_set(nopad) then nopad = 0
    if not keyword_set(linear) then linear = 0
    if not keyword_set(quadratic) then quadratic = 0
    if not keyword_set(lsquadratic) then lsquadratic = 0
    if not keyword_set(spline) then spline = 0
    if not keyword_set(cubic) then cubic = 0

    if (linear + quadratic + lsquadratic + spline + cubic) gt 1 then begin
        message,'Only one of /linear, /quadratic, /lsquadratic, /spline, and /cubic can be specified at one time',/info
        return
    endif

    ishift = round(offset)
    fshift = offset-ishift

    if abs(ishift) gt 0 then begin
        ; use builtin shift - which wraps
        *dc.data_ptr = shift(*dc.data_ptr,ishift)

        if not keyword_set(wrap) then begin
            ; zero out the wrapped stuff
            if ishift gt 0 then begin
                ibeg = 0
                iend = ishift-1
            endif else begin
                iend = nch -1
                ibeg = iend + ishift - 1
            endelse
            (*dc.data_ptr)[ibeg:iend] = !values.f_nan
        endif
    endif

    ; do fractional shift here
    if n_elements(ftol) eq 0 then ftol = 0.01

    if (abs(fshift) gt ftol) then begin
        if linear or spline or quadratic or lsquadratic then begin
            curChans = lindgen(nch)
            newChans = curChans - fshift ; new channels in old channel units
            *dc.data_ptr = interpol(*dc.data_ptr, curChans, newChans, spline=spline, $
                                    quadratic=quadratic, lsquadratic=lsquadratic)
        endif else begin
            if cubic then begin
                *dc.data_ptr = interpolate(*dc.data_ptr, findgen(nch) - fshift, cubic=-0.5)
            endif else begin
                ; FFT happens here
                ; watch for blanked data
                okChans = where(finite(*dc.data_ptr),count,complement=blankedChans)
                doFractShift = 1
                if count lt nch then begin
                    if count le 0 then begin
                        ; all the data is bad, no point in shifting it
                        doFractShift = 0
                    endif else begin
                        (*dc.data_ptr)[blankedChans] = interpol((*dc.data_ptr)[okChans],okChans,blankedChans)
                    endelse
                endif

                if doFractShift then begin
                    if not nopad then begin
                        ; expand the data to next power of 2 to 
                        ; prevent aliasing
                        pow2 = round(alog10(nch)/alog10(2))
                        pow2 += 1
                        newsize = 2^pow2
                    endif else begin
                        newsize = nch
                    endelse
            
                    apad = dblarr(newsize)
                    npad = newsize - nch
                    nskip = round(npad/2.0)
                    apad[nskip:(nskip+nch-1)] = *dc.data_ptr
                    ; use the two end values to pad out the region
                    apad[0:(nskip-1)] = apad[nskip]
                    apad[(nskip+nch):(newsize-1)] = apad[nskip+nch-1]

                    ; fft the data
                    fta = fft(apad,/inverse,/overwrite)
            
                    ; add in the phase shift and do welch windowing
                    phshift = 2.0d * !dpi * fshift/double(newsize)

                    amp = sqrt(real_part(fta)^2 + imaginary(fta)^2)
                    phs = atan(imaginary(fta),real_part(fta))

                    ; do each half separately
                    half = (newsize/2)
                    farr = findgen(half)
                    ; this multiplies by the Welch function
                    if not nowelsh then amp[0:(half-1)] *= (1.0 - (farr/float(newsize/2))^2)
                    ; and this does the shift
                    phs[0:(half-1)] += phshift*farr

                    farr += float(half - newsize)
                    ; this multiplies by the Welch function
                    if not nowelsh then amp[half:(newsize-1)] *= (1.0 - (farr/float(newsize/2))^2)
                    ; and this does this shift
                    phs[half:(newsize-1)] += phshift*farr

                    ; return to complex form
                    fta = complex(amp*cos(phs),amp*sin(phs))

                    ; and back to frequency domain
                    apad = fft(fta,/overwrite)

                    ; and back to a
                    *dc.data_ptr = real_part(apad[nskip:(nskip+nch-1)])
                    ; if there are any blankedChans, reblank the data
                    if count lt nch then begin
                        (*dc.data_ptr)[blankedChans] = !values.f_nan
                        ; expand the blanked region by one channel in 
                        ; the direction of the shift
                        if fshift gt 0 then begin
                            lastBlanked = blankedChans[n_elements(blankedChans)-1]
                            if lastBlanked lt (nch-2) then (*dc.data_ptr)[lastBlanked+1] = !values.f_nan
                        endif else begin
                            if blankedChans[0] gt 0 then (*dc.data_ptr)[blankedChans[0]-1] = !values.f_nan
                        endelse
                    endif    ;  end of reblanking section
                endif ; end of doFractShift section
            endelse ; end of FFT section
        endelse ; end of non INTERPOL section
    endif else begin ; end of fraction shift secion
        fshift = 0.0
    endelse
    
    ; and reset the reference pixel
    trueShift = double(ishift) + fshift
    dc.reference_channel += trueShift

    ok = 1
end
