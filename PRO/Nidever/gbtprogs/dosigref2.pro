;+
; Combine signal and reference data containers.
;
; <p>The result is 
; <pre>
; ((dcsig-dcref)/dcref) * dcref.tsys 
; </pre>
; where dcsig and dcref are the data values in the signal and
; reference data containers.
; The tsys in the result, dcresult.tsys, is equal to dcref.tsys.  
; <p>
; The exposure time of the result is = t_sig * t_ref * smoothref /
; (t_sig+t_ref*smoothref),  where t_sig and t_ref are the exposures of
; the signal and reference data container and where smoothref=1 in
; when smoothref has not been supplied.  That is the effective integration
; time appropriate for use in the radiometer equation.  Note that in
; the two limits of smoothref (1=no smoothing and large smoothing), if
; t_sig = t_ref then exposure = 1/2 * t_sig or exposure = t_sig.  With
; no smothing, the noise increases due to the subtraction of an
; equally noisy reference spectrum.  With smoothing, the noise is as
; it would be for just the signal spectrum.  Smoothing should be done
; with caution, though, because it can emphasize systematic glitches
; present in both signal and reference spectra that are not fully
; subtracted as a consequence of the smoothing.  
; <p>All of the other header values are copied from dcsig.
;
; <p>If smoothref is set, the dcref values are smoothed using the IDL
; smooth function.  This does a simple boxcar smooth where the smoothref
; parameter is the width of the boxcar. smoothref is only used if it
; is larger than 1.  In certain cases this can improve the signal to
; noise ratio, but it may degrade baseline shapes and artificially
; emphasize spectrometer glitches.  Use with care.  A value of
; smoothref=16 is often a good choice 
;
; <p>The dcresult data container is created as necessary.  If it
; already exists, the internal pointer will be reused.  It is the
; responsibility of the calling procedure or functiosdn to free that
; pointer using data_free.  Failure to do that will result in a memory
; leak.
;
; @param dcresult {out}{required}{type=data_container} The result.
; @param dcsig {in}{required}{type=data_container} The signal data
; container.
; @param dcref {in}{required}{type=data_container} The reference
; data container.
; @param smoothref {in}{optional}{type=integer} Boxcar smooth width
; for reference spectrum.  No smoothing if not supplied or if value is
; less than or equal to 1.
; 
; @version $Id
;-
pro dosigref2,dcresult,dcsig,dcref,smoothref,rfi=rfi
    compile_opt idl2

    ok = dcpaircheck(dcsig,dcref)
    if not ok then return

    ; copy the headers from dcresult
    data_copy,dcsig,dcresult
   
    refdata = *dcref.data_ptr

    ; interpolate over the RFI in the refdata
    lo = freqtochan(dcref,rfi.flo)
    hi = freqtochan(dcref,rfi.fhi)    
    nchan = n_elements(*dcref.data_ptr)
    gd = where(lo ge 0 and hi lt nchan-1,ngd)
    for i=0,ngd-1 do refdata[round(lo[gd[i]]):round(hi[gd[i]])] = !values.f_nan
    for i=0,ngd-1 do begin
      ilo = round(lo[gd[i]])
      ihi = round(hi[gd[i]])
      neilo = (ilo-100)>0
      neihi = (ihi+100)<(nchan-1)
      med = median(refdata[neilo:neihi],/even)  ; ignores NAN
      refdata[ilo:ihi] = med
    endfor

    ; smoothing
    nsmooth = 1
    if n_elements(smoothref) gt 0 then begin
        if smoothref gt 1 then begin
            refdata = smooth(refdata,smoothref,/nan,/edge_truncate)
            nsmooth = smoothref
        endif 
    endif


    ; blank the sig
    sig = *dcsig.data_ptr
    for i=0,n_elements(rfi)-1 do begin
      slo = round( freqtochan(dcsig,rfi[i].flo) )
      shi = round( freqtochan(dcsig,rfi[i].fhi) )
      if slo ge 0 and shi le nchan-1 then sig[slo:shi]=!values.f_nan
    endfor

; KEEP TRACK OF WHAT WAS INTERPOLATED ???

    *dcresult.data_ptr = ((sig - refdata)/refdata) * dcref.tsys
    ;*dcresult.data_ptr = ((*dcsig.data_ptr - refdata)/refdata) * dcref.tsys
    dcresult.tsys = dcref.tsys
    dcresult.exposure = dcsig.exposure*dcref.exposure*nsmooth/(dcsig.exposure+dcref.exposure*nsmooth)

end
