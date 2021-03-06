;+
; Calculate the mean Tsys using the data from two spectral line data
; containters, one with the CAL on and one with CAL off.
;
; <pre>
;  mean_tsys = tcal * mean(nocal) / (mean(withcal-nocal)) + tcal/2.0
; </pre>
; where nocal and withcal are the data values from dc_nocal and
; dc_withcal and tcal is as described below.
;
; <ul>
; <li>The outer 10% of all channels in both data containers are
; ignored.
; <li>Blanked data values are ignored.
; <li>The tcal value used here comes from the dc_nocal data container
; unless the user supplies a value in the tcal keyword.
; <li>The tcal value actually used is returned in used_tcal.
; </ul>
;
; <p>This is used by the GUIDE calibration routines and is
; encapsulated here to ensure consistency.  
;
; @param dc_nocal {in}{required}{type=spectrum data container} The
; data with no cal signal.
; @param dc_withcal {in}{required}{type=spectrum data container} The
; data with a cal signal.
; @keyword tcal {in}{optional}{type=float} A scalar value for the cal
; temperature (K).  If not supplied. dc_nocal.mean_tcal will be used.
; @keyword used_tcal {out}{optional}{type=float} The tcal value
; actually used.
;
; @version $Id: dcmeantsys.pro,v 1.2 2007/01/31 13:49:15 bgarwood Exp $
;-
function dcmeantsys, dc_nocal, dc_withcal, tcal=tcal,used_tcal=used_tcal
    compile_opt idl2

    if n_elements(tcal) eq 0 or n_elements(tcal) gt 1 then begin
        if n_elements(tcal) gt 1 then $
          message,'Vector tcal is not yet supported, sorry.  Ignoring user-supplied tcal.',/info
        used_tcal = dc_nocal.mean_tcal
    endif else begin
        used_tcal = tcal[0]
    endelse

    ; Use the inner 80% of data to calculate mean Tsys
    nchans = n_elements(*dc_nocal.data_ptr)
    pct10 = nchans/10
    pct90 = nchans - pct10

    meanTsys = mean((*dc_nocal.data_ptr)[pct10:pct90],/nan,/double) / $
               mean((*dc_withcal.data_ptr)[pct10:pct90] - (*dc_nocal.data_ptr)[pct10:pct90],/nan,/double) * $
               used_tcal + used_tcal/2.0

    return, meanTsys
end

