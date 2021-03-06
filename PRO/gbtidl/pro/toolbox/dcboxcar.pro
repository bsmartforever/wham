;+
; Smooth a data container with a boxcar smoothing of a certain
; width, in channels. For odd width, this uses the built-in idl
; SMOOTH function.  For even widths this uses
; <a href="../toolbox/doboxcar1d.html">doboxcar1d</a> and the
; reference channel is moved left by 1/2 channel width.
;
; Replaces the contents of the data being smoothed with the smoothed
; data.  Use the GUIDE procedure, BOXCAR, to smoothing data containers
; in the !g structure.
;
; <p>For spectrum data containers, the frequency_resolution is set 
; using <a href="estboxres.html">estboxres</a> 
;
; @param dc {in}{required}{type=data container} The data container to
; be smoothed.  The data values are modified by this procedure.
; 
; @param width {in}{required}{type=integer} Width of boxcar in
; channels. 
;
; @keyword decimate {in}{optional}{type=boolean} If set, the data
; container is reduced - taking every width channels starting at
; channel 0.
;
; @uses <a href="doboxcar1d.html">doboxcar1d</a>
; @uses <a href="doextract.html">dcextract</a>
;
; @version $Id: dcboxcar.pro,v 1.5 2005/11/16 19:22:26 bgarwood Exp $
;-
pro dcboxcar, dc, width, decimate=decimate
    compile_opt idl2

    if n_elements(dc) eq 0 or n_elements(width) eq 0 then begin
        message,'Usage: dcboxcar, dc, width[, decimate=decimate]',/info
        return
    end

    nch=data_valid(dc,name=name)
    if nch le 0 then begin
        message, 'dc contains no valid data.',/info
        return
    endif

    if (width le 0 or width gt nch) then begin
        message,string(nch,format='("Width must be between 1 and ",i)'),/info
        return
    endif

    *dc.data_ptr = doboxcar1d(*dc.data_ptr,width,/edge_truncate,/nan)
    if width mod 2 eq 0 and name eq 'SPECTRUM_STRUCT' then begin
        dc.reference_channel -= 0.5
    endif

    if name eq 'SPECTRUM_STRUCT' then begin
        chanRes = dc.frequency_resolution / abs(dc.frequency_interval)
        chanRes = estboxres(width,chanRes)
        dc.frequency_resolution = chanRes * abs(dc.frequency_interval)
    endif

    if keyword_set(decimate) then begin
        newdc = dcextract(dc,0,(nch-1),width)
        data_copy,newdc,dc
        data_free,newdc
    endif
end
