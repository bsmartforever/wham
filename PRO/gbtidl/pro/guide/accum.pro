;+
; This procedure adds a data container to an accum buffer, in
; preparation for averaging.  The primary data container (!g.s[0], the
; PDC) is used by default but an alternate data container can be
; specified using the 'dc' keyword.
; 
; <p>The first data container accum'ed in a buffer is used as a
; template for that buffer and subsequent data containers accum'ed to
; that buffer must match in number of channels.  On subsequent uses of
; accum, a warning is printed if the channel spacing
; (frequency_interval) or frequency resolution differs from the values
; already in the buffer.  The accumulation proceeds even when a
; warning is printed.  Use  <a href="resample.html">resample</a> to change the frequency_interval 
; and <a href="gsmooth.html">gsmooth</a> to change the frequency_resolution.
;
; <p> There are four accum buffers available to this and related 
; GUIDE-layer procedures.  Users can use these to have several
; averages proceeding simultaneously, but separately, when it is useful
; to do so (e.g. polarizations).  Use the accumnum keyword to specify
; which buffer to use (defaults to 0).
;
; <p>The default weight for each spectrum is
; exposure*frequency_resolution/Tsys^2.  A different weight can be
; given using the weight keyword.  Alternatively, each channel can be 
; given a separate weight by providing a vector of weights in the
; weight argument.  In that case, the number of elements in weight
; must be the same as in the data.  This can be used to re-start an
; accum from a previous average where the weight was retrieved from
; that average.  See <a href="ave.html">ave</a> for more details.  See <a href="../toolbox/dcaccum.html">dcaccum</a> for 
; additional information on how the header parameters are weighted
; during the accumulation.
;
; <p>Blanked channels (Not a Number data values) are excluded from the
; average. An entirely blanked spectrum (all values are NaNs - e.g. bad
; lags from the GBT spectrometer) is completely ignored by the
; accumulation (the contents of that accum buffer are unchanged).
;
; @param accumnum {in}{optional}{type=integer}{default=0} accum buffer.  
; Defaults to the primary buffer (accumnum = 0).  There are 4 buffers 
; in all so this value must be between 0 and 3, inclusive.
;
; @keyword weight {in}{optional}{type=float} The weight to use for 
; averaging this data.  If not set, a weight of
; exposure*frequency_resolution/Tsys^2 is used.  This can also be a
; vector of weights, one per channel.
;
; @keyword dc {in}{optional}{type=spectrum or integer} The data
; container to accum.  If not supplied, use the PDC.  If this is an
; integer, then use the data container at that buffer number in !g.s.
;
; @examples
; A simple averaging operation:
; <pre>
;   sclear
;   getrec,1
;   accum
;   getrec,2
;   accum
;   ave
; </pre>
; <p> 
; Average two polarizations separately for some position switched scans
; <pre>
;   sclear                  ; clears accum buffer 0
;   sclear, 1               ; clears accum buffers 1
;   getps,32,plnum=0
;   accum, 0
;   getps,32,plnum=1
;   accum, 1
;   getps,34,plnum=0
;   accum, 0
;   getps,34,plnum=1
;   accum, 1
;   ave,1                   ; Average plnum=1 data and store
;                           ;  the result in the PDC
;   copy,0,1                ; Copy the result to DC 1
;   ave, 0                  ; Average plnum=0 data
;   oshow, 1                ; Overplot the plnum=1 average
; </pre>
; <p>
; Average some data, remember the vector weights at the average, and
; then average some more data.
; <pre>
;   sclear
;   getrec,1
;   accum
;   getrec,2
;   accum
;   ave, wtarray=wtave1_3
;   copy, 0, 10              ; save average for later use
;   sclear                   ; no necessary, but better to be sure
;   getrec,3
;   accum
;   getrec,4
;   accum
;   ave, wtarray=wtave3_4
;   copy, 0, 11              ; save this for later use
;   ; other things could happen here
;   ; average 11 and 10 using appropriate weighting
;   ; they might not be scalars if some part of each was flagged or
;   ; blanked
;   sclear
;   accum, dc=10, weight=wtave1_3
;   accum, dc=11, weight=wtave3_4
;   ave, wtarray=wtave_all4
; </pre>
;   
; @uses <a href="../toolbox/data_valid.html">data_valid</a>
; @uses <a href="../toolbox/dcaccum.html">dcaccum</a>
;
; @version $Id: accum.pro,v 1.23 2006/05/15 19:20:55 bgarwood Exp $
;-
pro accum, accumnum, weight=weight, dc=dc
    compile_opt idl2

    on_error, 2

    if not !g.line then begin
        message,'Can not accumulate continuum data, sorry.',/info
        return
    endif

    if n_elements(accumnum) eq 0 then accumnum = 0

    if (accumnum lt 0 or accumnum gt 3) then begin
        message,'accumnum must be in the range 0 to 3',/info
        return
    endif

    accumbuf = !g.accumbuf[accumnum]

    if n_elements(dc) eq 0 then begin
        thisdc = !g.s[0]
    endif else begin
        thisdc = dc
    endelse

    if size(thisdc,/type) ne 8 then begin
        ; interpret it as a buffer number
        if thisdc lt 0 or thisdc ge n_elements(!g.s) then begin
            message,string(n_elements(!g.s),format='("dc buffer must be between 0 and ",i2)'),/info
            return
        endif
        thisdc = !g.s[thisdc]
    endif
    dataOk = data_valid(thisdc,name=name)
    if dataOk le 0 then begin
        message,'No valid data found to accum',/info
        return
    endif
    if name ne 'SPECTRUM_STRUCT' then begin
        message,'data container is not a SPECTRUM_STRUCT',/info
        return
    endif

    dcaccum, accumbuf, thisdc, weight=weight

    !g.accumbuf[accumnum] = accumbuf

end
