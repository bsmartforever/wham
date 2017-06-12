;+
; Procedure to measure the area, width, and velocity of a galaxy
; profile.  
;
; <p>The derived area, width and velocity are displayed unless the
; /quiet keyword is used.  These values can also be returned using the
; ret keyword.  The value of ret is a 3-element array containing the
; area, width, and velocity in that order.   The width and velocity
; are always given (shown and returned) in km/s and the area is given
; in data units * km/s.
;
; <p>This is a front-end to <a href="../toolbox/awv.html">awv</a>.  The source code there originated at 
; Arecibo.  See the comments there for a description of the 3 possibly
; values for mode and how fract and rms are used for each mode.
;
; <p>The data that is currently displayed in the plotter is used in
; this procedure.  The displayed x-axis can be any type.  The x-axis
; values are be converted to velocity taking into account any
; previously set velocity offset (even if the displayed axis is not
; the velocity axis). If there is no data displayed in the plotter, the
; contents of the primary data container (buffer 0) are used.
;
; <p>If the region of interest is not specified, the user is asked to
; mark it interactively.
;
; <p>A baseline should have been removed prior to using this function.
;
; <p>brange, erange, lefthorn, and righthorn are given in the
; currently display x-axis units unless the chan keyword is set.
;
; <p><B>Originally Contributed By: Karen O'Neil, NRAO-GB and Bob Garwood,
; NRAO-CV</B>
;
; @param mode {in}{required}{type=integer}  Must be one of 1,2,or 3.
; See the comments in <a href="../toolbox/awv.html">awv</a> for more
; details.
;
; @param fract {in}{required}{type=float} Fraction of peak or mean used in
; locating the edges of the galaxy profile.  See the comments in 
; <a href="../toolbox/awv.html">awv</a> for more details.  Note that in
; mode 3, the user is asked to mark the two horns of the galaxy
; profile with the cursor unless the optional lefthorn and righthorn
; keywords are used.
;
; @keyword brange {in}{optional}{type=integer} Starting value of the
; region of interest in x-axis units unless chan is set.
;
; @keyword erange {in}{optional}{type=integer} Ending value of the
; region of interest in x-axis units unless chan is set.
;
; @keyword rms {in}{optional}{type=float} Used in modes 2 and 3 as
; described in <a href="../toolbox/awv.html>awv"</a>.  If this is not
; supplied, it defaults to the stddev of the data within the region of
; interest given by brange and erange.
;
; @keyword chan {in}{optional}{type=boolean} Range and optional horn positions
; (mode 3 only) specified in channels?
;
; @keyword lefthorn {in}{optional}{type=float} Location of the left
; peak in x-axis units unless chan was specified.
;
; @keyword righthorn {in}{optional}{type=float} Location of the left
; peak in x-axis units unless chan was specified.
;
; @keyword quiet {in}{optional}{type=boolean} When set, the results
; are not printed to the terminal (the ret keyword should be used to
; get the results).
;
; @keyword ret {out}{optional}{type=floating point} Three-element
; array giving [area,width,velocity].  All three elements will be 0.0
; if there was a problem.
;
; @examples
; <pre>
;   gmeasure, 1, 0.5, ret=ret50
;   gmeasure, 1, 0.8, ret=ret80
; </pre>
;
; @uses <a href="../toolbox/awv.html">awv</a>
;
; @version $Id: gmeasure.pro,v 1.7 2006/05/17 07:11:14 bgarwood Exp $
;-
pro gmeasure, mode, fract, brange=brange, erange=erange, rms=rms,$
  chan=chan, lefthorn=lefthorn, righthorn=righthorn, quiet=quiet, ret=ret
    compile_opt idl2

    ret = [0.0,0.0,0.0]

    if not !g.line then begin
        message,'gmeasure is only appropriate in continuum mode.',/info
        return
    endif

    if n_params() ne 2 then begin
        usage,'gmeasure'
        return
    endif

    theDataContainer = getplotterdc()
    npts = data_valid(theDataContainer)
    if npts le 0 then begin
        ; try the PDC
	theDataContainer = !g.s[0]
	npts = data_valid(theDataContainer) 
        if npts le 0 then begin
            message,'The plotter and buffer 0 are empty, can not continue.',/info
            return
        endif
    endif

    if mode ne 1 and mode ne 2 and mode ne 3 then begin
        message,'mode must be one of 1,2 or 3',/info
        return
    endif

    chanRange = getstatsrange('gmeasure',npts,brange,erange,chan=chan)
    if n_elements(chanRange) eq 1 then begin
        message,'range is out of bounds for this data.',/info
        return
    endif
    bchan = chanRange[0]
    echan = chanRange[1]
    data = *theDataContainer.data_ptr
    if bchan eq echan then begin
        message,'gmeasure is not supported for a single channel.',/info
        message,'Data value at channel ',bchan,' = ',data[bchan],/info
        return
    end

    if !g.plotter_axis_type eq 2 then begin
        ; x-axis is velocity, use it
        v = getxarray()
        if (getxunits() ne 'km/s') then begin
            ; must be m/s, scale to km/s
            v /= 1.d3
        endif
    endif else begin
        v = makeplotx(theDataContainer,type=2) / 1.d3 ; always v is in m/s
    endelse
    if n_elements(lefthorn) then begin
        llefthorn = lefthorn
        if n_elements(chan) eq 0 then llefthorn = xtochan(llefthorn)
    endif
    if n_elements(righthorn) then begin
        lrighthorn = righthorn
        if n_elements(chan) eq 0 then lrighthorn = xtochan(lrighthorn)
    endif
    ret = awv(data, v, bchan, echan, mode, fract, $
              lefthorn=llefthorn, righthorn=lrighthorn, rms=rms, quiet=quiet)
    return
end
