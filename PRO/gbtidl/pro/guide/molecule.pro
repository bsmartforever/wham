;+
; Plot the molecular line frequencies for previously detected
; molecular lines stored in !g.molecule on the currently displayed
; plot at the appropriate location given the current x-axis.
;
; <p>Initial version contributed by Glen Langston, glangsto\@nrao.edu
;
; @keyword doPrint {in}{optional}{type=boolean}{default=0} optionally print
; the line frequencies.  The printed frequencies are the line
; frequencies in the frame being displayed on the plotter.
;
; @uses <a href="../../devel/guide/moleculeread.html">moleculeread</a>
; @uses <a href="freq.html">freq</a>
; @uses <a href="../toolbox/veltovel.html">veltovel</a>
; @uses <a href="../toolbox/shiftvel.html">shiftvel</a>
; @uses <a href="../toolbox/shiftfreq.html">shiftfreq</a>
; @uses <a href="../toolbox/decode_veldef.html">decode_veldef</a>
; @uses <a href="../plotter/show.html">show</a>
; @uses <a href="../plotter/vline.html">vline</a>
; @uses <a href="../plotter/getstate.html#_getxrange">getxrange</a>
; @uses <a href="../plotter/getstate.html#_getyrange">getyrange</a>
; @uses <a href="../plotter/getstate.html#_getxvoffset">getxvoffset</a>
; @uses <a href="../plotter/getstate.html#_getxunits">getxunits</a>
; @uses <a href="../plotter/getstate.html#_getxoffset">getxoffset</a>
; @uses <a href="../plotter/getstate.html#_getplotterdc">getplotterdc</a>
; @uses textoidl
; 
; @version $Id: molecule.pro,v 1.8 2007/05/01 19:57:12 bgarwood Exp $
;-
pro molecule, doPrint=doPrint
    compile_opt idl2
    on_error,2

    if n_elements(doPrint) eq 0 then doPrint = 0

    if not !g.line then begin
        message,'This only works with spectral line data.',/info
        return
    endif

    freq                        ; set x axis to frequency

    yrange = getyrange(empty=empty)
    if empty then begin
        message,'Nothing in the plotter',/info
        return
    endif
    ; use full values of x, not just xrange, so that if we are zoomed
    ; when unzoom happens, the currently hidden lines will be shown
    x = getxarray()
    xmax = x[n_elements(x)-1]
    xmin = x[0]
    if xmax lt xmin then begin
        tmp = xmax
        xmax = xmin
        xmin = tmp
    endif
    ymax = yrange[1]
    ymin = yrange[0]

    ; check for lines in range
    yrange = ymax - ymin
    yincr=0.04                 ; compute reason-able tag posion offsets
    yOffset= -4.0*yincr        ; start from bottom most position
    nShow = 0                  ; count number of lines shown

    moleculeRead          ; read in molecule frequencies
                          ; will not duplicate any effort if already read

    ; get source velocity as a true velocity, use DC in the plotter
    pdc = getplotterdc()
    vdef = pdc.velocity_definition
    ok = decode_veldef(vdef, veldef, vframe)
    ; this is the TRUE velocity in vframe
    vsrel = veltovel(pdc.source_velocity, 'TRUE',veldef)
    ; this is the current velocity offset that the user has set in 
    ; the plotter - that is a real velocity offset.
    vxOffset = getxvoffset()
    ; the velocity offset to actually shift the rest frequencies
    vshift = shiftvel(-vsrel,+vxoffset,veldef='TRUE')

    ; use this to get the doppler factor, just pick any frequency
    ; use observed_frequency - easy to get
    dopplerFactor = shiftfreq(pdc.observed_frequency, vshift, veldef='TRUE') / pdc.observed_frequency

    ; xoffset - this is a simple linear offset, applied last
    xoffset = getxoffset()

    xunits = getxunits()
    ; find scale from xunits to MHz
    fscale = 1.d
    case xunits of 
        'Hz': fscale = 1.d6
        'kHz': fscale = 1.d3
        'MHz': fscale = 1.d
        'GHz': fscale = 1.d-3
    endcase

    ; clear any previously displayed molecule vertical lines
    clearvlines,idstring='__molecule',/noshow

    ;For all known molecular lines
    for i = 0,(!g.nmol-1) do begin

        ; get frequency in appropriate frame
        xf = !g.molecules[i].freq;
        xf = xf * dopplerFactor ; still in MHz
        xf = xf * fscale ; now in plotter units
        xf = xf - xoffset ; now with correct offset

        ; if line is in the plotted range
        if ((xf gt xmin) and (xf lt xmax)) then begin
            textLabel  = textoidl('U')
            if (!g.molecules[i].type ne 'U') then begin
                textLabel = textoidl( strtrim(string( !g.molecules[i].formula)))
            endif
            ; actually mark line location, with formula
            vline, xf,label=textLabel, ylabel=yOffset+1.0,/noshow,/ynorm, idstring='__molecule'
            nShow = nShow + 1   ; count lines plotted
            yOffset = yOffset + yincr ; move to next text position
            if (yOffset ge -yincr) then yOffset = -4.*yincr ; cycle back to first
            if (doPrint) then print, !g.molecules[i].formula,' nu = ', xf
        endif
        ; if past both x end points, exit
    endfor
    reshow

    return
end ; end of molecule
