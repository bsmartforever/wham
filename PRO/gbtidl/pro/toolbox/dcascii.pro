;+
; Used internally in dcascii to center a string in a given output
; length.
;
; @param str {in}{required}{type=string} The string to center.
; @param len {in}{required}{type=integer} The maximum length of the
; string.
;
; @returns the centered string.
;
; @private
;-
function centerpad,str,len
    compile_opt idl2

    extra = len - strlen(str)
    if (extra le 0) then begin
        result = strmid(str,0,len)
    endif else begin
        front = extra/2
        back = extra-front
        result = ''
        if front gt 0 then result = strjoin(make_array(front,/string,value=' '))
        result = result + str
        if back gt 0 then result = result + strjoin(make_array(back,/string,value=' '))
    endelse

    return, result
end

;+
; Output a data container as ascii showing the x-axis value and the
; corresponding y-axis value over the given range of X values.
;
; @param dc {in}{optional}{type=data container} The data container to
; use.  If not supplied, the plotter is queried for the currently
; displayed values.
;
; @keyword brange {in}{optional}{type=float}{default=all} beginning
; of the range to plot, in current plotter units
;
; @keyword erange {in}{optional}{type=integer}{default=all} end of
; the range to plot, in current plotter units
;
; @keyword file {in}{optional}{type=string}{default=/dev/tty} The file
; to write to.  Defaults to the current screen, using "more" to page
; the output.
;
; @examples
; list the current displayed spectrum at the terminal from 1415 to 1425 MHz.
; The current x-axis must be in MHz.
; <pre>
;   dcascii,brange=1415,erange=1425
;   ; the output looks something like this:
;
; Scan:     88         GalPlane 2005-11-15 +08 06 49.0
;                                  Ta    
;          MHz-LSR                 YY
;     1424.9752215054413682     -0.0136188
;     1424.9361597211752724      0.0389956
;     1424.8970979369089491      0.0697150
;     1424.8580361526428533      0.0732256
;     1424.8189743683767574      0.0983463
;     1424.7799125841104342      0.1009806
;           .....
;      < Press Spacebar to continue, ? for help >
; </pre>
; The first line shows the scan number, source name, date, and UT
; associated with this data.  The next two lines describe what is in
; each column. The x-axis values are in the left column and the y-axis
; values are in the right column.  The first header in the left column
; gives the velocity definition if velocities are show (otherwise it
; is empty).  The second header in that column gives the units and the
; reference frame.  The first header in the right column gives the
; units and the second header gives the polarization.
; 
; @version $Id: dcascii.pro,v 1.9 2011/02/16 19:24:04 bgarwood Exp $
;-
pro dcascii, dc, brange=brange, erange=erange, file=file
    compile_opt idl2

    if n_elements(dc) eq 0 then begin
        xarray=getxarray(count=nch)
        if nch le 0 then begin
            message,'No data is displayed.',/info
            return
        endif
        xunits=getxunits()
        veldef=getxveldef()
        frame=getxframe()
        yarray=getyarray()
        dc = getplotterdc()
        nch = data_valid(dc,name=name)
        yunits = dc.units
    endif else begin
        nch = data_valid(dc,name=name)
        if (nch le 0) then begin
            message,'No data found in dc.',/info
            return
        endif
        yarray = *dc.data_ptr
        yunits = dc.units
        ; everything starts out as channels
        xunits='Channels'
        xarray=findgen(n_elements(yarray))

        axisType = !g.plotter_axis_type
        if name eq "CONTINUUM_STRUCT" then axisType = 0

        case axisType of
            0: ; channels nothing more to do
            1: begin
                ; frequency
                ; construct an x-axis using !g.plotter_axis_type and 
                ; frame and veldef in the data container.
                veldef_ok = decode_veldef(dc.velocity_definition, veldef, frame)
                if veldef_ok le 0 then begin
                    ; frame may be wrong, try using one at 
                    message,"Problems deciphering dc.velocity_definition, using frame in dc.frequency_type",/info
                    frame=dc.frequency_type
                endif
                xarray=convertxvalues(dc,xarray,1.0d,0,'','',0.0d,0.0d,$
                                      1.0d,1,frame,veldef,0.0d,0.0d)
                ; now scale it to an appropriate value
                scalevals,xarray[0],scaled,prefix
                scale = xarray[0]/scaled
                xarray = xarray/scale
                xunits = prefix + "Hz"
            end
            2: begin
                ; velocity
                ; construct an x-axis using !g.plotter_axis_type and 
                ; frame and veldef in the data container.
                veldef_ok = decode_veldef(dc.velocity_definition, veldef, frame)
                if veldef_ok le 0 then message, "Problems deciphering dc.velocity_definition, velocities may be wrong",/info
                ; get it in km/s right away
                xarray=convertxvalues(dc,xarray,1.0d,0,'','',0.0d,0.0d,$
                                      1.d3,2,frame,veldef,0.0d,0.0d)
                xunits="km/s"
            end
            else: begin
                ; unknown, use channels
                message,'Unknown x-axis type in !g.plotter_axis_type, output is in channels',/info
            end
        endcase
        
    endelse
    
    usemore = 0
    if n_elements(file) eq 0 then begin
        file='/dev/tty'
        usemore = 1
        ; check for existance of !g
        defsysv,'!g',exists=hasg
        ttyok = hasg ? !g.interactive : hastty()
        if not ttyok then begin
            usemore = 0
            file=''
        endif
    endif

    if n_elements(brange) eq 0 then brange = xarray[0]
    if n_elements(erange) eq 0 then erange = xarray[nch-1]

    if brange gt erange then begin
        tmp = brange
	brange = erange
	erange = tmp
    endif
    
    if strlen(file) gt 0 then begin
        openw, out, file, /get_lun, more=usemore
    endif else begin
        ; just write to stdout, without using more
        out = -1
    endelse

    if name eq "CONTINUUM_STRUCT" then begin
        utc0 = (*dc.utc)[0]
        utcstring = adstring(utc0/3600.0)
        date = (*dc.date)[0]
    endif else begin
        utcstring = adstring(dc.utc/3600.0)
        date = dc.date
    endelse
    printf,out,dc.scan_number,dc.source,date,utcstring, $
           format='("Scan: ",i6," ",a16," ",a10," ",a11)'
    if strlen(yunits) eq 0 then yunits = 'Counts'
    yunits = centerpad(yunits,9)
    if strpos(xunits,'m/s') ge 0 then begin
        veldef = centerpad(veldef,7)
        printf,out,veldef,yunits,format='(9x,a7,14x,a9)'
    endif else begin
        printf,out,yunits,format='(30x,a9)'
    endelse
    if xunits eq 'Channels' then rframe='' else rframe='-'+frame
    unitsFrame=centerpad(xunits+rframe,9)
    ; printf,out,unitsFrame,dc.polarization,format='(7x,a9,9x,a)'
    printf,out,unitsFrame,dc.polarization,format='(8x,a9,16x,a)'
    for i=0,nch-1 do begin
      if xarray[i] ge brange and xarray[i] le erange then $
         ; printf,out,xarray[i],yarray[i]
         printf,out,xarray[i],float(yarray[i]),format="(f,f)"
    end
    if out ne -1 then begin
        free_lun, out
        if not usemore then print,'ASCII file written:', file
    endif
end
 
