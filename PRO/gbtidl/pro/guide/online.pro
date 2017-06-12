;+
; Set the line io object to look at the online file.
; 
; <p>The online file is determined by finding the most recently 
; updated spectral line file (ACS or SP) in the online directory
; (/home/sdfits).  Once a file is connected to, this command must be
; used again to connect to a more recent spectral line sdfits file
; (from switching projects or switching backends).  The default is to
; use the most recent file for either spectral line backend.  Use one
; of the two keywords to attach to the most recent file for a specific
; backend.  If both keywords are true, a message will be printed and
; this procedure will return without changing the attached file.
; 
; <p>Note that any file previously attached using "filein" or
; "offline" will be closed as a result of using this procedure.  There
; can be only one input spectral line data file at a time.
;
; @keyword acs {in}{optional}{type=boolean} the most recent
; spectrometer sdfits file will be connected to. 
; @keyword sp {in}{optional}{type=boolean} the most recent spectral
; processor sdfits file will be connected to.
;
; @version $Id: online.pro,v 1.7 2011/04/14 16:41:04 bgarwood Exp $
;-
pro online, acs=acs, sp=sp
    compile_opt idl2
    
    if !g.line eq 0 then begin
        message, "Online continuum mode not supported", /info
        return
    endif

    ; get the info structs for all the possible files
    latest_info = !g.lineio->get_online_infos(acsi,dcri,spi,zpeci,status)
    if status eq 0 then begin
        message, "Cannot find or read the online status file.  No online data available.", /info
        return
    endif
 
    ; should we connect to acs or sp fits files?
    use_acs = 0
    use_sp  = 0
    use_acs = keyword_set(acs)
    use_sp = keyword_set(sp)
    if (use_acs and use_sp) then begin
        message, "Only one of /acs and /sp can be used at a time",/info
        return
    endif
    
    ; latest_info will be set to something if status is not 0
    info = latest_info
    if use_acs then begin
        if acsi.file eq "" then begin 
            message, "No spectrometer online fits file is available.", /info
            return
        endif
        info = acsi
    endif
    if use_sp then begin
        if spi.file eq "" then begin 
            message, "No spectral processor online fits file is available.", /info
            return
        endif
        info = spi
    endif
        
    ; inform the user what is being used    
    print, "Connecting to file: "+info.file
    print, "File has not been updated in ",info.age," minutes.",format='(a29,f16.2,a9)'
    if info.file ne latest_info.file then begin
        print, "Latest file in online directory: "+latest_info.file
        if (use_acs) then begin
            print, '  use: "online, /sp" to attach to that file'
        endif else begin
            print, '  use: "online, /acs" to attach to that file'
        endelse
    endif    

    new_io = obj_new("io_sdfits_line")
    if (obj_valid(new_io)) then begin
        if (obj_valid(!g.lineio)) then obj_destroy, !g.lineio
        !g.lineio = new_io
    endif

    ; finally, load this file in online mode
    !g.lineio->set_online, info.file
    !g.line_filein_name = info.file

end    
