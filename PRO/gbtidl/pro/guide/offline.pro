;+
; Shorthand for filein, '/home/sdfits/filename.{acs,sp}.fits', where
; filename comes from the supplied project and the choice of "acs" or
; "sp" can be made using keywords (defaults to 'acs").
; <p>
; Provide a project name and optionally the backend type (acs or sp)
; to connect to file in the online data directory (/home/sdfits).
; Note that this file will not be treated as online, and will not be
; updated, just as filein does.  Continuum is not supported.
; <p>In addition to being less typing, using offline ensures that
; should the location of the automatically generated sdfits files move
; at any point, you don't have to know where they were moved to - the
; Green Bank installation of GBTIDL will always know where to find
; them. 
;
; @param project {in}{required}{type=string} The project name to use
; in constructing the filename'.
; @keyword acs {in}{optional}{type=boolean} the most recent spectrometer sdfits file will be connected to. This is the default if in line mode.
; @keyword sp {in}{optional}{type=boolean} the most recent spectral
; processor sdfits file will be connected to.
;
; @examples
; <pre>
;    offline,'AGBT02A_028_05'  ; opens ACS data for this project
; </pre>
;
; @version $Id: offline.pro,v 1.6 2007/09/27 16:31:04 bgarwood Exp $
;-
pro offline, project, acs=acs, sp=sp
    compile_opt idl2

    if n_elements(project) eq 0 then begin
        message, "Must supply a project name", /info
        return
    endif

    if !g.line eq 0 then begin
        message, "Contnuum mode not supported for this command", /info
        return
    endif

    ; is the online dir visible from here?
    dir = getConfigValue("SDFITS_DATA",defaultValue="/home/sdfits")
    if file_test(dir) eq 0 then begin
        message, "online directory not visible: "+dir, /info
        return
    endif

    ; what type of backend should we connect to?
    use_acs = 0
    use_sp  = 0
    if keyword_set(acs) eq 0 and keyword_set(sp) eq 0 then begin
        use_acs = 1
        key_used = 0
    endif else begin
        if keyword_set(acs) and keyword_set(sp) then begin
            use_acs = 1
        endif else begin
            if keyword_set(acs) then begin
                use_acs = 1
            endif else begin
                use_sp = 1
            endelse
        endelse
        key_used = 1
    endelse

    if use_acs then type = 'acs' else type = 'sp'

    ; new layout : <online_dir>/<project>/*.fits
    if file_test(dir + '/' + project, /directory) then dir = dir + '/' + project


    ; construct the name of the file to connect to
    filename = dir+'/'+project+'.raw.'+type+'.fits'

    ; if this file does not exist and was specified, try the other backend
    if file_test(filename) eq 0 and key_used eq 0 then begin
        message, "file not found in online dir, trying other backend.", /info
        ; switch backend types
        if use_acs then type = 'sp' else type = 'acs'
        filename = dir+'/'+project+'.raw.'+type+'.fits'
        ; check this new name
        if file_test(filename) eq 0 then begin
            message, "Alternate backend not found as well (project correct?): "+filename , /info
            return
        endif
    endif

    print, "Connecting to file: "+filename

    ; finally, connect to the file
    filein, filename

end
