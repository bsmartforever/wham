;+
; Read in the list of all previously detected molecular lines, storing
; them into an array of structures at !g.molecules.  The total number
; of molecules stored there is indicated by !g.nmol.  If nmol is > 0
; then this procedure returns immediately without modifying
; !g.molecules.  See <a href="molecule.html">molecule</a> for
; the code that is intended to use this information.
;
; <p>The text file containing the line information is found at
; $GBT_IDL_DIR/pro/guide/nistLineTable.txt.  It contains information
; extracted from <a href="http://physics.nist.gov/cgi-bin/micro/table5/start.pl">Recommended Rest Frequencies for observed interstellar molecular lines"</a> by Frank J. Lovas.
; It was originally compiled by Glen Langston: glangsto\@nrao.edu.
;
; @private_file
; @version $Id: moleculeread.pro,v 1.4 2007/05/01 19:57:12 bgarwood Exp $
;-
pro moleculeRead
    compile_opt idl2

    on_error,2

    if !g.nmol gt 0 then return

    fname=file_search('$GBT_IDL_DIR/pro/guide/nistLineTable.txt',count=count,/expand_environment)
    if count lt 1 then begin
        message,'Line information file could not be found, no line IDs available',/info
        return
    endif

    maxl = n_elements(!g.molecules)

    ;now define for molecules function
    record = {molecule_struct}

    ;- Open input file
    openr, lun, fname, /get_lun

    ;- Define record structure and create array
    fmt = '(a1,1x,f11.4,4x,a13)'

    ;- Read records until end-of-file reached
    recnum = 0L
    while (eof(lun) ne 1 && !g.nmol lt maxl) do begin
        on_ioerror, bad_rec
        error = 1
        readf, lun, record, format=fmt
        error = 0
        ;  print,'type=', record.type,', freq=',record.freq,',
        ;  formula=',record.formula
        if record.type ne ';' then begin
            ; only do this for non-comment lines
            !g.molecules[!g.nmol] = record
            ; convert MHz to Hz
            !g.nmol += 1
        endif
        ;- Silently ignore bad input record - these are always comment lines
        bad_rec:
        recnum = recnum + 1
    endwhile

    close,lun 
    free_lun, lun

    return
end ; end of moleculeread
