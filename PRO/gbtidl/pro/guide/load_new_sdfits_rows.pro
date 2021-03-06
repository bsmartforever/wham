;+
; Searches fits files listed in index file, determines if fits files 
; have new rows that should be appended to index file, and appends 
; only those rows to the index file
;
; If line mode is on (!g.line is 1) then this updates the global 
; line io object, otherwise, it sets the
; global continuum object.
;
; @examples
; <pre>
; >filein, 'filename'
; >list
; ; output from 'filename'
; >list
; ; here you see contents of 'filename'
; > load_new_sdfits_rows; even though 'filename' hasnt changed
; >'No sdfits file(s) in index need updating'
; ; now new rows are appended to 'filename' (by the online filler perhaps)
; > load_new_sdfits_rows
; >'Index file updated with 5 rows'
; >list
; ; here you see the original contents of 'filname'
; ; plus the extra 5 new rows.
; ; NOTE: the index file was NOT re-created from scatch
; </pre>
;
; @uses <a href="sdfitsin.html">sdfitsin</a>
;
; @private_file
;
; @version $Id: load_new_sdfits_rows.pro,v 1.3 2006/05/17 07:11:14 bgarwood Exp $
;-
pro load_new_sdfits_rows
    if (!g.line) then begin
        !g.lineio->load_new_sdfits_rows
    endif else begin
        !g.contio->load_new_sdfits_rows
    endelse
end
