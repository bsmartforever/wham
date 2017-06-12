;+
; This procedure prints a postscript file that reproduces the spectrum
; in the plotter.  To save the postscript to a file and not print it use 
; <a href="write_ps.html">write_ps</a>.
;
; @param filename {in}{optional}{type=string} The postscript filename can be specified
; using this parameter.  If omitted, the file will be called 'gbtidl.print.file.ps'.
;
; @keyword device {in}{optional}{type=string} The name of the printer
; to use.  If not set, this defaults to the value of !g.printer.  If
; !g.printer has zero-length, it falls back to using 'lp'.
;
; @keyword portrait {in}{optional}{type=boolean} If set then 
; the postscript will be generated in portrait mode instead of the 
; default landscape mode.
;
; @examples
;    print_ps,'myplot.ps',device='ps2',/portrait
;
; @uses <a href="write_ps.html">write_ps</a>
;
; @version $Id: print_ps.pro,v 1.3 2006/01/27 18:30:18 bgarwood Exp $
;-

pro print_ps,filename,portrait=portrait,device=device
    if n_elements(filename) eq 0 then filename = 'gbtidl.print.file.ps'
    write_ps,filename,portrait=portrait
    ; check that the file exists
    if (file_test(filename,/read)) then begin
        if (not keyword_set(device)) then device = !g.printer
        if (strlen(device) eq 0) then device = 'lp'
        ; spool it to the printer
        printCmd = 'lpr -P' + device + ' ' + filename
        spawn, printCmd
        print, filename,' printed on ', device
    endif
end
    