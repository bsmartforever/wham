pro writeline,file,lines,stp=stp,prepend=prepend,append=append

;+
; This writes a string array to a file. Reverse of READLINE.PRO
;
; INPUTS:
;  file      File name.
;  lines     String array
;  =comment  Comment string.  Lines starting with this
;            character will be skipped
;  /prepend  Prepend to file (i.e. at the beginning)
;  /append   Append to file (i.e at the end)
;  /stp      Stop at the end of the program
;
; OUTPUTS:
;  The output written to the file
;
; USAGE:
;  IDL>writeline,'test.txt',lines
;
; By D.Nidever  Feb.2007
;-

if n_elements(file) eq 0 then begin
  print,'Syntax - writeline,file,lines,stp=stp'
  return
endif

info = file_info(file)

; File does *not* exist
if (info.exists eq 0) then begin

  openw,unit,/get_lun,file

  nlines = n_elements(lines)
  for i=0,nlines-1 do printf,unit,lines[i]

  close,unit
  free_lun,unit

; File exists
endif else begin

  ; Prepend
  if keyword_set(prepend) and not keyword_set(append) then begin

    ; Write to a temporary file
    openw,unit,/get_lun,'temptemp.temp'

    nlines = n_elements(lines)
    for i=0,nlines-1 do printf,unit,lines[i]

    close,unit
    free_lun,unit

    ; Copy main file to temporary file
    file_move,file,'temptemp2.temp'

    ; Concatenate them
    spawn,'cat temptemp.temp temptemp2.temp > '+file,out

    ; Erase temporary file
    file_delete,'temptemp.temp'
    file_delete,'temptemp2.temp'

  endif

  ; Normal operation or append
  if not keyword_set(prepend) then begin

    openw,unit,/get_lun,file,append=append

    nlines = n_elements(lines)
    for i=0,nlines-1 do printf,unit,lines[i]

    close,unit
    free_lun,unit

  endif


endelse

if keyword_set(stp) then stop

end
