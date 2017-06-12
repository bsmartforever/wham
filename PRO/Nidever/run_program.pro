pro run_program,name,input,comment=comment,logfile=logfile,$
                stp=stp,noprint=noprint,_extra=extra

;+
; This is a generic driver program.  You can give it inputs
; to loop over, i.e. file names, etc.  You can also set other
; keyword parameters and it will passed onto "program" in
; the _EXTRA parameter.
;
; INPUTS:
;  name     Name of program to run
;  input    A file with the list of inputs for the program
;  =comment Comment string
;  =logfile The name of a logfile.
;  /noprint Don't print anything to the screen
;  /stp     Stop at the end of the program.
;
; OUTPUTS:
;  Whatever "program" outputs
;
; EXAMPLE:
;  IDL>run_program,'test','input.lst'
;
; By D.Nidever Feb. 2007
;-


; Not enough inputs
if n_params() lt 2 then begin
  print,'Syntax - run_program,name,input,stp=stp'
  return
endif

; A list file was input
if strmid(input[0],0,1) eq '@' then begin

  inp = strmid(input[0],1)

  ; Loading the files
  ;readcol,inp,list,format='A',/silent,comment='#'
  readline,inp,list,comment=comment
  nlist = n_elements(list)

endif else begin

  ; Probably an array of filenames
  if n_elements(input) gt 1 then begin
    list = input
    nlist = n_elements(list)

  ; A globbed list or only one file
  endif else begin
    list = file_search(input)
    nlist = n_elements(list)
  endelse

endelse


; Log file
if keyword_set(logfile) then begin
  type = size(logfile,/type)
  if type ne 7 then outfile=maketemp(name,'.log') else outfile=logfile
  print,'Starting LOGFILE ',outfile
  journal,outfile
endif

; Loop through the input list
for i=0,nlist-1 do begin

  if not keyword_set(noprint) then begin
    print,''
    print,strtrim(i+1,2),'/',strtrim(nlist,2),'  RUNNING ',name,',',";"+list[i]+"'"
    print,''
  end

  ; Break up the string into its parts?

  ; Call the procedure
  call_procedure,name,list[i]  ;,_extra=extra

end

; Closing the logfile
if keyword_set(logfile) then journal

if keyword_set(stp) then stop

end
