pro run_astrom,input,transfile,stp=stp

;+
; This runs mscmagma.pro on many input files
;
; INPUTS:
;  input   A file with the list of directories to run mscmagma.pro on
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;  OPT files for all files in the specified directories to be used
;  with DAOPHOT and ALLSTAR.
;
; EXAMPLE:
;  IDL>run_mscmagma,'input.lst'
;
; By D.Nidever Jan. 2007
;-


; Not enough inputs
if n_params() lt 1 then begin
  print,'Syntax - run_msmagma,input,transfile,stp=stp'
  return
endif

; A list file was input
if strmid(input[0],0,1) eq '@' then begin

  inp = strmid(input[0],1)

  ; Loading the files
  readcol,inp,list,format='A',/silent
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


; Remember where we started
cd,current=curdir

; Loop through the directories
for i=0,nlist-1 do begin

  ; Get the directory name for this input file
  dir = file_dirname(list[i])

  fdir = file_expand_path(list[i])

  ; Run get_astrom_global
  get_astrom_global,fdir,fpar


  ; Save the file
  field = file_basename(fdir)
  print,'Saving parameters to ',fdir+'/'+field+'_astrom.dat'
  save,fpar,file=fdir+'/'+field+'_astrom.dat'

end

if keyword_set(stp) then stop

end
