pro run_comb_astphot,input,logfile=logfile,stp=stp

;+
; This runs comb_asphot.pro on many directories
;
; INPUTS:
;  input   A file with the list of directories to run mscmagma_prep.pro on
;  =logfile Name for a logfile.  If only /logfile is set then the
;            name of the logfile will be "run_deredden_#######.log
;            where ####### is a random number.
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;
; EXAMPLE:
;  IDL>run_comb_astphot,'@dirs.txt'
;
; By D.Nidever Jan. 2007
;-


; Not enough inputs
if n_elements(input) eq 0 then begin
  print,'Syntax - run_comb_astphot,input,stp=stp'
  return
endif

; Load input
LOADINPUT,input,dirs
ndirs = n_elements(dirs)

; No valid files
if ndirs eq 0 then begin
  print,'NO DIRECTORIES INPUT'
  return
endif

; Log file
if keyword_set(logfile) then begin
  type = size(logfile,/type)
  if type ne 7 then outfile=maketemp('run_combastphot','.log') else outlogfile=logfile
  file_delete,outlogfile,/allow_nonexistent     ; erase if it exists already
  print,'Starting LOGFILE ',outlogfile
  journal,outlogfile
endif


; Loop through the directories
for i=0,ndirs-1 do begin

  dir = dirs[i]
  dir2 = file_expand_path(dir)
  field = file_basename(dir2)

  astfiles = file_search(dir2+'/obj*_1.ast')
  nastfiles = n_elements(astfiles)
  if nastfiles eq 0 then begin
    print,'NO AST FILES TO RESTORE'
    goto,BOMB
  endif
  filebase = file_basename(astfiles[0],'.ast')
  arr = strsplit(filebase,'_',/extract)
  base = arr[0]

  ; Printing some stuff
  print,'---------------------------------'
  print,'Combining AST files for ',dir
  print,'---------------------------------'
  print,''

  ; Run comb_astphot.pro
  undefine,all
  COMB_ASTPHOT,field,all


  ; Save the structure
  outfile = dir2+'/'+field+'_ast.dat'
  print,'Saving structure to ',outfile
  print,''
  save,all,file=outfile

  BOMB:

end

; Closing the logfile
if keyword_set(logfile) then journal

if keyword_set(stp) then stop

end
