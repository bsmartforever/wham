pro daomatch,files,stp=stp

; This runs the DAOMATCH and DAOMASTER scripts on the
; input lits of als files

nfiles = n_elements(files)
if nfiles eq 0 then begin
  print,'Syntax - daomatch,files'
  return
end

cd,current=curdir

dir = file_dirname(files[0])
cd,dir

files2 = file_basename(files,'.als')

; Remove any files
file_delete,files2[0]+'.mch',/allow_nonexistent
file_delete,files2[0]+'.raw',/allow_nonexistent
file_delete,files2[0]+'.tfr',/allow_nonexistent

; Running DAOMATCH
cmd1 = '/net/home/dln5q/bin/daomatch.sh '+files2[0]
if nfiles gt 1 then cmd1=cmd1+' '+files2[1]
if nfiles gt 2 then cmd1=cmd1+' '+files2[2]
if nfiles gt 3 then print,'daomatch.sh can only do 3 frames at a time'
spawn,cmd1,out,errout

nlines = file_lines(files2[0]+'.mch')
if nlines lt nfiles then begin
  print,'Number of lines in MCH is only ',strtrim(nlines,2),'  EXPECTED ',strtrim(nfiles,2)
  print,'CHECK THIS BY HAND!!!'
endif

if (errout[0] ne '') then begin
  print,'DAOMATCH.SH ERROR'
  print,errout
  return
endif

; Running DAOMASTER
cmd2 = '/net/home/dln5q/bin/daomaster.sh '+files2[0]
spawn,cmd2,out2,errout2

if (errout2[0] ne '') then begin
  print,'DAOMASTER.SH ERROR'
  print,errout2
  return
endif

cd,curdir

if keyword_set(stp) then stop

end
