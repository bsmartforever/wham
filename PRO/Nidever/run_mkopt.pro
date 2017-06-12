pro run_mkopt,input,hilimit=hilimit,stp=stp

;+
; This runs mkopt.pro on many directories
;
; INPUTS:
;  input   A file with the list of directories to run mkopt.pro on
;  hilimit The saturation upper limit, 64,000 by default.
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;  OPT files for all files in the specified directories to be used
;  with DAOPHOT and ALLSTAR.
;
; EXAMPLE:
;  IDL>run_mkopt,'dirs.txt'
;
; By D.Nidever Oct. 2006
;-

if n_elements(hilimit) eq 0 then hilimit = 6.4e4

; Read the input files
readcol,input,dirs,format='A',comment='#'
ndirs = n_elements(dirs)

; Remember where we started
cd,current=curdir

; Loop through the directories
for i=0,ndirs-1 do begin

  cd,curdir

  ; Printing some stuff
  print,''
  print,'---------------------------------'
  print,'MAKING OPT FILES FOR ',dirs[i]
  print,'---------------------------------'

  ;; Create the mkopt.lst file
  ;cd,dirs[i]
  ;file_delete,'mkopt.lst',/allow_non
  ;spawn,'ls obj????_?.fits | grep -v _0.fits > mkopt.lst',dum
  ;spawn,'ls obj????_??.fits >> mkopt.lst',dum
  ; Create the mkopt.lst file
  cd,dirs[i]
  file_delete,'mkopt.lst',/allow_non
  list1 = file_search('obj????_?.fits')
  list2 = file_search('obj????_??.fits')
  fitsfiles = [list1,list2]

  ; Getting files _1.fits to _16.fits
  gd = where(stregex(fitsfiles,'_[1-9].fits',/boolean) eq 1 or $
             stregex(fitsfiles,'_1[0-6].fits',/boolean) eq 1,ngd)
  fitsfiles = fitsfiles[gd]

  ; Run mkopt.pro on the list
  mkopt,fitsfiles,hilimit=hilimit
  ;mkopt,'mkopt.lst'

end

; Go back to starting directory
cd,curdir

if keyword_set(stp) then stop

end
