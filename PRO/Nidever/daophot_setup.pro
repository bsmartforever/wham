pro daophot_setup,input,stp=stp

;+
; This sets up daophot programs/scripts for many directories
;
; INPUTS:
;  input   A file with the list of directories to setup daophot for
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;  Five daophot programs/scripts are copied to each directory and
;  a unix script is created that runs daophot.sh on all split MOSAIC
;  FITS files in the directory.  Also, a master daophot script is
;  made in the original directory called daophotscript_master
;
;  The five daophot programs/scripts/setup files are photo.opt,
;  daophot.sh, apcor.opt, goodpsf, lstfilter.  These need to be
;  in the starting directory.
;
; EXAMPLE:
;  IDL>daophot_setup,'dirs.txt'
;
; By D.Nidever Oct. 2006
;-

; Read the input files
readcol,input,dirs,format='A',comment='#'
ndirs = n_elements(dirs)

; Remember where we started
cd,current=curdir

; Loop through the directories
for i=0,ndirs-1 do begin

  cd,curdir

  ; Printing some stuff
  print,'Setting up daophot for ',dirs[i]

  ; Copy photo.opt to this directory
  file_copy,'photo.opt',dirs[i],/overwrite
  file_copy,'daophot.sh',dirs[i],/overwrite
  file_copy,'apcor.opt',dirs[i],/overwrite
  file_copy,'goodpsf.pro',dirs[i],/overwrite
  ;file_copy,'goodpsf',dirs[i],/overwrite
  file_copy,'lstfilter',dirs[i],/overwrite


  ; Create the mkopt.lst file
  cd,dirs[i]
  list1 = file_search('obj????_?.fits')
  list2 = file_search('obj????_??.fits')
  fitsfiles = [list1,list2]

  ; Getting files _1.fits to _16.fits
  gd = where(stregex(fitsfiles,'_[1-9].fits',/boolean) eq 1 or $
             stregex(fitsfiles,'_1[0-6].fits',/boolean) eq 1,ngd)
  fitsfiles = fitsfiles[gd]

  ; Make the daophotscript file
  nfitsfiles = n_elements(fitsfiles)
  openw,unit,/get_lun,'daophotscript'
  for j=0,nfitsfiles-1 do begin
    fil = file_basename(fitsfiles[j],'.fits')
    printf,unit,'daophot.sh '+fil
  end
  close,unit
  free_lun,unit

  ; Make it executable
  file_chmod,'daophotscript','755'o

end

; Go back to starting directory
cd,curdir

; Making master daophotscript
print,'CREATING daophotscript_master'
daophotlist = file_search('','daophotscript')
;spawn,'lfind daophotscript',daophotlist
ndaophotlist = n_elements(daophotlist)

openw,unit,/get_lun,'daophotscript_master'
printf,unit,'echo Running DAOPHOT on $HOST'
for i=0,ndaophotlist-1 do begin
  dum = strsplit(daophotlist[i],'/',/extract)
  dirname = dum[0]
  printf,unit,'cd '+curdir+'/'+dirname
  printf,unit,'daophotscript'
end
close,unit
free_lun,unit

; Make it executable
file_chmod,'daophotscript_master','755'o

if keyword_set(stp) then stop

end
