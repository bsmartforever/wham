pro run_deredden,input,strname=strname,suffix=suffix,logfile=logfile,stp=stp

;+
;
; RUN_DEREDDEN.PRO
;
; Run DEREDDEN.PRO on my photometry structures and save them
;
; INPUTS:
;  input    This is a list of structures to run deredden.pro on.
;           The input can be:
;            (1) an array list of filenames
;            (2) a globbed list, i.e. "*.dat"
;            (3) the name of a file that contains a list, i.e. "list.txt"
;  =strname The name of the structure.  The default is "all".
;  =suffix  The suffix to append to the filename.  The default is  "dered",
;            i.e. the output filename will be filename_dered.dat
;  =logfile Name for a logfile.  If only /logfile is set then the
;            name of the logfile will be "run_deredden_#######.log
;            where ####### is a random number.
;
; OUTPUTS:
;  This program saves the final structure with dereddened magnitudes
;  and colors into the same directory as input structure.
;  The output structure will be called "final"
;
;  A logfile can also be created.
;
; USAGE:
;  IDL>run_deredden,'@files.txt'
;
; By D.Nidever  January 2007
;-


; No input
if n_elements(input) eq 0 then begin
  print,'Syntax - run_deredden,"*.dat"'
  return
endif

; Load input
loadinput,input,files
nfiles = n_elements(files)

; No valid files
if nfiles eq 0 then begin
  print,'NO VALID FILES'
  return
endif

; Log file
if keyword_set(logfile) then begin
  type = size(logfile,/type)
  if type ne 7 then outfile=maketemp('run_deredden','.log') else outlogfile=logfile
  file_delete,outlogfile,/allow_nonexistent     ; erase if it exists already
  print,'Starting LOGFILE ',outlogfile
  journal,outlogfile
endif


print,'Running DEREDDEN.PRO on ',strtrim(nfiles,2),' files'

; Looping through the files
for i=0,nfiles-1 do begin

  file = files[i]
  base = file_basename(file)
  dir = file_dirname(file)

  test = file_test(file)

  ; The file exists
  if (test eq 1) then begin

    ; Field info
    base = file_basename(file)
    dir = file_dirname(file)

    ; Making output name
    outdir = dir
    outdir2 = file_expand_path(outdir)
    base = file_basename(file)

    arr = strsplit(base,'.',/extract)
    filebase = arr[0]
    if not keyword_set(suffix) then suffix='dered'
    outfile = outdir2+'/'+filebase+'_'+suffix+'.dat'    


    ; Restoring the structure
    if not keyword_set(strname) then strname='all'
    cmd = 'undefine,'+strname
    dum = execute(cmd)
    restore,file


    ; Test if the structure exists
    cmd = 'nstr=n_elements('+strname+')'
    dum = execute(cmd)
    if (nstr eq 0) then begin
      print,'STRUCTURE ',strname,' DOES NOT EXIST'
      goto,BOMB
    endif

    ; Rename the structure to "temp"
    cmd = 'temp='+strname
    dum = execute(cmd)
    ; Erase the original
    cmd = 'undefine,'+strname
    dum = execute(cmd)

    print,''
    print,'-------------------------'
    print,'Dereddening ',file
    print,'-------------------------'

    ; Running DEREDDEN.PRO
    undefine,final
    DEREDDEN,temp,final


    ; SAVE the structure
    ; To the original directory
    print,'Saving output "Final" structure to ',outfile
    save,final,file=outfile


  ; File doesn't exist
  endif else begin

    print,file,' DOES NOT EXIST'

  endelse

  BOMB:

end

print,'RUN_DEREDDEN.PRO FINISHED'

; Closing the logfile
if keyword_set(logfile) then journal

if keyword_set(stp) then stop

end
