pro run_stdphot,listin

; This runs stdphot.sh on a list of frames

if n_elements(listin) eq 0 then begin
  print,'Syntax - run_stdphot,listin'
  return
endif


; A list file was input
if strmid(listin[0],0,1) eq '@' then begin

  inp = strmid(listin[0],1)

  ; Loading the files
  readcol,inp,list,format='A',/silent
  nlist = n_elements(list)

endif else begin

  ; Probably an array of filenames
  if n_elements(listin) gt 1 then begin
    list = listin
    nlist = n_elements(list)

  ; A globbed list or only one file
  endif else begin
    list = file_search(listin)
    nlist = n_elements(list)

    ; File not found
    if list[0] eq '' then begin
      print,'FILE ',listin[0],' NOT FOUND'
      return
    endif

  endelse

endelse

nlist = n_elements(list)
list = strtrim(list,2)

print,'RUNNING stdphot.sh on ',strtrim(nlist,2),' FILES'

; Looping through the files
for i=0,nlist-1 do begin
  file = list[i]
  test = file_test(file)
 
  ; Try adding a .fits extension
  if test eq 0 then file=file+'.fits'
  test = file_test(file)

  ; File exists
  if test eq 1 then begin

    fil = file_basename(list[i],'.fits')

    ; Run stdphot.sh
    ;print,'RUNNING stdphot.sh ON ',fil
    print,fil
    spawn,'stdphot.sh '+fil,dum

  ; File doesn't exist
  endif else begin
    print,'FILE ',list[i],' DOES NOT EXIST'
  endelse

end

;stop

end
