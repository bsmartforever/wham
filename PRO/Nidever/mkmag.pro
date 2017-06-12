pro mkmag,listin

; This runs selstartot.pro on a list of frames
; Give a list of the *.tot files

if n_elements(listin) eq 0 then begin
  print,'Syntax - mkmag,listin'
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

print,'RUNNING mkmag.pro on ',strtrim(nlist,2),' FILES'

; Looping through the files
for i=0,nlist-1 do begin
  file = list[i]
  test = file_test(file)
 
  ; File exists
  if test eq 1 then begin

    fil = file_basename(list[i],'a.tot')

    totfile = fil+'a.tot'
    coordfile = fil+'.coord'
    outfile = fil+'.mag'

    ; Run selstartot.pro
    print,fil
    selstartot,totfile,coordfile,outfile

  ; File doesn't exist
  endif else begin
    print,'FILE ',list[i],' DOES NOT EXIST'
  endelse

end

;stop

end
