pro msc_movefiles,dir,stp=stp

;+
; This moves MOSAIC files into their correct directories
;
; INPUTS:
;  dir   The base directory where are all the FITS files are
;        By default the current directory is used.
;  /stp  Stop at the end of the program.
;
; OUTPUTS:
;  All object fits files are moved into their respective directories.
;  The directories are created if they do not exist already.
;
; By D.Nidever  Oct 2006
;-

; Use current directory
if n_elements(dir) eq 0 then begin
  dir = ''
end

files = file_search(dir+'obj*.fits')
nfiles = n_elements(files)

objarr = strarr(nfiles)

; Loop through the files
for i=0,nfiles-1 do begin
  head = headfits(files(i))
  obj = sxpar(head,'OBJECT')
  filt = sxpar(head,'FILTER')
  objarr[i] = obj
  print,files(i),' ',obj,' ',filt
end
objarr = strtrim(objarr,2)

gd = lindgen(nfiles)

; Remove any twilights
bd = where(stregex(objarr[gd],'twil',/boolean) ne 0,nbd)
if nbd gt 0 then remove,bd,gd

; Remove any skyflats
bd = where(stregex(objarr[gd],'sky',/boolean) ne 0,nbd)
if nbd gt 0 then remove,bd,gd

; Remove any pointing
bd = where(stregex(objarr[gd],'pointing',/boolean) ne 0,nbd)
if nbd gt 0 then remove,bd,gd

; Remove any test
bd = where(stregex(objarr[gd],'test',/boolean) ne 0,nbd)
if nbd gt 0 then remove,bd,gd


; Getting unique names
ui = uniq(objarr[gd],sort(objarr[gd]))
objects = objarr[gd[ui]]
nobjects = n_elements(objects)
print,''
print,strtrim(nobjects,2),' OBJECTS FOUND'

; Looping through the objects
for i=0,nobjects-1 do begin

  print,''
  print,objects[i]

  ; Making directory if it doesn't exist yet
   obj = objects[i]
   test = file_test(dir+obj,/directory)
   if test eq 0 then file_mkdir,dir+obj

   ; Copy the files into the directory
   g = where(objarr[gd] eq obj,ng)
   file_move,dir+files[gd[g]],dir+obj,/verbose

end

if keyword_set(stp) then stop

end
