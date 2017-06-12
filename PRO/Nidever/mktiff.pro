pro mktiff,file

; This program grabs the screen and writes it to a tiff file

if not keyword_set(file) then file = 'idl.tiff'

; checking the open windows
device,window_state=windows
open = where(windows eq 1,nopen)

; No windows open
if nopen eq 0 then begin
  print,'No Windows Open'
  return
endif

; creating tiff image
tiff = tvrd(true=1)
tiff = reverse(tiff,3)
write_tiff,file,tiff,1

;stop

end
