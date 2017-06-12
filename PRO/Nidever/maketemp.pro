function maketemp,prefix,suffix

;+
;
; MAKETEMP.PRO
;
; PURPOSE:
;  This function creates a temporary filename.  It also checks
;  that this file does not exist already.  Seven random digits
;  are added to the prefix.
;
; INPUTS:
;  prefix    The prefix for the temporary filename (e.g. "temp")
;  suffix    The suffix for the temporary filename (e.g. ".txt")
;
; OUTPUTS:
;  temp      The filename of a temporary file
;
; USAGE:
;  IDL>tmp = maketemp('temp','.txt')
;
; By D.Nidever   August 2005
;-

dum = 'bad'

; Making sure it doesn't exist already
while (dum(0) ne '') do begin

  if n_elements(prefix) eq 0 then prefix='temp'
  if n_elements(suffix) eq 0 then suffix=''

  file = prefix
  for i=0,6 do file=file+strtrim(rndint(),2)
  file = file+suffix

  ; Checking if it exists already
  dum = findfile(file+'*')

end

;stop

return,file

end
