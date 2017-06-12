; $Id: $
function opan_readTextGroup,filename
if n_params() eq 0 then return,(-1)

dummy = ''
openr,lun,filename,/get_lun
; Read in the number of channels
repeat begin
  readf,lun,dummy
  colPos = strpos(dummy,'#')
endrep until colPos eq (-1)

nchan = fix(dummy)
x = fltarr(nchan)

; Read in the number of groups
repeat begin
  readf,lun,dummy
  colPos = strpos(dummy,'#')
endrep until colPos eq (-1)
ngroups = fix(dummy)
y = fltarr(ngroups)
z = fltarr(nchan,ngroups)
zerr = fltarr(nchan,ngroups)

repeat begin
  readf,lun,dummy
  colPos = strpos(dummy,'#')
endrep until colPos eq (-1)

; Read in the xvalues
x[0] = float(dummy)
for j = 1,nchan-1 do begin
  readf,lun,dummy
  x[j] = float(dummy)
endfor

; Read in the yvalues
repeat begin
  readf,lun,dummy
  colPos = strpos(dummy,'#')
endrep until colPos eq (-1)
y[0] = float(dummy)
for j = 1,ngroups-1 do begin
  readf,lun,dummy
  y[j] = float(dummy)
endfor

xval = 0.0 & yval = 0.0

for i = 0,ngroups-1 do begin
  repeat begin
    readf,lun,dummy
    colPos = strpos(dummy,'#')
  endrep until colPos eq (-1)
  vals = strsplit(dummy,' ',/extract)
  z[0,i] = float(vals[0])
  zerr[0,i] = float(vals[1])
  for j = 1,nchan-1 do begin
    readf,lun,xval,yval
    z[j,i] = float(xval)
    zerr[j,i] = float(yval)
  endfor
endfor

free_lun,lun

d = {x:x,y:y,z:z,zerr:zerr, $
     xlabel:'x', $
     ylabel:'y', $
     zlabel:'z'}

return,d
end