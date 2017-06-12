pro read_aric, aric

a = 1.0
openr, 1, '/usr/users/tufte/idl/cubeview/aric_cube.dat'
aric = fltarr(75,23,10)
for iv=0,9 do begin
  for ix=0,74 do begin
    for iy=0,22 do begin
      readf,1,a
      aric(ix,iy,iv) = a
    endfor
  endfor
endfor
close,1
return
end


