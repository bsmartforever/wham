pro writespe, filename, v, spect, sig,  ndp

a = 1.0
b = 1.0
c = 1.0

openw, 1, filename

for i = 0, ndp - 1 do begin
    a = v(i)
    b = spect(i)
    c = sig(i)
    printf, 1, a, b, c
endfor

close, 1

return
end

