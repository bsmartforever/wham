function autocorrelate,x

; The autocorrelation of an image
; a = im*im <-> A = IM x IM
;

temp = fft(x,-1)
auto = real_part(fft(temp*conj(temp),1))
sz = size(auto)
auto2 = shift(auto,sz(1)*0.5,sz(2)*0.5)
return, auto2
end

