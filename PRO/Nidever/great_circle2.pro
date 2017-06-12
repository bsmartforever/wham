pro great_circle2,npole,l,b

; this program creates a small circle given a north pole
;

npol = npole

deg2rad = !dpi/180.d
rad2deg = (180.d)/!dpi

; using a random equator
equator = [0.,0.]
if equator(0) eq npol(0) and equator(1) eq npol(1) then equator=[80.,0.]

;Use rotate_lb to get l,b in the new coordinate system
larr=findgen(360)
barr=fltarr(360)
rotate_lb,larr,barr,npol,equator,nlarr,nbarr,/reverse,/noprint

l = nlarr
b = nbarr

;stop

end
