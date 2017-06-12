function add,num1,num2

; this adds two numbers together

fsum = ''
carry = 0

old_num1 = num1
old_num2 = num2

num1 = stringize(num1,ndec=0,/nocomma)
num2 = stringize(num2,ndec=0,/nocomma)
;num1 = strtrim(num1,2)
;num2 = strtrim(num2,2)

len1 = strlen(num1)
len2 = strlen(num2)

if len1 gt len2 then begin		;reversing them
  dum = num1				;num1 should always be the smaller one
  num1 = num2
  num2 = dum
  len1 = strlen(num1)
  len2 = strlen(num2)
endif

for i=0,len2-1 do begin

  if (len1-i-1) lt 0. then dig1='0'
  if (len1-i-1) ge 0. then dig1 = strmid(num1,len1-i-1,1)
  if (len2-i-1) lt 0. then dig2='0'
  if (len2-i-1) ge 0. then dig2 = strmid(num2,len2-i-1,1)

  sum = long(dig1) + long(dig2)
  sum = sum + carry			;carrying from previous one

  carry = sum/10
  left = sum - carry*10
  left = strtrim(left,2)

  fsum = left + fsum

endfor

if carry eq 1 then fsum = strtrim(carry,2) + fsum

;stop
;
;if len2 gt len1 then begin
;  dig1 = carry
;  dig2 = strmid(num2,len2-len1-1,1)
;
;  sum = long(dig1) + long(dig2)
;  sum = strtrim(sum,2)
;
;  fsum = sum + fsum
;
;  ;adding on the extra digits on the front
;  leftover = strmid(num2,0,len2-len1-1)
;  leftover = strtrim(leftover,2)
;
;  fsum = leftover + fsum
;endif
;
;if len1 eq len2 then begin		; the same length
;  if carry ne 0 then fsum = strtrim(carry,2) + fsum
;endif

num1 = old_num1		;return the originals
num2 = old_num2

return,fsum

end
