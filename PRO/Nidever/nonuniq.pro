function nonuniq, array1,array2,dbl=dble,bad=bad,indbl=indbl

; This function returns the indices of array1 that are not array2
;
; INPUT:
;  array1	Main array
;  array2	subset of array
;
; OUTPUT:
;  final        Returned array of indices of non-doubles
;  bad=bad	Array of non-doubles (array1 not in array2)
;  dbl=dbl	Array of doubles (in both array1 and array2)
;  indbl=indbl	Array of indices (of array1) of doubles (in both array1 and array2)
;
; Created by D.Nidever  2005

if N_params() LT 1 then begin   
       print, 'Syntax - result=nonuniq(main_arr,sub_arr)
       return,1
endif

if n_elements(array1) eq 0 or n_elements(array2) eq 0 then begin
  bad=-1
  dbl=-1
  goto,BOMB
endif

;if n_elements(array1) eq 1 then array1=lindgen(array1)

if n_elements(dble) gt 0 then undefine,dble

n=n_elements(array1)

for i=0,n-1 do begin
  ind = where(array2 eq array1(i),ngd)

  if ngd eq 0 then push,final,i       ; indices of non-doubles
  if ngd eq 0 then push,bad,array1(i) ; array of non-doubles
  if ngd ne 0 then push,indbl,i       ; indices of doubles
  if ngd ne 0 then push,dble,array1(i) ; array of doubles

end

if n_elements(dble) eq 0 then dble=-1
if n_elements(bad) eq 0 then bad=-1

;nbad=n_elements(bad)
;if nbad gt 1 then bad=bad(1:nbad-1)
;if nbad eq 1 then bad=bad


BOMB:

if n_elements(final) eq 0 then final=-1

;stop

return,final

end
