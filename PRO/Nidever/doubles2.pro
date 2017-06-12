pro doubles2,arr1,arr2,ind1,ind2,count=count,silent=silent,stp=stp

;+
; This is similar to DOUBLES.PRO except that you input two
; arrays.  The inputs are similar to SRCOR.PRO.
; NOTE: If a "double" appears multiple times in one of the
; arrays then only the index of the first instance is returned.
;
; CALLING SEQUENCE:
;  DOUBLES2,arr1,arr2,ind1,ind2,count=count,stp=stp
;
; INPUTS:
;  arr1     First array
;  arr2     Second array
;  /silent  Don't print anything.
;  /stp     Stop at the end of the program.
;
; OUTPUTS:
;  ind1     Array of doubles for first array
;  ind2     Array of doubles for second array
;  =count   The number of matches.  This is set to -1 if there was
;             and error.
;
; USAGE:
;  IDL>doubles2,arr1,arr2,ind1,ind2,count=count,silent=silent,stp=stp
;
; By D.Nidever  April 2007
;-

undefine,ind1,ind2,count

n1 = n_elements(arr1)
n2 = n_elements(arr2)

; Enough inputs
if n1 eq 0 or n2 eq 0 then begin
  print,'Syntax - doubles2,arr1,arr2,ind1,ind2,count=count,silent=silent,stp=stp'
  return
endif

; Error Handling
;------------------
; Establish error handler. When errors occur, the index of the  
; error is returned in the variable Error_status:  
CATCH, Error_status 

;This statement begins the error handler:  
if (Error_status ne 0) then begin 
   if not keyword_set(silent) then $
     print,'DOUBLES2 ERROR: ', !ERROR_STATE.MSG  
   ind1 = -1
   ind2 = -1
   count = -1     ; This indicates there was an error
   CATCH, /CANCEL 
   return
endif


; Getting the unique elements for each
ui1 = uniq(arr1,sort(arr1))
arr1b = arr1[ui1]
n1b = n_elements(arr1b)
ui2 = uniq(arr2,sort(arr2))
arr2b = arr2[ui2]
n2b = n_elements(arr2b)

; Getting the doubles
ind = DOUBLES([arr1b,arr2b],/all)

; Breaking them up into indices for the 1st and 2nd arrays
gd1 = where(ind le (n1b-1),ngd1)
gd2 = where(ind ge n1b,ngd2)

; NO DOUBLES
if ngd1 eq 0 or ngd2 eq 0 then begin
  count = 0
  ind1 = -1
  ind2 = -1
  if not keyword_set(silent) then $
    print,'NO DOUBLES'
  return
endif

ind1b = ind[gd1]
ind2b = ind[gd2]-n1b

; Now put the indices in terms of the ORIGINAL arrays
ind1 = ui1[ind1b]
ind2 = ui2[ind2b]

; How many matches are there
count = n_elements(ind1)

; Sort them
si = sort(ind1)
ind1 = ind1[si]
ind2 = ind2[si]
;si1 = sort(arr1[ind1])
;ind1 = ind1[si1]
;si2 = sort(arr2[ind2])
;ind2 = ind2[si2]

if keyword_set(stp) then stop

end
