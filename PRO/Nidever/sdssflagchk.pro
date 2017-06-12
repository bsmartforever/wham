function sdssflagchk,inflags,flagname,off=off,stp=stp

;+
; This checks if flags are set or not
; It returns 1s and 0s for each input
;
; INPUTS:
;  inflags   Array of flag numbers
;  flagname  Flag name or flag index (starting with 1) that is to be checked
;  /off      Want the indices for which the flag is OFF, otherwise the indices
;             for which the flag is ON will be returned.
;  /stp      Stop at the end of the program
;
; OUTPUTS:
;  ind       Array of indices for which the appropriate flag is ON (or OFF if /off is set)
;
; EXAMPLE:
;  This example returns the indices of arr.flags that have BINNED1 set to ON
;  IDL>ind = sdssflagchk(arr.flags,'BINNED1')
;
;  This example returns the indices of arr.flags that have flag index=19 (SATURATED) set to OFF
;  IDL>ind = sdssflagchk(arr.flags,19,/off)
;
; By D.Nidever  Feb 2007
;-

; Not enough inputs
if n_elements(inflags) eq 0 or n_elements(flagname) eq 0 then begin
  print,'Syntax - ind = sdssflagchk(inflags,flagname,[off=off,stp=stp])
  return,-1
endif

dir = '/net/halo/dln5q/sdss/data/'
restore,dir+'sdss_flags.dat'
nflags = n_elements(flags)

; The flag index input
; flagname must start with "1"
if valid_num(flagname) eq 1 then begin

  ind = flagname-1L
  if flagname gt nflags then begin
    print,'FLAG NUMBER OUT OF RANGE.  MUST BE 1 - ',strtrim(nflags,2)
    return,-1
  endif

; The flag name input
endif else begin

  ind = where(flags eq strupcase(flagname),nind)
  if nind eq 0 then begin
    print,'FLAG ',flagname,' NOT FOUND'
    return,-1
  endif

endelse


; Flag ON
if not keyword_set(off) then begin
  gd = where((inflags and strtrim(string(2.d0^ind[0],format='(I40)'),2)) ne 0,ngd)

; Flag OFF
endif else begin
  gd = where((inflags and strtrim(string(2.d0^ind[0],format='(I40)'),2)) eq 0,ngd)
endelse

if keyword_set(stp) then stop

return,gd

end
