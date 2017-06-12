pro head_adxy,head,a,d,x,y,degree=degree,stp=stp

;+
;
; HEAD_ADXY
;
; This program uses the WCS in a FITS header to convert
; RA/DEC values (in hours and degrees) to X/Y pixels values.
; Vectors can be input.  Unlike adxy.pro/xyad.pro this also
; works with the MOSAIC TNX WCS system.
;
; INPUTS:
;  head    A FITS header containing a WCS
;  a       RA values
;  d       DEC values
;  /degree RA/DEC are in degrees.
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;  x       X pixel values
;  y       Y pixel values
;
; USAGE:
;  IDL>head_adxy,head,a,d,x,y,/degree
;
; By D. Nidever   January 2007
;-

nhead = n_elements(head)
na = n_elements(a)
nd = n_elements(d)


; Not enough inputs
if nhead eq 0 or na eq 0 or nd eq 0 then begin
  print,'Syntax - head_adxy,head,a,d,x,y,degree=degree'
  return
endif

ctype1 = sxpar(head,'CTYPE1')
tnx = stregex(ctype1,'TNX',/boolean,/fold_case)

; WCS is not TNX
if tnx eq 0 then begin
  adxy,head,a,d,x,y        ; Needs ra/dec in degrees

; WCS IS TNX
endif else begin
  if keyword_set(degree) then a2=a/!radeg else a2=a*15.0d0/!radeg
  d2 = d/!radeg   
  wcs = hdr2wcstnx(head)
  wcstnx_rd2xy, a2, d2, wcs, x, y    ; Needs ra/dec in RADIANS
endelse

; Return scalars if only one element
if n_elements(x) eq 1 then x=x[0]
if n_elements(y) eq 1 then y=y[0]

if keyword_set(stp) then stop

end
