pro head_xyad,head,x,y,a,d,degree=degree,stp=stp

;+
;
; HEAD_XYAD
;
; This program uses the WCS in a FITS header to convert
; X/Y pixel values to RA/DEC values (in hours and degrees).
; Vectors can be input.  Unlike adxy.pro/xyad.pro this also
; works with the MOSAIC TNX WCS system.
;
; INPUTS:
;  head    A FITS header containing a WCS
;  x       X pixel values
;  y       Y pixel values
;  /degree RA/DEC will be in degrees.
;  /stp    Stop at the end of the program.
;
; OUTPUTS:
;  a       RA values
;  d       DEC values
;
; USAGE:
;  IDL>head_xyad,head,x,y,a,d,/degree
;
; By D. Nidever   January 2007
;-


nhead = n_elements(head)
nx = n_elements(x)
ny = n_elements(y)

; Not enough inputs
if nhead eq 0 or nx eq 0 or ny eq 0 then begin
  print,'Syntax - head_xyad,head,x,y,a,d,degree=degree'
  return
endif

ctype1 = sxpar(head,'CTYPE1')
tnx = stregex(ctype1,'TNX',/boolean,/fold_case)

; WCS is not TNX
if tnx eq 0 then begin
  xyad,head,x,y,a,d

; WCS IS TNX
endif else begin
  wcs = hdr2wcstnx(head)
  wcstnx_xy2rd, x, y, wcs, a2, d2     ; returns RA/DEC in radians!
  a = a2*!radeg
  d = d2*!radeg
endelse

if not keyword_set(degree) then a=a/15.0

; Return scalars if only one element
if n_elements(a) eq 1 then a=a[0]
if n_elements(d) eq 1 then d=d[0]

if keyword_set(stp) then stop

end
