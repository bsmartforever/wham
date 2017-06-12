;+
; NAME:
;   PRINTARR
;
; AUTHOR:
;   David Nidever
;
; PURPOSE:
;   This program prints out variables in a columnar form
;
; MAJOR TOPICS:
;   Output data to screen or file
;
; CALLING SEQUENCE:
;   printarr,a0,a1,a2,file='file.out'
;
; DESCRIPTION:
;
; INPUTS:
;
; OUTPUTS:
;
; INPUT KEYWORD PARAMETERS:
;
; OUPUT KEYWORDS:
;
; EXAMPLES:
;
; MODIFICATION HISTORY:
;
;-

pro printarr,a0,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,$
             a14,a15,a16,a17,a18,a19,a20,a21,a22,a23,a24,a25,$
             a26,a27,file=file,stp=stp

; This program prints out variables in a columnar form

ncol = n_params()

; Open File
if keyword_set(file) then begin
  openw,unit,/get_lun,file
endif

; Find largest size array
nrow = 0
for i=0,ncol-1 do begin
  cmd = 'aa = a'+strtrim(i,2)
  dum = execute(cmd)
  naa = n_elements(aa)
  if naa gt nrow then nrow=naa
end

; Construct array
arr = strarr(ncol,naa)
for i=0,ncol-1 do begin
  cmd = 'aa = a'+strtrim(i,2)
  dum = execute(cmd)
  naa = n_elements(aa)
  nspace = 4
  space = string(replicate(32B,nspace))
  type = size(aa,/type)
  if type eq 4 or type eq 5 then form = '(G20.17)' else form='(A20)'
  if i eq 0 then arr(i,0:naa-1) = space+strtrim(string(aa,format=form),2)+space else $
    arr(i,0:naa-1) = strtrim(string(aa,format=form),2)+space
end

; Making format codes
fmt = '('
for i=0,ncol-1 do begin
  if i eq 0 then n = 20+2*nspace else n = 20+nspace
  fmt=fmt+'A'+strtrim(n,2)
  if i ne ncol-1 then fmt=fmt+','
  if i eq ncol-1 then fmt=fmt+')'
end

; Printing
if keyword_set(file) then begin
  printf,unit,arr,format=fmt
  close,unit
  free_lun,unit
endif else begin
  print,arr,format=fmt
endelse

if keyword_set(stp) then stop

end