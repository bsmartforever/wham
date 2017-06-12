pro mk_daogrow_std,dir,file,silent=silent

; This makes the input file for DAOGROW

if n_elements(dir) eq 0 then dir=''
if n_elements(file) eq 0 then file='daogrow'

files = file_search(dir,'*_*.fits')
nfiles = n_elements(files)

gd = where(stregex(files,'Raw',/boolean,/fold_case) eq 0 and $
           stregex(files,'twilight',/boolean,/fold_case) eq 0 and $
           stregex(files,'bad',/boolean,/fold_case) eq 0 and $
           stregex(files,'flat',/boolean,/fold_case) eq 0 and $
           stregex(files,'zero',/boolean,/fold_case) eq 0,ngd)
files = files(gd)
nfiles = n_elements(files)

if not keyword_set(silent) then print,'Making DAOGROW input file: ',file+'.inf'

openw,unit,/get_lun,file+'.inf'

; Loop through the files
for i=0,nfiles-1 do begin
  head = headfits(files[i])
  name = file_basename(files[i],'.fits')
  filter = sxpar(head,'FILTER')

  filt = strcompress(filter,/remove_all)
  if stregex(filter,'M Washington',/boolean) eq 1 then filt=1  ;'M'
  if stregex(filter,'D51 DDO',/boolean) eq 1 then filt=2   ;'D'
  if stregex(filter,'I c6028',/boolean) eq 1 then filt=3   ;'I'

  ut = sxpar(head,'TIME-OBS')
  ;ut2 = strtrim(ut)
  ;ut2 = strmid(ut,0,10)
  ut2 = sexig2ten(strtrim(ut,2))
  ut2 = ut2[0]
  uthr = strtrim(long(ut2),2)
  if ut2 lt 10 then uthr='0'+uthr
  utmin = strtrim(long( (ut2-long(ut2))*60. ),2)
  if float(utmin) lt 10 then utmin='0'+utmin

  am = sxpar(head,'AIRMASS')

  ; This frame has a bad header
  if strtrim(am,2) eq 'Not' then begin
    if stregex(name,'obj2068',/boolean) eq 1 then am=1.786
    if strtrim(am,2) eq 'Not' then stop
  endif

  exp = sxpar(head,'EXPTIME')

  if am lt 1.0 then stop

  ; Aperture name
  aname = name+'a'

  ;format = '(A12,I3,A12,F11.3,F10.3)'
  ;printf,unit,format=format,name,filt,ut2,am,exp
  format='(A1,A-11,A22,A4,A3,F7.3,F10.3)'
  printf,unit,format=format,'',aname,filt,uthr,utmin,am,exp

end

close,unit
free_lun,unit


; Making the list of aperture photometry file
apfiles = file_search(dir,'*_*a.ap')
napfiles = n_elements(apfiles)

gd = where(stregex(apfiles,'Raw',/boolean,/fold_case) eq 0 and $
           stregex(apfiles,'twilight',/boolean,/fold_case) eq 0 and $
           stregex(apfiles,'bad',/boolean,/fold_case) eq 0 and $
           stregex(apfiles,'flat',/boolean,/fold_case) eq 0 and $
           stregex(apfiles,'zero',/boolean,/fold_case) eq 0,ngd)
apfiles = apfiles(gd)
napfiles = n_elements(apfiles)

if not keyword_set(silent) then print,'Making DAOGROW list file: ',file+'.ext'

openw,unit,/get_lun,file+'.ext'

for i=0,napfiles-1 do printf,unit,file_basename(apfiles[i])

close,unit
free_lun,unit

;stop

end
