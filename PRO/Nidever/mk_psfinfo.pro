pro mk_psfinfo,dir,stp=stp

;+
; This makes the psfinfo.X file for filldaophot.pro
; Input a list of directories.  Look in these
; directories for *a.ap files and their associated FITS files.
;
; INPUTS:
;  dir   List of directories
;
; OUTPUTS: 
;  psfinfo.X files for each filter 
;
; By D.Nidever  Dec.2006
;-

if n_elements(dir) eq 0 then begin
  print,'Syntax - mk_psfinfo,dir'
  return
endif

; A list file was input
if strmid(dir[0],0,1) eq '@' then begin

  inp = strmid(dir[0],1)

  ; Loading the files
  readcol,inp,dirs,format='A',/silent
  ndirs = n_elements(dirs)

endif else begin

  ; Probably an array of filenames
  if n_elements(dir) gt 1 then begin
    dirs = dir
    nlist = n_elements(dirs)

  ; A globbed list or only one file
  endif else begin
    dirs = file_search(dir,/test_directory)
    ndirs = n_elements(dirs)

    ; File not found
    if dir[0] eq '' then begin
      print,'DIR ',dir[0],' NOT FOUND'
      return
    endif

  endelse

endelse

; Initializing the arrays
n = 10000.
namearr = strarr(n)
fieldarr = strarr(n)
filtarr = strarr(n)
nightarr = lonarr(n)
utarr = strarr(n)
amarr = fltarr(n)
exparr = fltarr(n)

count = 0

; Loop through the directories
for i=0,ndirs-1 do begin

  files = file_search(dirs[i]+'*a.ap')
  dum = where(files ne '',nfiles)

  ; Loop through the files
  for j=0,nfiles-1 do begin

    ; NAME
    name = file_basename(files[j],'a.ap')

    ; Checking for the FITS file
    dir1 = file_dirname(files[j])
    fitsfile = dir1 + '/' + name + '.fits'

    test = file_test(fitsfile)
    if test eq 0 then begin
      print,'FILE ',fitsfile,' DOES NOT EXIST'
      goto,BOMB
    endif

    ; Getting the header
    head = headfits(fitsfile)

    ; FIELD, getting it from the directory name
    field='XXXX'
    if (stregex(dir1,'N3680',/boolean,/fold_case) eq 1) then field='N3680'
    if (stregex(dir1,'NGC3680',/boolean) eq 1) then field='N3680'
    if (stregex(dir1,'SA110',/boolean) eq 1) then field='SA110'
    if (stregex(dir1,'SA98',/boolean) eq 1) then field='SA98'
    if (stregex(dir1,'SA114',/boolean) eq 1) then field='SA114'
    ;object = sxpar(head,'OBJECT')
    ;object = strtrim(object,2)
    ;dum = strsplit(object,' ',/extract)
    ;field = strtrim(dum[0],2)

    ; NIGHT
    ; Assume the 4th digit gives the night
    ; i.e. obj1140_13.fits -> 1
    night = long(strmid(name,3,1))

    ; FILTER
    filter = sxpar(head,'FILTER')
    filt = strcompress(filter,/remove_all)
    if stregex(filter,'M Washington',/boolean) eq 1 then filt='M'
    if stregex(filter,'D51 DDO',/boolean) eq 1 then filt='DDO51'  ;'D'
    if stregex(filter,'I c6028',/boolean) eq 1 then filt='T2'     ;'I'

    ; UT TIME
    date = sxpar(head,'DATE-OBS')
    dum = strsplit(date,'T',/extract)
    ; Using DATE-OBS
    if strtrim(date,2) ne '0' or n_elements(dum) gt 1 then begin
      ut = strtrim(dum[1],2)
      ut2 = strmid(ut,0,8)

    ; Using TIME-OBS
    endif else begin
      time = sxpar(head,'TIME-OBS')
      ; NO time
      if strtrim(time,2) ne '0' then begin
        ut2 = strtrim(time,2)
        ut2 = strmid(ut2,0,8)
      endif else begin
        print,'ERROR: NO VALID DATE-OBS or TIME-OBS INFORMATION IN HEADER'
        ut2 = '99:99:99'
      endelse
    endelse ; using TIME-OBS


    ; AIRMASS
    am = sxpar(head,'AIRMASS')

    ; This frame has a bad header
    if strtrim(am,2) eq 'Not' then begin
      if stregex(name,'obj2068',/boolean) eq 1 then am=1.786
      if strtrim(am,2) eq 'Not' then stop
    endif

    if am lt 1.0 then stop

    ; EXPOSURE TIME
    exp = sxpar(head,'EXPTIME')
    exp = float(exp)


    ; Aperture name
    aname = name+'a'


    ; Adding to the arrays
    namearr[count] = name
    fieldarr[count] = field
    filtarr[count] = filt
    nightarr[count] = night
    utarr[count] = ut2
    amarr[count] = am
    exparr[count] = exp

    count = count+1

    BOMB:

  end ; file loop

end ; directory loop

; Clipping of the end of arrays
namearr = namearr[0:count-1]
fieldarr = fieldarr[0:count-1]
filtarr = filtarr[0:count-1]
nightarr = nightarr[0:count-1]
utarr = utarr[0:count-1]
amarr = amarr[0:count-1]
exparr = exparr[0:count-1]


; How many filters are there
ui = uniq(filtarr,sort(filtarr))
filters = filtarr(ui)
nfilters = n_elements(filters)

; Making one psfinfo.X file for each filter
for i=0,nfilters-1 do begin

  ; Open the file
  print,'MAKING psfinfo.'+filters[i]
  openw,unit,/get_lun,'psfinfo.'+filters[i]

  ; Getting all the field for this filter
  ind = where(filtarr eq filters[i],nind)

  ; Looping over the fields
  for j=0,nind-1 do begin

    name = namearr[ind[j]]
    field = fieldarr[ind[j]]
    filt = filtarr[ind[j]]
    night = nightarr[ind[j]]
    template = field+filt
    am = amarr[ind[j]]
    ut = utarr[ind[j]]
    exp = exparr[ind[j]]

    ;image name       image name  filt night template airmass  UT           exposure
    ;-------------------------------------------------------------------------------
    ;ccd217_final    ccd217_final    V  1    RU149V  1.305   08:23:37         5.00
    ;obj2047_13      obj2047_13      M  2NGC3680_21M   1.889   00:37:26         4.00


    format='(A-16,A-16,A-6,I-3,A-13,F7.3,A11,F13.2)'
    printf,unit,format=format,name,name,filt,night,template,am,ut,exp

  end
  
  ; Close the file
  close,unit
  free_lun,unit

end

print,''
print,'DOUBLE-CHECK THESE FILES'

if keyword_set(stp) then stop

end
