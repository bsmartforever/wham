pro mscmagma_prep,mchfiles,apcfile,outfile,silent=silent

; This makes the input file for MSCMAGMA.PRO
; This must be run from the directory that has
; the FITS and MCH files.
; 
; The input list needs to be of MCH files.
; The apcfile is the aperture correction file

; Not enough inputs
if n_params() lt 2 then begin
  print,'Syntax - msmagma_prep,mchfiles,apcfile,outfile'
  return
endif

test = file_test(apcfile)
if test eq 0 then begin  
  print,'FILE ',apcfile,'DOES NOT EXIST'
  return
endif 


; A list file was input
if strmid(mchfiles[0],0,1) eq '@' then begin

  inp = strmid(mchfiles[0],1)

  ; Loading the files
  readcol,inp,list,format='A',/silent
  nlist = n_elements(list)

endif else begin

  ; Probably an array of filenames
  if n_elements(mchfiles) gt 1 then begin
    list = mchfiles
    nlist = n_elements(list)

  ; A globbed list or only one file
  endif else begin
    list = file_search(mchfiles)
    nlist = n_elements(list)
  endelse

endelse

; Read in the aperture correction file
; The apcor.lst file has the a.del file names
readcol,apcfile,apcnames,apcvalue,format='A,F',/silent
apcnames2 = repstr(apcnames,'a.del')  ; base names

; Opening the output file
if n_elements(outfile) eq 0 then begin
  outfile = 'mscmagma.input'
endif

print,'WRITING INPUT FILE TO ',outfile
openw,unit,/get_lun,outfile


; Looping through the MCH files
for i=0,nlist-1 do begin

  ; Load the file
  readcol,list[i],files,format='a',/silent

  files = strtrim(files,2)
  files = repstr(files,"'")
  nfiles = n_elements(files)

  ; Initializing arrays
  filtarr = strarr(nfiles)
  amarr = dblarr(nfiles)
  exparr = dblarr(nfiles)
  apcarr = dblarr(nfiles)

  ; Loop through the photometry files
  for j=0,nfiles-1 do begin

    file = files[j]
    arr = strsplit(file,'.',/extract)
    narr = n_elements(arr)
    if narr ge 2 then filebase = strjoin(arr[0,narr-2]) else filebase = file

    fitsfile = filebase+'.fits'

    ; Reading header of FITS file
    head = headfits(fitsfile)

    filter = sxpar(head,'FILTER')
    if stregex(filter,'M Washington',/boolean) eq 1 then filt='M'
    if stregex(filter,'D51 DDO',/boolean) eq 1 then filt='D'
    if stregex(filter,'I c6028',/boolean) eq 1 then filt='I'

    am = sxpar(head,'AIRMASS')
  
    ; This frame has a bad header
    if strtrim(am,2) eq 'Not' then begin
      if stregex(file,'obj2068',/boolean) eq 1 then am=1.786
      if strtrim(am,2) eq 'Not' then stop
    endif

    exp = sxpar(head,'EXPTIME')

    ; Getting the aperture correction
    gd = where(apcnames2 eq filebase,ngd)
    apcorr = 0.0
    if (ngd gt 0) then apcorr=apcvalue[gd[0]]
    if apcorr lt 0.0 then apcorr=0.0        ; don't want negative correction
      
    ; Pluggin into the arrays
    filtarr[j] = filt
    amarr[j] = am
    exparr[j] = exp
    apcarr[j] = apcorr

  end ; looping through phot files

  arr = strsplit(list[i],'.',/extract)
  narr = n_elements(arr)
  if narr ge 2 then filebase = strjoin(arr[0,narr-2]) else filebase = list[i]


  ; OUTPUTTING
  ; RAW filename, Band1 name, Band1 airmass, Band1 exptime, Band1 aperture correction, Band2 ...
  tab = '   '
  out = string(filebase,format='(A15)')+'.raw' + tab

  for j=0,nfiles-1 do begin
    out = out + string(filtarr[j],format='(A6)') + tab
    out = out + string(amarr[j],format='(F7.4)') + tab
    out = out + string(exparr[j],format='(F7.1)') + tab
    out = out + string(apcarr[j],format='(F7.4)') + tab
  end

  ; Writing to the output file
  printf,unit,out

end ; looping through MCH files

; Closing the output file
close,unit
free_lun,unit

;stop

end
