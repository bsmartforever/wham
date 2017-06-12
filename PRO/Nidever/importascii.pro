;+
;
; IMPORTASCII.PRO
;
; This imports an ascii file by using the IDL read_ascii.pro
; routine.  An ASCII template has to be built on the fly.
; This is normally done interactively with ascii_template.pro
;
; INPUT:
;  fname       Filename of the data file
;  comment     Comment string.  comment='#' by default.
;  /indef      Replace INDEFs with 999999
;  /allstr     Make all columns strings
;  /noprint    Don't print anything
;  fieldnames  Array of field names.
;  fieldtypes  Array of the IDL field types
;  skipline    Number of lines you want to skip at the beginning
;  /allfloat   All columns are floats (and no INDEFS) and can
;              be read more quickly with READF than READ_ASCII
;  /header     The file has column headers on the first line.
;              Use these for the field names
;  =delimit    The delimiting character, space or tab used by default.
;
; Future improvements:
;  Check for end of file better.
;
; Makes use of DATATYPES.PRO
;
; By David Nidever  Feb. 2006
;-


Function datatypes,arr

;+
; This function figures out what data types strings have
;
; INPUT
;  arr   Array of strings
; 
; OUPUT
;  typearr   Array of the data types (3-Long Integer, 4-Float, 5-Double, 7-String)
;            See the documentation for SIZE for a description of data types.
;
; David Nidever   Feb.2006
;-

; No Parmaters input
if n_params() eq 0 then begin
  print,'Syntax - typearr = datatypes(array)'
  return,-1
endif


npar = n_elements(arr)
typearr = lonarr(npar)

; Figuring out each column's data type
for i=0,npar-1 do begin
  var = strtrim(arr(i),2)
  bvar = byte(var)
  nvar = n_elements(bvar)

  ; If the string has only 0-9, -, +, ., E, e then it's a float, otherwise a string
  ; 0-9 is 48-57 (in bytes)
  ; "-" is 45
  ; "+" is 43
  ; "." is 46
  ; "E" is 69
  ; "e" is 101
  bfloat = [bindgen(10)+48B,43B,45B,46B,69B,101B]
  bint = [bindgen(10)+48B,43B,45B]

  ; Checking each character in "bvar"
  ; Are there any non-"number" characters?
  badfloat = 0           ; float until proven otherwise
  badint = 0
  last = 0B
  for j=0,nvar-1 do begin
    ; Checking for float characters
    g = where(bvar(j) eq bfloat,ng)  ; is this character a float characters
    if ng eq 0 then badfloat=1

    ; Checking for integer characters
    g = where(bvar(j) eq bint,ng)    ; is this character an integer characters
    if ng eq 0 then badint=1

    ; Checking for plus or minus, must be at beginning, okay after 'E' or 'e'
    if (bvar(j) eq 43B) or (bvar(j) eq 45B) then begin
         if (j ne 0) and (last ne 69B) and (last ne 101B) then badfloat=1
         badint = 1
    endif

    ; Checking for period, CAN'T be at beginning
    if (bvar(j) eq 46B) then begin
      if (j eq 0) then badfloat=1
      badint = 1
    endif

    last = bvar(j)
  end

  ; String
  if (badfloat eq 1) then type = 7   ; String

  ; Float
  if (badfloat eq 0 and badint eq 1) then begin

    ; Float or Double?
    dec = first_el(strsplit(var,'.',/extract),/last)
    ndec = strlen(dec)

    ; type = 5, Double
    ; type = 4, Float
    if (ndec gt 6) then type=5 else type=4
  endif

  ; Long Integer
  if (badfloat eq 0 and badint eq 0) then type = 3   ; Integer (Long)

  ; Long64 integer
  if (badfloat eq 0 and badint eq 0 and nvar gt 9) then type = 14   ; Long64

  ; NAN's are floats
  if strtrim(strupcase(var),2) eq 'NAN' then type = 4     ; float

  typearr(i) = type

end

;stop

return,typearr

end


;---------------------------------------------------------------------


Function importascii,fname,indef=indef,allstr=allstr,$
         comment=comment,noprint=noprint,fieldnames=fieldnames0,$
         allfloat=allfloat,skipline=skipline,stp=stp,fieldtypes=fieldtypes,$
         header=header,delimit=delimit

; No parameters input
if n_params() eq 0 then begin
  print,'Syntax - arr=importascii(filename,/indef,/allstr,comment=comment,'
  print,'                         /noprint,fieldnames=fieldnames,/allfloat)'
  return,-1
endif

; Skip header line
if keyword_set(header) and not keyword_set(skipline) then skipline=1

if n_elements(comment) eq 0 then comment='#'   ; Comment string
if n_elements(indef) eq 0 then indef=1         ; By default remove indef
if n_elements(allstr) eq 0 then allstr=0       ; By default use proper types
if n_elements(skipline) eq 0 then data_start=0 else data_start=skipline  ; number of lines to skip

if keyword_set(fieldtypes) then typearr = fieldtypes
if keyword_set(fieldnames0) then fieldnames = fieldnames0


; Searching for the file
d = file_search(fname)
if (d eq '') then begin
  print,'FILE ',fname,' NOT FOUND!!'
  return,-1
endif

; FIGURING out how many columns there are
str1=''
openr,unit,fname,/get_lun

; Skipping lines
if (data_start gt 0) then begin
  dum=''
  for i=0,data_start-1 do readf,unit,dum
end

; Making sure the line IS NOT commented out
readf,unit,str1
first = strmid(str1,0,1)
while((first eq comment) and (eof(unit) eq 0)) do begin
  readf,unit,str1
  first = strmid(str1,0,1)
end

close,unit
free_lun,unit

; Default delimiter character
if not keyword_set(delimit) then begin
  delimit = 32B  ; space is the default delimiter

  ; Split the string
  arr1 = strsplit(str1,' ',/extract)
  arr2 = strsplit(str1,string(9B),/extract)  ; tab

  ; Use tab as delimiter
  if n_elements(arr2) gt n_elements(arr1) then begin
    delimit = 9B
    arr1 = arr2
  end

; Using INPUT delimiter
endif else begin
  arr1 = strsplit(str1,delimit,/extract)
endelse


ncol = n_elements(arr1)  ; # of columns


; Checking for ALL FLOATS
typearr = datatypes(arr1)
bd = where(typearr eq 7,nbd)
if (nbd eq 0) then begin
  if not keyword_set(noprint) then print,'ALL FLOATS?'
end

; Using the header line
if keyword_set(header) then begin
  headstr=''
  openr,unit,fname,/get_lun
  readf,unit,headstr
  close,unit
  free_lun,unit

  ; Remove comment strings
  headstr = stress(headstr,'D',0,'#')
  headstr = stress(headstr,'D',0,comment)

  ; Split the string
  headarr = strsplit(headstr,delimit,/extract)
  nhead = n_elements(headarr)

  ; Picking fieldnames
  if nhead ge ncol then begin
    fieldnames = headarr[0:(nhead-1) < (ncol-1)]
  endif else begin
    print,'Headers do not match # of columns'
    stop
  endelse

  ;stop

endif


; Field Names
if not keyword_set(fieldnames) then $
  fieldnames = 'FIELD'+strtrim(sindgen(ncol),2)
if n_elements(fieldnames) ne ncol then begin
  print,'Input array of field names not of the correct size'
  fieldnames = 'FIELD'+strtrim(sindgen(ncol),2)
  dum=''
  read,dum,'Do you want to continue?'
  if strlowcase(strmid(strtrim(dum,2),0,1)) ne 'y' then stop
  ;stop
endif 


; Using READ_ASCII to read the data
IF not keyword_set(allfloat) THEN BEGIN

  START:

  ; All strings at the beginning
  typearr = lonarr(ncol)+7

  ; Figuring out field locations.
  fieldlocations = strsplit(str1,string(delimit))   ; this returns the locations
  fieldgroups = lindgen(ncol)

  ; Creating the template
  template = {version:1.0, datastart:0, delimiter:delimit, missingvalue:!values.f_nan,$
              commentsymbol:'#',fieldcount:ncol,fieldtypes:typearr,fieldnames:fieldnames,$
              fieldlocations:fieldlocations,fieldgroups:fieldgroups}

  ; Reading the Data
  arr = read_ascii(fname,template=template,data_start=data_start)

  nrow = n_elements(arr.(0))

  ; Making it into a "normal" structure
  cmd = 'dum = {'
  for i=0,ncol-1 do begin
    if typearr(i) eq 3 then char = '0L'
    if typearr(i) eq 4 then char = '0.0'
    if typearr(i) eq 5 then char = '0.d0'
    if typearr(i) eq 7 then char = '""'
    if typearr(i) eq 14 then char = '0LL'
    cmd=cmd+fieldnames(i)+':'+char
    if i ne ncol-1 then cmd = cmd+', '
  end
  cmd = cmd+'}'

  ddd = execute(cmd)
  str = replicate(dum,nrow)

  ; Transferring the data
  for i=0,ncol-1 do str.(i) = arr.(i)
  undefine,arr

  ; Converting INDEF's to 999999
  if keyword_set(indef) then begin

    nbad = 0

    ; Looping through the fields
    for i=0,ncol-1 do begin
      col = strtrim(reform(str.(i)),2)
      bd = where(col eq 'INDEF' or col eq "'INDEF'",nbd)
      if nbd gt 0 then str(bd).(i) = '999999'
      nbad = nbad + nbd
    end

    if (not keyword_set(noprint)) then $
      if (nbad gt 0) then print,"INDEF's converted to 999999"

  endif

  ; Converting them to the proper types
  if not keyword_set(allstr) then begin

    ; Getting the data types
    if not keyword_set(fieldtypes) then begin
      for i=0,ncol-1 do begin

        ; Check the first 100 (or nrow) for the type
        ; Use the maximum type
        ; The higher types encompass all othe lower ones.
        ; 7-string > 5-double > 4-float > 3-long > 2-int
        ;type = datatypes(str(0).(i))
        type100 = datatypes(str(0:99<(nrow-1)).(i))    ; using first 100 rows 
        type = max(type100)                            ; use the maximum 
        typearr(i) = type
      end
    endif else begin
      typearr = fieldtypes
    endelse

    ; Making a new structure with the proper types
    cmd = 'dum = {'
    for i=0,ncol-1 do begin
      if typearr(i) eq 3 then char = '0L'
      if typearr(i) eq 4 then char = '0.0'
      if typearr(i) eq 5 then char = '0.d0'
      if typearr(i) eq 7 then char = '""'
      if typearr(i) eq 14 then char = '0LL'
      cmd=cmd+fieldnames(i)+':'+char
      if i ne ncol-1 then cmd = cmd+', '
    end
    cmd = cmd+'}'

    ddd = execute(cmd)
    arr = replicate(dum,nrow)

    ; Transferring the data
    for i=0,ncol-1 do arr.(i) = str.(i)
    str = arr
  end


; /ALLFLOAT, Using READF to read the data
ENDIF ELSE BEGIN

  ; Checking the data types
  typearr = datatypes(arr1)
  bd = where(typearr eq 7,nbd)

  ; Some of the columns are strings
  if nbd gt 0 then begin
    print,'Some of the columns are strings'
    print,'Using READ_ASCII to read the data'
    goto,START
  endif

  nrow = file_lines(fname)
  arr = fltarr(ncol,nrow)

  ; Reading the data
  openr,unit,fname,/get_lun

  ; Skipping lines
  if (data_start gt 0) then begin
    dum=''
    for i=0,data_start-1 do readf,unit,dum
  endif

  readf,unit,arr
  close,unit
  free_lun,unit

  ; Creating the normal structure
  cmd = 'dum = {'
  for i=0,ncol-1 do begin
    cmd=cmd+fieldnames(i)+':0.0'
    if i ne ncol-1 then cmd = cmd+', '
  end
  cmd = cmd+'}'

  ddd = execute(cmd)
  str = replicate(dum,nrow)

  ; Transferring the data
  for i=0,ncol-1 do str.(i) = reform(arr(i,*))

ENDELSE


; Report on the data
if not keyword_set(noprint) then $
  print,strtrim(ncol,2),' columns x ',strtrim(nrow,2),' rows'


if keyword_set(stp) then stop

return,str

end
