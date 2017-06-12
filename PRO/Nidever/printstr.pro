pro printstr,str,file,noheader=noheader,dbformat=dbformat,stp=stp

; This program prints a structure to an ASCII file

if not keyword_set(file) then file='idl.txt'

if not keyword_set(str) then begin
  print,'Syntax - printstr,str,file,/noheader,dbformat=dbformat,stp=stp'
  return
endif

;format = '('

tags = tag_names(str)
ntags = n_elements(tags)

for i=0,ntags-1 do begin

  sz = size(str(0).(i))
  nsz = n_elements(sz)
  type = sz(nsz-2)
  dim = sz(0)
 
  ;if i ne ntags-1 then add = ',' else add=''
  add = ','

  ; Figuring out what the format is and the names
  case dim of
    0: begin
         push,names,tags(i)
         ;format = format+formatize(type)+add
         push,format,'('+formatize(type)+')'
       end
    1: begin
         n = sz(1)
         for j=0,n-1 do begin
           push,names,tags(i)+strtrim(j,2)
           ;format = format+formatize(type)+add
           push,format,'('+formatize(type)+')'
         end
       end

    2: begin
         n1 = sz(1)
         n2 = sz(2)
         for j=0,n1-1 do begin
           for k=0,n2-1 do begin
             push,names,tags(i)+strtrim(j,2)+strtrim(k,2)
             ;format = format+formatize(type)+add
             push,format,'('+formatize(type)+')'
           end  ; for k
         end  ; for j
       end ; 2
  endcase

end ; for i

;; dbformat, need to add a key column
;if keyword_set(dbformat) then begin
;  names = ['KEY',names]
;  format = ['('+formatize(3)+')',format]
;endif

;len = strlen(format)
;format = strmid(format,0,len-1)
;format = format+')'

openw,unit,/get_lun,file

; Printing the header
if not keyword_set(noheader) then begin
  head = '# '

  if keyword_set(dbformat) then head = head+'     KEY     '

  nnames = n_elements(names)
  for i=0,nnames-1 do begin
    len = strlen(names(i))                     ; length of name
    ind = strpos(format(i),'.')                ; position of . in format string

    ; Floating point
    if ind(0) ne -1 then begin
      spaces = long(strmid(format(i),2,ind-2))   ; 
    endif

    ; Integer or Character
    if ind(0) eq -1 then begin
      len2 = strlen(format(i))
      spaces = long(strmid(format(i),2,len2-3))      
    endif

    nfirst = floor(float(spaces-len)*0.5)
    nlast = spaces - (len+nfirst)
    first = string(replicate(32B,nfirst))
    last = string(replicate(32B,nlast))
    head = head+first+names(i)+last

  end

  printf,unit,head

endif

nrec = n_elements(str)

; Looping through records
for i=0LL,nrec-1 do begin

  cnt = 0
  text = ''

  if keyword_set(dbformat) then begin
    text = text + string( i+1, FORM='('+formatize(3)+')')
  endif

  ; Looping through tags
  for j=0,ntags-1 do begin

    sz = size(str(0).(j))
    nsz = n_elements(sz)
    type = sz(nsz-2)
    dim = sz(0)
 
    case dim of
      0: begin
           text = text + string( str[i].(j), FORM=format[cnt++])
         end
      1: begin
           n = sz(1)
           for k=0,n-1 do begin
             text = text + string( (str[i].(j))(k), FORM=format[cnt++])        
           end ; for k
         end

      2: begin
           n1 = sz(1)
           n2 = sz(2)
           for k=0,n1-1 do begin
              for l=0,n2-1 do begin
                text = text + string( (str[i].(j))(k,l), FORM=format[cnt++])
             end  ; for l
          end  ; for k
         end  ; 2
    endcase

  end ; for j

  printf,unit,text

end  ; for i

close,unit
free_lun,unit
print,'STRUCTURE written to: ',file

; DBFORMAT file
if keyword_set(dbformat) then begin
  if (string(dbformat) eq '1' or size(dbformat,/type) ne 7) then $
    dbfile = 'idl.dbformat' else dbfile=dbformat

  openw,unit,/get_lun,dbfile

  printf,unit,'KEY            int     %13d   key'

  for i=0,nnames-1 do begin
    form = format(i)
    name = names(i)
    len = strlen(name)
    space1 = string(replicate(32B,15-len))
    space2 = string(replicate(32B,4))
    lo = strpos(form,'.')
    length = long(strmid(form,2,lo-2))

    ; Float or Double
    if strmid(form,1,1) eq 'G' then begin
      hi = strpos(form,')')
      typ1 = 'real'
      typ2 = '%'+strmid(form,2,hi-2)
    endif
    ; Integer
    if strmid(form,1,1) eq 'I' then begin
      len = strlen(form)
      length = long(strmid(form,2,len-3))      
      typ1 = 'int'
      typ2 = '%'+strtrim(length,2)
    endif
    ; Character
    if strmid(form,1,1) eq 'A' then begin
      len = strlen(form)
      length = long(strmid(form,2,len-3))
      typ1 = 'char'
      typ2 = '%'+strtrim(length,2)+'s  '+strtrim(length,2)
    endif
    printf,unit,name,space1,typ1,space2,typ2

  end

  close,unit
  free_lun,unit

  print,'DBFORMAT file written to: ',dbfile

  ; Creating keys file
  openw,unit,/get_lun,'idl.keys'
  printf,unit,'KEY '+strtrim(names(0),2)
  printf,unit,strtrim(names(0),2)+' '+strtrim(names(1),2)
  printf,unit,strtrim(names(1),2)+' '+strtrim(names(2),2)
  close,unit
  free_lun,unit

  print,'KEYS file written to: idl.keys'  

end

if keyword_set(stp) then stop

end

