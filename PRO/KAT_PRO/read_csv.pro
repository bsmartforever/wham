;+
; NAME:
;    READ_CSV
;
; PURPOSE:
;    Reads a comma-separated-variables (CSV) file into a structure.
;
; CATEGORY:
;    I/O
;
; CALLING SEQUENCE:
;    Result = READ_CSV(Filename)
;
; INPUTS:
;    Filename:  Name of CSV file. The file must contain a header line
;               of the form "# COL1,COL2,COL3...".
;
; KEYWORD PARAMETERS:
;    ULON64:    A comma-separated list of column names that are to be
;               treated as unsigned long 64-bit integers.
;
;    STRING:    A comma-separated list of column names that are to be
;               treated as strings.
;
;    DOUBLE:    A comma-separated list of column names that are to be
;               treated as doubles.
;
;    NULL:      If there are null fields in the file, they will be assigned
;               to the string specified by the NULL keyword before being
;               cast to the appropriate type.
;
; OUTPUTS:
;    The function returns a structure containing one field for each column
;    in the file, named by the entry in the file header. All fields are
;    assumed to be doubles unless their name contains the string 'OBJID',
;    in which case they are assumed to be unsigned long 64-bit integers,
;    or 'NAME', in which case they are assumed to be strings (this comparison
;    is case-insensitive). These defaults can all be over-ridden by using the
;    ULON64, STRING, and DOUBLE keywords.
;    The structure also contains the fields NROWS and NCOLS which contain
;    the number of rows and columns respectively.
;
; EXAMPLE:
;    If the file 'galaxies.csv' consists of:
;
;    # NAME,OBJID,RA,DEC
;    M31,493,10.68,41.27
;    NGC 1068,92,40.67,-0.01
;
;    Then reading it in produces:
;
;    IDL> Galstruct = READ_CSV('galaxies.csv')
;    IDL> HELP, /STRUCT, Galstruct
;    ** Structure <8237564>, 6 tags, length=80, data length=80, refs=1:
;    NAME            STRING    Array[2]
;    OBJID           ULONG64   Array[2]
;    RA              DOUBLE    Array[2]
;    DEC             DOUBLE    Array[2]
;    NROWS           LONG                 2
;    NCOLS           LONG                 4
;
;
; MODIFICATION HISTORY:
;    Modified by: Jeremy Bailin
;    12 August 2008  Use IDL 5.6 FILE_LINES to count number of lines.
;    Modified by: Vittorio Brando
;    12 August 2008 added function get_nlines to count lines in not unix OS
;                   added function valid_tag_name to ensure that the
;                   tag names are IDL compliant 
;
;    Written by:   Jeremy Bailin
;    10 June 2008  Public release in JBIU
;    jbiu@astroconst.org
;
;-
;------------------------------------------------------------------------

function valid_tag_name,tmp_name
; ensure that the tag names are IDL compliant
; Tag names may not be IDL Reserved Words,
; and must be unique within a given structure
; Structure names and tag names follow the rules of IDL identifiers:
; they must begin with a letter; following characters can be letters,
; digits, or the underscore or dollar sign characters; and case is ignored.

reserved_words=['AND','BEGIN','BREAK','CASE','COMMON','COMPILE_OPT', $
                'CONTINUE','DO','ELSE','END','ENDCASE','ENDELSE','ENDFOR', $
                'ENDIF','ENDREP','ENDSWITCH','ENDWHILE','EQ','FOR', $
                'FORWARD_FUNCTION','FUNCTION','GE','GOTO','GT','IF', $
                'INHERITS','LE','LT','MOD','NE','NOT','OF','ON_IOERROR', $
                'OR','PRO','REPEAT','SWITCH','THEN','UNTIL','WHILE','XOR']
nonvalid_chars="[]() /|\,.<>!@#%^&*+=-"

    test_reserved = where(tmp_name eq reserved_words, test_result)
    if test_result ne 0 then  tmp_name+="_"  ;append underscore

    tmp_name=strjoin(STRSPLIT(tmp_name,nonvalid_chars,/extract),"_")

return, tmp_name
end
;------------------------------------------------------------------------

function read_csv, filename, ulon64=ul64str, string=strstr, double=dblstr, $
  null=nullstr

on_error, 0

nline=file_lines(filename) 
if nline eq 0 then begin
  print,''
  print,'** No lines to read in this CVS file.'
  print,''
  return, 0
endif
nrows=nline-1

if n_elements(nullstr) eq 0 then nullstr=''

openr, lun, filename, /get_lun
header = string('')
readf, lun, header
header_vars = strsplit(header, ',', /EXTRACT, COUNT=ncols)
if ncols eq 0 then message,'Header contains no columns'
; get rid of initial '# '
header_vars[0] = (stregex(header_vars[0],'(# *)?(.*)',/SUBEXP,/EXTRACT))[2]

if n_elements(ul64str) gt 0 then $
  ul64_list = strupcase(strsplit(ul64str, ',', /EXTRACT)) $
  else ul64_list=''
if n_elements(strstr) gt 0 then $
  str_list = strupcase(strsplit(strstr, ',', /EXTRACT)) $
  else str_list=''
if n_elements(dblstr) gt 0 then $
  dbl_list = strupcase(strsplit(dblstr, ',', /EXTRACT)) $
  else dbl_list=''

upcasehead = strupcase(header_vars)
objidp = bytarr(ncols)
for i=0L,ncols-1 do begin
  if stregex(strupcase(header_vars[i]), 'OBJID', /BOOLEAN) then objidp[i]=1 $
  else if stregex(strupcase(header_vars[i]), 'NAME', /BOOLEAN) then objidp[i]=2
  if total( strupcase(header_vars[i]) eq ul64_list ) gt 0 then objidp[i]=1
  if total( strupcase(header_vars[i]) eq str_list ) gt 0 then objidp[i]=2
  if total( strupcase(header_vars[i]) eq dbl_list ) gt 0 then objidp[i]=0
endfor

; ensure that the tag names are IDL compliant
for i=0L,ncols-1 do header_vars[i]=valid_tag_name(header_vars[i])

; create structure
case objidp[0] of
  0 : outstruct = create_struct(header_vars[0], dblarr(nrows))
  1 : outstruct = create_struct(header_vars[0], ulon64arr(nrows))
  2 : outstruct = create_struct(header_vars[0], strarr(nrows))
endcase

for i=1L,ncols-1 do begin
  case objidp[i] of
    0 : outstruct = create_struct(outstruct, header_vars[i], dblarr(nrows))
    1 : outstruct = create_struct(outstruct, header_vars[i], ulon64arr(nrows))
    2 : outstruct = create_struct(outstruct, header_vars[i], strarr(nrows))
  endcase
endfor
outstruct = create_struct(outstruct, 'nrows', nrows, 'ncols', ncols)

for i=0L,nrows-1 do begin
  input_line = string('')
  readf, lun, input_line
  values = strsplit(input_line, ',', /EXTRACT, COUNT=nlinecols)
  if nlinecols ne ncols then message,'Incorrect number of columns in line '+ $
    string(i+1,format='(I0)')
  wherenull = where(values eq 'null', nnull)
  if nnull gt 0 then values[wherenull]=nullstr
  for j=0,ncols-1 do begin
    case objidp[j] of
      0 : outstruct.(j)[i] = double(values[j])
      1 : outstruct.(j)[i] = ulong64(values[j])
      2 : outstruct.(j)[i] = values[j]
    endcase
  endfor
endfor


close, lun
free_lun, lun

return, outstruct

end