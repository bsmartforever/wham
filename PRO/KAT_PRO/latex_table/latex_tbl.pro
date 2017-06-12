pro latex_tbl, fits_fl, keys_fl, extension=extension, root=root

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose:
;   Produces the desired latex table from the 
;   existing fits table. Two mandatory 
;   and one optional parameters are
;   used: name of fits table file,
;   name of ascii file which specifies names
;   and formats of the fields that should be
;   included from fits table to latex table, 
;   and optional EXTENSION keyword, which
;   points to the fits extension (0 for primary).
;   If EXTENSION is not specified, then 0 is assumed.
;
; Input:
;   fits_fl - Either a fits file, a structure, or 
;             a save file (*.sav or *.save) containing
;             a single structure. Note that the program
;             is robust enough to extract a structure with 
;             a different name from the root of the save 
;             file name.
;   keys_fl - A text file containing the tag names,
;             format, error flag, and colhead label.
;             
;             tag names:
;                 Same tag as either the fits file 
;             keywords or as the structure tag.
;
;             format:
;               C style formating, with %S for a 
;             string, %10.5f for a float, %10.5e
;             for exponential, and %I for an integer.
;
;             error tag:
;               err for a global variable uncertainty value.
;             err_u for an upper uncertainty value. 
;             err_l for a lower uncertainty value.
;
;             colhead label:
;               Suppiled colhead labels are optional.
;             By default colhead will be labeled as 
;             column numbers. If colhead labels are 
;             supplied, then BLANK should be supplied 
;             to any tags that do not contain errors
;             in the third column of the keys_fl input
;             file. 
;
;  extension - The extension number to extract the 
;             data and table information from. 
;             The default value is 0.
;
;  root     - root of the output latex table file, of form
;               root.tex. The default root is latex_tbl.
;
; Example:
; latex_table, 'table.fits', 'keys.txt', EXTENSION=1
;
; where keys.txt consists of 3 columns: name 
; and format of intersted fields, and error 
; marker. Error marker (third column) is optional,
; if it is not specified then a separate latex columns
; will be generated for each key. Else if an error
; marker is specified then the key is assumed to be
; an error and will be compressed within the same 
; latex column where the presiding key is located.
; There are 3 types of error markers: err, err_l, and
; err_u. err is used for symmetric errors, and err_l,
; err_u - for nonsymmetric ones.   
; Here is an example of keys.txt:
;     
;     SOURCENAME   %s
;     RA           %10.5f
;     DEC          %10.5f
;     NH           %10.5f
;     NH_ERRL      %10.2f   err_l
;     NH_ERRU      %10.2f   err_u
;     KT           %10.2f
;     KT_ERRL      %10.2f   err_l
;     KT_ERRU      %10.2f   err_u
;     DUM          %10.2e
;     DUM_ERR      %10.2e   err   
;                 
; In this case the latex table will have 6 columns.
; The fourth column will have a format:
; $NH^{+NH_ERRU}_{-NH_ERRL}$. The fifth:
; $KT^{+KT_ERRU}_{-KT_ERRL}$. The sixth:
; $DUM\pmDUM_ERR$. 
; Remind user the C-style format which is 
; defined in the second column.
; %[w]s - string, %[w.d]f - floating-point fixed
; notation, %[w.d]e - floating-point scientific
; notation, %[w]i - integer. [w.d] are optional.
; w - an optional width, d - optional number of 
; positions after decimal point.
;
; Example:
;   latex_tbl,ha,'ha.keylist',root='ha_table'
;     Where ha is a structure.
;   latex_tbl,'ha.sav','ha.keylist',root='ha_table'
;     Where ha.sav is a save file containing a structure, 
;     not necessary of the variable name ha.
;
; Version: 1.0
; Author: Konstantin Getman, gkosta@astro.psu.edu 
;
; Version: 2.0 
; Edited by Dr. Kat Barger 09/2013
;   - Now accepts structures as well as *.fits files
;   - Now formats GLON, GLAT, MLON, and MLAT with \fgd,
;     such that a degree symbol appears over the decimal. 
;     This will not be applied if the supplied format is 
;     an integer, a.k.a., %I.
;   - Added the ability to define colhead. Supply this as 
;     the forth column in the keys_fl file. If this is 
;     supplied, any non-error variable should be given 
;     marked as with the word 'BLANK'
;   - Changed the format of all table numbers to 
;     LaTeX matimatical form with $ $ surrounding them.
;   - Added the root keyword
;
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;print, 'START'
if (NOT keyword_set(root)) OR (size(root,/type) NE 7) then $
  basename='latex_tbl' else $
  basename=root
tex_filename=basename+'.tex'
ps_filename=basename+'.ps'

if (size(fits_fl,/type) eq 7) then begin
  ;Check to see if fits_fl is a save file
  if strmatch(fits_fl,'*.sav*',/fold_case) then begin

    ; Create a savefile object. 
    sObj = OBJ_NEW('IDL_Savefile', fits_fl) 
     
    ; Get the variable name of the structure 
    ; contained in the save file
    sNames = sObj->Names() 
    restore,fits_fl
    fits_fl=SCOPE_VARFETCH(sNames[0], LEVEL=0)
    if size(fits_fl,/type) NE 8 then begin
      print,''
      print,'*** The supplied save file did not contain a structure ***'
      print,'        *** Or contained multiple variables ***'
      print,'*** Please only supply save files containing 1 structure ***'
      print,''
      return
    endif
  endif
endif

;check to see if a structure was passed
if (size(fits_fl,/type) NE 8) then begin
  if (NOT strmatch(fits_fl,'*.fits*',/fold_case)) then begin
      print,''
      print,'*** Please provide either a fits file, save file, or a structure ***'
      print,''
      return
  endif
  ;; read fits table
  if(NOT keyword_set(extension)) then extension=0
  fits_table=mrdfits(fits_fl, extension,/SILENT, STATUS=status)
  if (status NE 0) then begin
    print, 'Error. Can not read '+fits_fl
    stop
  endif
endif else fits_table=fits_fl  
  num_sources = n_elements(fits_table.(0))

;; read interested keys 
readcol, keys_fl, keys, FORMAT='A',COMMENT='#', DELIMITER='|',/silent
num_keys=n_elements(keys)
keys_name=strarr(num_keys)
keys_format=strarr(num_keys)
err_info=strarr(num_keys)
colhead=strarr(num_keys)

for iii=0,num_keys-1 do begin
  keys_name[iii]=strupcase((strsplit(strtrim(keys[iii],2),/EXTRACT))[0])
  keys_format[iii]=strupcase((strsplit(strtrim(keys[iii],2),/EXTRACT))[1])
  num_fields = n_elements(strsplit(strtrim(keys[iii],2),/EXTRACT))
  if (num_fields GE 3) then begin
    err_info[iii]=strupcase((strsplit(strtrim(keys[iii],2),/EXTRACT))[2])
  endif else begin
    err_info[iii]='BLANK'
  endelse
  if (num_fields GE 4) then begin
    tmp=(strsplit(strtrim(keys[iii],2),/EXTRACT))[3:*]
    for j=0, n_elements(tmp)-1 do colhead[iii]=colhead[iii]+tmp[j]+' '
  endif else begin
    colhead[iii]='BLANK'
  endelse
endfor

;; check keys
; err_info for the first row should be BLANK
if(err_info[0] NE 'BLANK') then begin
  print, 'Error. First row can not store error field.'
  stop
endif
 
for iii=0, num_keys-1 do begin
  
  ; check key exists in fits_table
  if (NOT tag_exist(fits_table,keys_name[iii])) then begin
    print, 'Error. Key ',keys_name[iii],' does not exist in fits table.'
    stop
  endif  

  ; check key format
  regex_str='^%{1}[0-9.]*(d|e|f|i|g|o|s|x|z)$'
  if (NOT stregex(keys_format[iii],regex_str,/BOOLEAN,/FOLD_CASE)) then begin
    print, 'Error. Wrong format type: '+keys_format[iii]
    stop
  endif

  ; check err_info column
  temp_str = ['ERR', 'ERR_U', 'ERR_L', 'BLANK']
  if(total(strmatch(temp_str,err_info[iii], /FOLD_CASE)) EQ 0) then begin
    print, 'Error. Third column has something different from (err, err_u, err_l, or BLANK).'
    stop
  endif  

endfor

  ; check number of err, err_u, and err_l
  index_err_u=where(strupcase(err_info) EQ 'ERR_U', c_err_u)
  index_err_l=where(strupcase(err_info) EQ 'ERR_L', c_err_l)
  if (c_err_u NE c_err_l) then begin
    print, 'Error. Number of err_u should be equal to number of err_l.'
    stop  
  endif
  index_err=where(strupcase(err_info) EQ 'ERR', c_err)
  if (c_err_u GT 0) then begin
    index_diff=abs(index_err_u-index_err_l)
    index_temp=where(index_diff NE 1, c_temp) 
    if (c_temp NE 0) then begin
      print, 'Error. The rows with err_u and err_l should be colocated.'
      stop
    endif

    if (c_err GT 0) then begin
      for iii=0, n_elements(index_err)-1 do begin 
        index_diff1=abs(index_err[iii]-index_err_l)
        index_diff2=abs(index_err[iii]-index_err_u)
        index_temp1=where(index_diff1 EQ 1, c_temp1)
        index_temp2=where(index_diff2 EQ 1, c_temp2)
        if (c_temp1 NE 0) OR (c_temp2 NE 0) then begin
          print, 'Error in row with the key #'+strtrim(string(index_err[iii]+1),2)+$
                 '. The rows with err and err_u(err_l) should not be colocated.'
          stop
        endif
        index_diff=abs(index_err[iii]-index_err)
        index_temp=where(index_diff EQ 1, c_temp)
        if (c_temp NE 0) then begin
          print, 'Error in row with the key #'+strtrim(string(index_err[iii]+1),2)+$
                 '. The rows with err should not be colocated.'
          stop
        endif   
      endfor  
    endif
  endif
   
;; define latex_table as a substructure of fits_table
tag_ind=0
for iii=0,num_keys-1 do begin
  name_of_tag = keys_name[iii]
  status=tag_exist(fits_table,keys_name[iii],INDEX=temp_ind)
  val=fits_table[0].(temp_ind)[0]
  null_val=(make_array(1,TYPE=size(val,/TYPE)))[0]
  if(tag_ind EQ 0) then row=create_struct(name_of_tag,null_val) $
  else row=create_struct(row,name_of_tag,null_val)
  tag_ind=tag_ind+1
endfor
latex_table=replicate(row,num_sources)

;; populate latex_table
for iii=0,num_sources-1 do begin
  for jjj=0,num_keys-1 do begin
    status=tag_exist(fits_table,keys_name[jjj],INDEX=temp_ind)
    latex_table[iii].(jjj)=fits_table[iii].(temp_ind)
    ;Check for invalid underscores in sring names
    if (size(latex_table[iii].(jjj),/type) eq 7) then begin
      if n_elements(strsplit(latex_table[iii].(jjj),'_',/extract)) gt 1 then begin
        latex_table[iii].(jjj)=(strsplit(latex_table[iii].(jjj),'_',/extract))[0]+' '+(strsplit(latex_table[iii].(jjj),'_',/extract))[1]
      endif
    endif  
  endfor 
endfor

;; create tex file

; prepare latex_format_string
latex_format_string=''
reverse_index=''
number_of_columns = 0
for jjj=0,n_tags(latex_table)-1 do begin
  index_temp=where(index_err EQ jjj+1, c_temp)
  if c_temp NE 0 then begin
    ; it is a single error
    latex_format_string=latex_format_string+$
    '$'+keys_format[jjj]+'\\pm'+keys_format[jjj+1]+'$'
    if (jjj NE n_tags(latex_table)-2) then latex_format_string=latex_format_string+' & ' 
    ; skip next jjj
    jjj=jjj+1  
  endif else begin
    index_temp=where(index_err_l EQ jjj+1, c_temp)
    if c_temp NE 0 then begin
      ; it is a lower error, it means
      ; that lower is before upper which opposite
      ; to latex syntax $^{+upper}_{-lower}$ then
      ; save this info in reverse_index
      latex_format_string=latex_format_string+$
      '$'+keys_format[jjj]+'^{+'+keys_format[jjj+2]+'}_{-'+$    
      keys_format[jjj+1]+'}$'
      reverse_index=reverse_index+' '+strtrim(string(jjj+1),2)
      if (jjj NE n_tags(latex_table)-3) then latex_format_string=latex_format_string+' & ' 
      ; skip next jjjs because they are errors and
      ; we accounted for them already
      jjj = jjj+2
    endif else begin
      index_temp=where(index_err_u EQ jjj+1, c_temp)
      if c_temp NE 0 then begin
        ; it is an upper error
        latex_format_string=latex_format_string+$
        '$'+keys_format[jjj]+'^{+'+keys_format[jjj+1]+'}_{-'+$    
        keys_format[jjj+2]+'}$'
        if (jjj NE n_tags(latex_table)-3) then latex_format_string=latex_format_string+' & ' 
        ; skip next jjjs 
        jjj = jjj+2
      endif else if strmatch(keys_format[jjj],'%s*',/fold_case) then begin
        ; there is no error field
        ; but it is a string
        latex_format_string=latex_format_string+$
        keys_format[jjj] 
        if (jjj NE n_tags(latex_table)-1) then latex_format_string=latex_format_string+' & ' 
      endif else begin
        ; there is no error field
        if strmatch(keys_name[jjj],'*lon',/fold_case) OR strmatch(keys_name[jjj],'*lat',/fold_case) $
           AND (strmatch(keys_format[jjj],'%i',/fold_case) ne 1) then begin
        ;The number is either a lon or lat, add /fdg.
          latex_format_string=latex_format_string+ $
          '$%I\\fdg%I$'
        endif else $
          latex_format_string=latex_format_string+ $
          '$'+keys_format[jjj]+'$'
        if (jjj NE n_tags(latex_table)-1) then latex_format_string=latex_format_string+' & ' 
      endelse   
    endelse
  endelse
number_of_columns = number_of_columns + 1
endfor


temp_strarr=strarr(n_tags(latex_table))
temp_string='\begin{deluxetable}{'

;Remove any headers associated with uncertainties 
colhead=colhead[where(strmatch(err_info,'ERR*',/fold_case) eq 0)]
for jjj=0,number_of_columns-1 do begin    
  temp_string=temp_string+'r'
  if colhead[jjj] ne 'BLANK' then colhead[jjj] = '\colhead{'+strtrim(colhead[jjj],2)+'}' $
  else colhead[jjj] = '\colhead{'+strtrim(string(jjj+1),2)+'}' 
endfor
;Remove extra column headers
colhead=colhead[0:number_of_columns-1]

;Initiallize the structures that will store the leading and following portions of coordinate surrounding the decimal point.
tmp1_arr=latex_table
tmp2_arr=latex_table
for jjj=0,n_tags(latex_table)-1 do begin
  if total(strmatch(strsplit(reverse_index,/EXTRACT),strtrim(string(jjj),2))) EQ 1 then begin
  ; have cases of reverse indexes
    temp_strarr(jjj)='latex_table[iii].'+(tag_names(latex_table))[jjj+1]
    temp_strarr(jjj+1)='latex_table[iii].'+(tag_names(latex_table))[jjj]
    jjj=jjj+1
  endif else begin

    if strmatch((tag_names(latex_table))[jjj],'*lon',/fold_case) OR strmatch((tag_names(latex_table))[jjj],'*lat',/fold_case) $
      AND (strmatch(keys_format[jjj],'%i',/fold_case) ne 1) then begin
      tag_loc=where(strcmp(tag_names(latex_table),(tag_names(latex_table))[jjj],/fold_case) eq 1)
      temp_strarr(jjj)=''

      for i=0,n_elements(latex_table.(tag_loc))-1 do begin
        tmp=latex_table[i].(tag_loc)
        tmp=strsplit(string(tmp),'.',/extract)
        tmp1=tmp[0]
        ;This is a string containing of the first decimal position.
        tmp2=strcompress(string(round(float(strmid(tmp[1],0,1)+'.'+strmid(tmp[1],1,1)))),/re)
        tmp1_arr[i].(jjj)=fix(tmp1)
        tmp2_arr[i].(jjj)=fix(tmp2)

      endfor
      temp_strarr(jjj)='tmp1_arr[iii].'+(tag_names(latex_table))[jjj]+','+'tmp2_arr[iii].'+(tag_names(latex_table))[jjj] ;strjoin(tmp1_arr,',')+strjoin(tmp2_arr,',')
      
    end else $
    temp_strarr(jjj)='latex_table[iii].'+(tag_names(latex_table))[jjj]
  endelse  
endfor

  
  openw, 1, tex_filename
  printf, 1, '\documentclass{aastex} \begin{document}'
  printf, 1, temp_string+'}'
  printf, 1, '\tabletypesize{\footnotesize}'
  printf, 1, '\tablecolumns{'+strtrim(string(number_of_columns),2)+'}'
  printf, 1, '\tablecaption{Latex table template}'
  printf, 1, '\tablehead{'
  printf, 1, strjoin(colhead,' & ')
  printf, 1, '} \startdata'

  temp_string='printf, 1, format='+"'(%"+'"'+latex_format_string+'\\\\"'+")',"+ $
  strjoin(temp_strarr,',')

  for iii=0,num_sources-1 do begin
    status=execute(temp_string) 
  endfor

  printf, 1, '\enddata \end{deluxetable} \end{document}' 
  close, 1

end

;; create dvi and ps files
cmd='latex '+tex_filename+' >&! latex_run.log'
spawn, cmd, exit_status=status
if (status NE 0) then begin
  print, 'Error running latex. See latex_run.log file.'
  stop
endif else begin
  file_delete, 'latex_run.log', /quiet
endelse

cmd='dvips '+basename+' -q -o '+ps_filename
spawn, cmd, exit_status=status

print, 'Find results in:'
print, '  '+tex_filename+' - ascii latex table'
if (status EQ 0) then print, '  '+ps_filename+' - postscript image of latex table'
print, 'DONE'
end
