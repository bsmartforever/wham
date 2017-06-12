function table2str,filename,dir=dir,skip=skip,tags=tags
;
; Purpose: To convert an ascii table to a structure.
;
; Input:
;	filename - File name containing the table. Must include file extension, e.g., *.dat.
;	dir 	 - Directory containing the table file.
;   skip     - Number of lines to skip to the reach table data. 
; 	tags 	 - Structure tag names for data in columns. 
;			   Default: Column names located on row skip-1
;   null	 - Null character    
;
; Output:
; 	structure containg data in ascii table file. Column data format is assumed 
;	to be either a string or a float; the format is automatically tested 
;   through the row skip
;
; Example:
; 	filename='ha_table.txt'
; 	ha=table2str(filename)
;
;   tmp=table2str('target_list.txt',tags=['id','qso','loc','glon','glat','mlon','mlat']
;
; Dependencies: 
;	validate_directory
;	valid_num
;	mg_read_ascii
;
; Created by Dr. Kat Barger 10/2013
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (NOT keyword_set(dir)) then dir='./' $
else dir=validate_directory(dir)
if size(filename,/type) ne 7 then begin
	print,''
	print,'*** Pass file name ***'
	print,''
	return,0
endif 
;Check to make sure file exists
test=file_test(dir+filename)
if test eq 0 then begin
	print,''
	print,'*** File does not exist ***'
	print,'Passed file: ',filename
	print,'Passed dir: ',dir
	print,''
	return,0
endif

if (NOT keyword_set(skip)) AND (NOT keyword_set(tags)) then skip=1 $
	else if (NOT keyword_set(skip)) AND (keyword_set(tags)) then skip=0 

;extract the number of data rows in the file and the tag names. 
table = read_ascii(dir+filename,data_start=skip,missing_value=0,count=num_row,header=file_tags)
if (NOT keyword_set(tags)) then tags=strsplit(file_tags,/extract) else if size(tags,/type) ne 7 then tags=strsplit(tags,/extract)
num_col=n_elements(tags)

tmp=' '
openr,lun,dir+filename,/get_lun
for i=0,skip do READF, lun, tmp
close,lun
free_lun,lun
tmp=strsplit(tmp,/extract)

types=intarr(num_col)
if n_elements(tmp) eq num_col then begin
	for i=0,num_col-1 do if valid_num(tmp[i]) eq 0 then types[i]=7 else types[i]=4 
endif else types=types+7

;re-run read_ascii through the mg_read_ascii wrapper. This wrapper essentially creates the template structure 
;used by the read_ascii file using the tag names and tag data format. This bipasses the need to use the 
;ascii_template function, which opens a GUI for the user to specify the information passed through this wrapper. 
;Using the wrapper allows the user to bypass this GUI so they can automate this procedure.   
table_str=mg_read_ascii($
	dir+filename,column_names=tags,column_types=types,$
	data_start=skip,missing_value=0,groups=lindgen(num_col))

table_str=struct_conv(table_str)

return,table_str

end