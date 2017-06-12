function table2str,filename,dir=dir,skip=skip,tags=tags,comment=comment, nrows=nrows
;
; Purpose: To convert an ascii table to a structure.
;
; Input:
;	filename - File name containing the table. Must include file extension, e.g., *.dat.
;	dir 	 - Directory containing the table file.
;   skip     - Number of lines to skip to the reach table data. 
; 	tags 	 - Structure tag names for data in columns. 
;			   Default: Column names located on row skip-1
; Output:
; 	structure containg data in ascii table file. Column data format is assumed 
;	to be either a string or a float; the format is automatically tested 
;   through the row skip
;
; Example:
; 	filename='ha_table.txt'
; 	ha=table2str(filename)
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

if (NOT keyword_set(skip)) then skip=1

;extract the number of data rows in the file  
if (NOT keyword_set(nrows)) then num_row=file_lines(filename)-skip else $
	num_row=nrows

;extract the tag names and data types
tmp=' '
file_tags=' '
openr,lun,filename,/get_lun
for i=0,skip-1 do READF, lun, file_tags
READF, lun, tmp
close,lun
free_lun,lun
tmp=strsplit(tmp,/extract)

;tag names
if (NOT keyword_set(tags)) then tags=strsplit(file_tags,/extract) else tags=strsplit(tags,/extract)
num_col=n_elements(tags)

;data types, assuming either string or float
types=intarr(num_col)
for i=0,num_col-1 do if valid_num(tmp[i]) eq 0 then types[i]=7 else types[i]=4 

;re-run read_ascii through the mg_read_ascii wrapper. This wrapper essentially creates the template structure 
;used by the read_ascii file using the tag names and tag data format. This bipasses the need to use the 
;ascii_template function, which opens a GUI for the user to specify the information passed through this wrapper. 
;Using the wrapper allows the user to bypass this GUI so they can automate this procedure.   
table_str=$
	mg_read_ascii(filename,column_names=tags,column_types=types,data_start=skip,missing_value=0,groups=lindgen(num_col),num_records=num_row,comment_symbol=comment)

return,table_str

end