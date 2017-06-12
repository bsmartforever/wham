function sym_size, minsize, maxsize, data_array=data_array, mindata=mindata, maxdata=maxdata, numsize=numsize

;
; Purpose:
;	To create a symbol size array that varies linearly with data values. 
;
; Input:
;	minsize - miniumum symbol size of symsize [Default 1]
;	maxsize - maximum symbol size of symsize [Default 3]
;
;	Must pass either data_array OR mindata, maxdata, and numsize
;
;	data_array - data array 
;	
;	mindata - miniumum value of data array [Default min(data_array) if data_array is passed]
;	maxdata - maxiumum value of data array [Default max(data_array) if data_array is passed]
;	numsize - number of elements of the symbol_size array 
;				[Default n_elements(data_array) of data_array is passed]
;
; Output:
;	symbol_size - an array containing the sizes to be input into symsize when plotting. This 
;			 		sizes linearly vary with the value of the data.  
;
; Example:
;	data_array=[1,2,6,7,8,3,2,5,4]
;	sym_size=sym_size(data_array=data_array)
;	;This creates a size array with sizes varying between symsize = 1. and 3.
;
;	num=n_elements(data_array)
;	plot,[0,num+2],[min(data_array)-1,max(data_array)+1],/nodata,$
;		xstyle=1,ystyle=1,xminor=1,xticks=num+2,$
;		xtitle='Index Number',ytitle='Data Value',$
;		charsize=1.5,charthick=2
;		
;	for i=0, num-1 do plots,i+1,data_array[i],symsize=sym_size[i],psym=symcat(46)
; 
; Created by Dr. Kat Barger 11/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if keyword_set(data_array) then data_arr=float(data_array)
	if n_elements(numsize) eq 0 then numsize=n_elements(data_arr)
	if (NOT keyword_set(minsize)) then minsize=1. 
	if (NOT keyword_set(maxsize)) then maxsize=3. 
	if (NOT keyword_set(mindata)) then mindata=min(data_array) 
	if (NOT keyword_set(maxdata)) then maxdata=max(data_array) 

	symbol_size=fltarr(numsize)
	slope=float(maxsize-minsize)/(maxdata-mindata)
	intercept=minsize-slope*mindata

	symbol_size=slope*data_arr+intercept

	return, symbol_size

end