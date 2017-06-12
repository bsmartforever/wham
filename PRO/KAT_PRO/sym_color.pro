function sym_color, data_array=data_array, mindata=mindata, maxdata=maxdata, $
	numcolor=numcolor

;
; Purpose:
;	To create a symbol color index array that varies linearly with data values. 
;
; Input:
;	minsize - miniumum symbol size of symsize [Default 1]
;	maxsize - maximum symbol size of symsize [Default 3]
;
;	Must pass either data_array OR mindata, maxdata, and numcolor
;
;	data_array - data array 
;	
;	mindata - miniumum value of data array [Default min(data_array) if data_array is passed]
;	maxdata - maxiumum value of data array [Default max(data_array) if data_array is passed]
;	numcolor - number of elements of the symbol_color array 
;				[Default n_elements(data_array) of data_array is passed]
;
; Output:
;	symbol_color - an array containing the sizes to be input into symsize when plotting. This 
;			 		sizes linearly vary with the value of the data.  
;
; Example:
;
;	
;	data_array=[1,2,6,7,8,3,2,5,4]
;	sym_color=sym_color(data_array=data_array)
;	;This creates a color index array with indexes varying between 0 and 255
;
;	@kat_color
;	num=n_elements(data_array)
;	plot,[0,num+2],[min(data_array)-1,max(data_array)+1],/nodata,$
;		xstyle=1,ystyle=1,xminor=1,xticks=num+2,$
;		xtitle='Index Number',ytitle='Data Value',$
;		charsize=1.5,charthick=2
;		
;	cgloadct,19,/brewer,/reverse
;	for i=0, num-1 do plots,i+1,data_array[i],color=sym_color[i],psym=symcat(16),symsize=3
; 
; Created by Dr. Kat Barger 11/2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


   tvlct, r_orig, g_orig, b_orig, /get

	if keyword_set(data_array) then data_arr=float(data_array)
	if (NOT keyword_set(mindata)) then mindata=min(data_arr)
	if (NOT keyword_set(maxdata)) then maxdata=max(data_arr)
	if (NOT keyword_set(numcolor)) then numcolor=n_elements(data_arr)


	minsize=0. & maxsize=254. 

	symbol_color=fltarr(numcolor)
	slope=float(maxsize-minsize)/(maxdata-mindata)
	intercept=minsize-slope*mindata

	symbol_color=slope*data_arr+intercept

   tvlct, r_orig, g_orig, b_orig

   symbol_color=fix(symbol_color)
   bad_colors=where(symbol_color gt maxsize,num)
   if num ne 0 then symbol_color[bad_colors]=maxsize
   bad_colors=where(symbol_color lt minsize,num)
   if num ne 0 then symbol_color[bad_colors]=minsize

	return, fix(symbol_color)

end