function hdr_fits_keyword, file, hdr_keyword, dir=dir, exten=exten

	if (NOT keyword_set(dir)) then dir=''
	if strmatch(file,'*\*',/fold_case) then $
		file=file_search(dir+file)

	hdr_keyword=StrUpCase(hdr_keyword)

	num=n_elements(file)
	if num gt 1 then begin
		hdr_keyword_values=strarr(num)
		for i=0, num-1 do begin
			hdr=headfits(file[i],exten=exten)
			hdr_keyword_values[i]=sxpar(hdr,hdr_keyword)
		endfor
	endif else begin
		hdr=headfits(file,exten=exten)
		hdr_keyword_values=sxpar(hdr,hdr_keyword)
	endelse

	return,hdr_keyword_values

end