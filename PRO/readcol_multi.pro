PRO readcol_multi,files,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,$
	v17,v18,v19,v20,v21,v22,v23,v24,v25,v26,v27,v28,v29,v30,v31,v32,v33,v34,$
	v35,v36,v37,v38,v39,v40,$;v41,v42,v43,v44,v45,v46,v47,v48,v49,v50, $
	_extra = extra

;+
;NAME: readcol_multi
;PURPOSE: wrapper for READCOL that reads multiple files in succession, 
;	outputting the results into a single set of arrays
;CALLING SEQUENCE: readcol_multi, files, v1, v2, ..., v40 [, readcol options]
;INPUT: files: array of filenames
;OUTPUTS: v1...v40
;MODIFICATION HISTORY:
;	2008 Sept 24: Written by Alex S. Hill
;   2009 Oct 31: Use astrolib READCOL, which now supports 40 vectors, instead
;       of READCOL50
;-

	ncol = N_params() - 1           ;Number of columns of data expected
	vv = 'v' + strtrim( indgen(ncol)+1, 2)
	vvtemp = 'vvtemp' + strtrim( indgen(ncol)+1, 2)
	namestr = ''
	FOR i=0, ncol - 1 DO BEGIN
		namestr += ','  + vvtemp[i]
	ENDFOR
	
	res=execute('readcol,"' + files[0] + '"' + namestr + ',_extra=extra')
	FOR j=0, ncol - 1 DO res=execute(vv[j] + '=' + vvtemp[j])
	
	IF n_elements(files) GT 1 THEN FOR i=1, n_elements(files) - 1 DO BEGIN
		res=execute('readcol,"' + files[i] + '"' + namestr + ',_extra=extra')
		FOR j=0, ncol - 1 DO res=execute(vv[j] + '=[' + vv[j] + ',' + $
			vvtemp[j] + ']')
	ENDFOR

END
