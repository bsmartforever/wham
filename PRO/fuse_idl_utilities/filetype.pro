function filetype, infile, silent=silent
;+
; NAME:
;	FILETYPE
; PURPOSE:
;	Returns the type of file from the DATA_FMT keyword (UCB data),
;	the INSTMODE or FILETYPE keyword (OPUS data), or the INSTRUME keyword
;	(FES data). Used to determine what program should be used to
;	read the file.
;
; CALLING SEQUENCE:
;	Result = FILETYPE(Filename)
;
; INPUT:
;	Filename:  Scalar or vector string giving the name of the FITS file.
;
; KEYWORD PARAMETERS:
;	SILENT:	If included, suppress informative messages.
;
; OUTPUTS:
;	Returns the file type, which can be any one of the following:
;		TTAG		FUSE OPUS time tag
;		HIST		FUSE OPUS spectral image mode
;		PIPELINE	FUSE Science pipeline image
;		FES		FUSE FES file
;		TTGD		FUSE TTGD file from pipeline (before v3.0)
;		IDF		FUSE IDF file from pipeline (v3.0 and later)
;		COUNT		FUSE accumulated count map file (v3.0) 
;		GAIN		FUSE accumulated gain map file (v3.0) 
;		TCNT
;		PHAMEAN
;		XYPT_LIST
;		FLAT		CalFUSE pipeline calibration file
;		BKG		CalFUSE pipeline calibration file
;		GEOM		CalFUSE pipeline calibration file
;		ONED		Calibrated one-dimensional data		
;		XYP_LIST	UCB GSE photon list
;		TXYP_LIST	UCB GSE photon list with time
;		COUNT_IMAGE	UCB GSE histogram
;		GAIN_IMAGE	UCB GSE gain histogram
;		COS_NUV_TT	COS NUV time tag
;		COS_NUV_ACCUM	COS NUV ACCUM
;		COS_FUV_TT	COS FUV time tag
;		COS_FUV_ACCUM	COS FUV ACCUM
;		UNDEFINED	If file type cannot be identified
;		FILE_NOT_FOUND	If file was not found		
;
; EXAMPLE:
; 	Display the file type of the FITS files 'temp.fit' in the current
;		directory
;
;		IDL> result = filetype('temp.fit')
;
; PROCEDURES USED:
;
; MODIFICATION HISTORY:
;	3 November 1998 Rewritten from scratch by David Sahnow. Previously,
;	 a modified version of FITS_INFO was used.
;	6 November 1998 Modified to handle files that have been processed by
;	 the pipeline (IMAGE_COR keyword is COMPLETE).
;	15 November 1998 Changed call to NUMEXT to NUM_EXT.
;	14 December 1998 Added calls to FREE_LUN to deallocated lun1. Fixed
;	 typo in OPUS/INITIAL section.
;	8 April 1999 Modified to handle pipeline calibration files (CALFTYPE
;	 type).
;	11 June 1999 Modified to handle FES files (FES type).
;	29 September 1999 Modified to allow the IMAG_COR keyword to be
;	 'PERFORM' (partially processed throught the pipeline)
;	30 May 2000 Added SILENT keyword.
;	19 June 2000 Modified to allow TTGD files to return 'PIPELINE.'
;	6 October 2000 Added 'ONED' type for calibrated 1D data.
;	4 November 2000 Added call to EXPAND_TILDE for input filename.
;	4 April 2003 Added 'IDF' type for Intermediate Data File.
;	15 July 2003 Modified to read COS data files.
;	12 January 2004 Added 'COUNT' and 'GAIN' file types.
;	24 September 2004 Modified to handle OPUS v1.7 files. Added RANGE
;	 keyword to MRDFITS so that only one row of data is read, which
;	 speeds things up.
;       2006-02-15     wvd     Replaced all calls to findfile with file_search.
;-

	filename = expand_tilde(infile)

	if (n_elements(silent) eq 0) then silent = 0
	ftype = 'UNDEFINED'	;default filetype

	fil = file_search(filename, COUNT = nfiles)
	if nfiles EQ 0 then return,'FILE_NOT_FOUND' ;message,'No files found'

	file = fil(0)
	openr, lun1, file, /GET_LUN, /BLOCK

START:  
;	ON_IOerror, BAD_FILE

;figure out how many extensions there are:
	extension = num_ext(file)
	if (extension eq 1) then begin
		if (silent eq 0) then print,extension,' extension in FITS file'
	endif else begin
		if (silent eq 0) then print,extension,' extensions in FITS file'
	endelse

;Now search the extensions until DATA_FMT or INSTMODE keywords are found:
	hd = ''
	for i=0,extension do begin
		result = mrdfits(file,i,hdr,/silent,range=[0])
		hd = [hd,hdr]
	endfor

	temp = string(fxpar(hd,'DATA_FMT'))
	;force result to be a string, even if it returns zero
	; (meaning not found)
	temp = strcompress(temp,/remove)
	;previous line added by  DS 9/3/97 to save last non-null DATA_FMT,
	; not last DATA_FMT

	if (temp ne '0') then ftype = temp
		;save the file type if this extension or primary
		; header contains the file type

	;for files that have been processed by OPUS:
	if (temp eq '0') then begin
		temp1 = string(fxpar(hd,'FILETYPE'))		;Calibrated 1D
		if (temp1 eq 'CALIBRATED EXTRACTED SPECTRUM') then begin
			ftype = 'ONED'
		endif else begin
			temp3 = string(fxpar(hd,'IMAG_COR'))	;OPUS + pipeline
			temp3 = strcompress(temp3,/remove)
			if (temp3 eq 'COMPLETE') then begin
				ftype = 'PIPELINE'
			endif
			if (temp3 eq 'INITIAL' or temp3 eq 'PERFORM' or $
				temp1 eq 'RAW PHOTON ADDRESS') then begin
								;OPUS
;RAW PHOTON ADDRESS line new 9/24
				temp2 = string(fxpar(hd,'INSTMODE'))
				temp2 = strcompress(temp2,/remove)
				if (temp2 ne '0') then ftype = temp2
			endif
		endelse
		if (temp1 eq 'INTERMEDIATE DATA FILE') then begin
			ftype = 'IDF'
		endif
	endif
	;end of OPUS files

	;for FUSE calibration files:
	if (ftype eq 'UNDEFINED') then begin
		temp1 = string(fxpar(hd,'CALFTYPE'))	;pipeline cal. file
		temp1 = strcompress(temp1,/remove)
			;valid image types are 'BKGD','FLAT','GEOM'
		if (temp1 ne '0') then ftype = temp1
	endif

	;for FUSE TTGD and other intermediate data files:
	if (ftype eq 'UNDEFINED') then begin
		temp1 = string(fxpar(hd,'FILETYPE'))	;pipeline cal. file
		temp1 = strcompress(temp1,/remove)
		if (temp1 eq 'RAWHISTOGRAM') then ftype = 'PIPELINE'		
	endif

	;for FUSE FES files:
	if (ftype eq 'UNDEFINED') then begin
		temp1 = string(fxpar(hd,'FILETYPE'))	;pipeline cal. file
		temp1 = strcompress(temp1,/remove)
		if (temp1 eq 'RAWFES') then ftype = 'FES'		
	endif

	;for COS files:
	if (ftype eq 'UNDEFINED') then begin
		if (strcompress(string(fxpar(hd,'INSTRUME')),/remove) $
			eq 'COS') then begin
			detector = strcompress(string(fxpar(hd,'DETECTOR')),$
				/remove)
			obsmode = strcompress(string(fxpar(hd,'OBSMODE')),$
				/remove)
			if (obsmode eq 'TIME-TAG') then obsmode = 'TT'
			ftype = 'COS_'+detector+'_'+obsmode 
		endif
	endif

	free_lun,lun1
	return, ftype

BAD_FILE:
	free_lun,lun1
	message, 'Error reading FITS file ' + file, /CON
	return, 'Error reading FITS file '

THEEND:

end
