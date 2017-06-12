	FUNCTION NUM_EXT, FILE
;+
; NAME:
;	NUM_EXT
; PURPOSE:
;	Return the number of extensions in a FITS file.
;
; CALLING SEQUENCE:
;	extensions=NUM_EXT(FILE)
;
; INPUT PARAMETERS:
;	FILE	= FITS file name, scalar string
;
; RETURNS:
;       Unit number of file or -1 if an error is detected.
;
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	Opens closes the file.
; PROCEDURE:
;	Each FITS header is read in and parsed, and the file pointer is moved
;	to where the next FITS extension header until each
;       extension is reached.
; PROCEDURE CALLS:
;	FXPAR(), MRD_HREAD, MRD_SKIP
; MODIFICATION HISTORY:
;	Originally FXPOSIT in ASTRON library
;	Derived from William Thompson's FXFINDEND routine.
;       Modified by T.McGlynn, 5-October-1994.
;	Modified by T.McGlynn, 25-Feb-1995 to handle compressed
;          files.  Pipes cannot be accessed using FXHREAD so
;          MRD_HREAD was written.
;	W. Landsman 23-Apr-1997    Force the /bin/sh shell when uncompressing 
;	W. Landsman 26-May-1997    Non-unix is not just VMS
;	D. Sahnow 3 November 1998 Changed to NUMEXT to just count the number
;	   of extensions.
;	D. Sahnow 15 November 1998 Renamed to NUM_EXT to avoid conflict with
;	   a variable in FUSEFITS.PRO
;	D. Sahnow 18 November 1998 Added a few print statements for cases
;	   when -1 is returned.
;NOTE: should first check for the NEXTEND keyword, which may have the number
; of extensions directly.
;-
;
	ON_ERROR,2

	ext_no = 500		;maximum number of extensions
;
;  Check the number of parameters.
;
	IF N_PARAMS() ne 1 THEN BEGIN 
	    print,'Syntax:  extensions = NUM_EXT(file)'
	    return,-1
        ENDIF
;
;  Check if this is a compressed file.
;
	unit = -1
	
	len = strlen(file)
	if len gt 3 then tail = strmid(file, len-3, 3) else tail = ' '
	ucmprs = ' '
	if strmid(tail,1,2) eq '.Z' or strmid(tail,1,2) eq '.z' then  $
			   ucmprs = 'uncompress -c '
	if tail eq '.gz'  or tail eq '.GZ' then ucmprs = 'gunzip -c '
		
;  Handle compressed files.
	if ucmprs ne ' ' then begin
		if (!version.os ne 'vms') and (!version.os ne 'windows') and $
                   (!version.os ne 'MacOS') then begin
			spawn, ucmprs+file, unit=unit,/sh
		endif else begin
			print, 'MRDFITS: Only Unix IDL supports piped spawns'
			print, '         File must be uncompressed manually'
			return, -1			
	        endelse
		
        endif else begin
;
;  Go to the start of the file.
;
		openr, unit, file, /get_lun, /block, ERROR = error
		if ERROR NE 0 then begin
			print,!ERR_STRING
			return,-1
		endif
	endelse
	
	for ext = 0, ext_no-1 do begin
	       
;
;  Read the next header, and get the number of bytes taken up by the data.
;
		IF EOF(UNIT) THEN BEGIN
			free_lun,unit
			return, ext-1
		ENDIF

		; Can't use FXHREAD to read from pipe, since it uses
		; POINT_LUN.  So we read this in ourselves using mrd_hread

		mrd_hread, unit, header, status
		
		if status lt 0 then begin
			print,'ERROR in NUM_EXT, extension ',ext,': status < 0'
			return, -1
		endif
			
		; Get parameters that determine size of data
		; region.
		
		BITPIX = FXPAR(HEADER,'BITPIX')
		NAXIS  = FXPAR(HEADER,'NAXIS')
		GCOUNT = FXPAR(HEADER,'GCOUNT') 
		IF GCOUNT EQ 0 THEN GCOUNT = 1
		PCOUNT = FXPAR(HEADER,'PCOUNT')
		
		IF NAXIS GT 0 THEN BEGIN 
			DIMS = FXPAR(HEADER,'NAXIS*')		;Read dimensions
			NDATA = DIMS(0)
			IF NAXIS GT 1 THEN FOR I=2,NAXIS DO NDATA = NDATA*DIMS(I-1)
			
		ENDIF ELSE NDATA = 0
		
		NBYTES = (ABS(BITPIX) / 8) * GCOUNT * (PCOUNT + NDATA)
;
;  Move to the next extension header in the file.
;
		NREC = LONG((NBYTES + 2879) / 2880)
		
		mrd_skip, unit, nrec*2880L

	endfor

;if this point is reached, there are more than ext_no extensions	
	free_lun,unit
	print,'More than ',ext_no,'extensions'
	return, -1
	
	END
