pro fs_fits_write,file_or_fcb,data,header_in,extname=extname,extver=extver, $
		xtension=xtension, extlevel=extlevel, NaNvalue=NaNvalue, $
		no_abort=no_abort, message = message, header = header, $
		no_data = no_data
;+
;
;*NAME:
;	FS_FITS_WRITE
;
;*PURPOSE:
;	To write a FITS primary data unit or extension for CALFUSE which
;	is very picky about the order of the keywords
;
;*CATEGORY:
;	INPUT/OUTPUT
;
;*CALLING SEQUENCE:
;	FITS_WRITE, filename_or_fcb, data, [header_in]
;
;*INPUTS:
;	FILENAME_OR_FCB: name of the output data file or the FITS control
;		block returned by FITS_OPEN (called with the /WRITE or
;		/APPEND) parameters.
;
;*OPTIONAL INPUTS:
;	DATA: data array to write.  If not supplied or set to a scalar, a
;		null image is written.
;	HEADER_IN: FITS header keyword.  If not supplied, a minimal basic
;		header will be created.  Required FITS keywords, SIMPLE,
;		BITPIX, XTENSION, NAXIS, ... are added by FITS_WRITE and
;		do not need to be supplied with the header.  If supplied,
;		thier values will be updated as necessary to reflect DATA.
;
;*KEYWORD PARAMETERS:
;
;	XTENSION: type of extension to write (Default="IMAGE"). If not
;		supplied, it will be taken from HEADER_IN.  If not in either
;		place, the default is "IMAGE".  This parameter is ignored
;		when writing the primary data unit.
;	EXTNAME: EXTNAME for the extension.  If not supplied, it will be taken
;		from HEADER_IN.  If not supplied and not in HEADER_IN, no
;		EXTNAME will be written into the output extension.
;	EXTVER: EXTVER for the extension.  If not supplied, it will be taken
;               from HEADER_IN.  If not supplied and not in HEADER_IN, no
;               EXTVER will be written into the output extension.
;	EXTLEVEL: EXTLEVEL for the extension.  If not supplied, it will be taken
;               from HEADER_IN.  If not supplied and not in HEADER_IN, no
;               EXTLEVEL will be written into the output extension.
;	NaNvalue: data value in DATA to be replaced with IEEE NaN in the output
;		file.
;       /NO_ABORT: Set to return to calling program instead of a RETALL
;               when an I/O error is encountered.  If set, the routine will
;               return with !err=-1 and a message in the keyword MESSAGE.
;               If not set, FITS_READ will print the message and issue a RETALL
;       MESSAGE: value of the error message for use with /NO_ABORT
;	HEADER: actual output header written to the FITS file.
;	/NO_DATA: Set if you only want FITS_WRITE to write a header.  The
;		header supplied will be written without modification and
;		the user is expected to write the data using WRITEU to unit
;		FCB.UNIT. When FITS_WRITE is called with /NO_DATA, the user is
;		responsible for the validity of the header, and must write
;		the correct amount and format of the data.  When FITS_WRITE
;		is used in this fashion, it will pad the data from a previously
;		written extension to 2880 blocks before writting the header.
;
;*NOTES:
;	If the first call to FITS_WRITE is an extension, FITS_WRITE will
;	automatically write a null image as the primary data unit.
;
;	Keywords and history in the input header will be properly separated
;	into the primary data unit and extension portions when constructing
;	the output header (See FITS_READ for information on the internal
;	Header format which separates the extension and PDU header portions).
;	
;*EXAMPLES:
;	Write an IDL variable to a FITS file with the minimal required header.
;		FITS_WRITE,'newfile.fits',ARRAY
;
;	Write the same array as an image extension, with a null Primary data
;	unit.
;		FITS_WRITE,'newfile.fits',ARRAY,xtension='IMAGE'
;
;	Write 4 image extensions to the same file.
;		FITS_OPEN,'newfile.fits',fcb
;		FITS_WRITE,fcb,data1,extname='FLUX',extver=1
;		FITS_WRITE,fcb,err1,extname'ERR',extver=1
;		FITS_WRITE,fcb,data2,extname='FLUX',extver=2
;		FITS_WRITE,fcb,err2,extname='ERR',extver=2
;		FITS_CLOSE,FCB
;		
;*PROCEDURES USED:
;	FITS_OPEN, SXADDPAR, SXDELPAR, SXPAR()
;*HISTORY:
;	Written by:	D. Lindler	August, 1995
;	Work for variable length extensions  W. Landsman   August 1997
;	Converted to IDL V5.0   W. Landsman   September 1997
;	Lindler, Nov. 1999, changed position of TFIELDS keyword in output
;		header
;-
;-----------------------------------------------------------------------------
;
; print calling sequence if no parameters supplied
;
	if n_params(0) lt 1 then begin
	    print,'CALLING SEQUENCE: fs_fits_write,file_or_fcb,data,header_in'
	    print,'KEYWORD PARAMETERS: extname, extver, xtension, extlevel'
	    print,'                    NaNvalue, no_abort, message, header'
	    print,'                    no_data
	    return
	end
;
; Open file if file name is supplied instead of a FCB
;
        s = size(file_or_fcb) & fcbtype = s[s[0]+1]
	fcbsize = n_elements(file_or_fcb)
        if (fcbsize ne 1) or ((fcbtype ne 7) and (fcbtype ne 8)) then begin
                message = 'Invalid Filename or FCB supplied'
                goto,error_exit
        end

        if fcbtype eq 7 then begin
		if keyword_set(no_data) then begin
			print,'FITS_WRITE: Must have FCB supplied for NO_DATA'
			retall
		endif
                fits_open,file_or_fcb,fcb,/write, $
					no_abort=no_abort,message=message
		if !err lt 0 then goto,error_exit
           end else fcb = file_or_fcb
;
; if user did not pad data to 2880 blocks, pad it now
;
	point_lun,-fcb.unit,current_position
	npad = 2880 - (current_position mod 2880)
	if npad eq 2880 then npad = 0
	if npad gt 0 then writeu,fcb.unit,bytarr(npad)
;
; if no_data, just go and write user header as supplied
;
	if keyword_set(no_data) then begin
		header = header_in
		goto,write_header
	end
;
; if header not supplied then set it to a null header
;
	if n_elements(header_in) le 1 then begin
		header = strarr(1)
		header[0] = 'END     '
	end else header = header_in

;
; on I/O error go to statement IOERROR
;
;	on_ioerror,ioerror
;
; verify file is open for writing
;
	if fcb.open_for_write eq 0 then begin
		message,'File is not open for writing'
		goto,error_exit
	endif
;
; determine bitpix and axis information
;
	s = size(data)
	naxis = s[0]
	if naxis gt 0 then axis = s[1:naxis]
	idltype = s[naxis+1]

	if (idltype gt 5) then begin
		message='Data array is an invalid type'
		goto,error_exit
	endif
	bitpixs = [8,8,16,32,-32,-64]
	bitpix = bitpixs[idltype]
;
; determine extname, extver, xtension and extlevel and delete current values
;
	if n_elements(xtension) gt 0 then begin
		Axtension = xtension
	   end else begin
		Axtension = sxpar(header,'xtension')
		if !err lt 0 then Axtension = ''
	end

	if n_elements(extname) gt 0 then begin
		Aextname = extname
	   end else begin
		Aextname = sxpar(header,'extname')
		if !err lt 0 then Aextname = ''
	end

	if n_elements(extver) gt 0 then $
		Aextver = extver $
		else Aextver = sxpar(header,'extver')

	if n_elements(extlevel) gt 0 then $
		Aextlevel = extlevel $
		else Aextlevel = sxpar(header,'extlevel')

	sxdelpar,header,['XTENSION','EXTNAME','EXTVER','EXTLEVEL']

;
; separate header into main and extension header
;
	keywords = strmid(header,0,8)
	hpos1 = where(keywords eq 'BEGIN MA') & hpos1 = hpos1[0] ;begin main
	hpos2 = where(keywords eq 'BEGIN EX') & hpos2 = hpos2[0] ;begin ext.
	hpos3 = where(keywords eq 'END     ') & hpos3 = hpos3[0] ;end of header	

	if (hpos1 gt 0) and (hpos2 lt hpos1) then begin
		message,'Invalid header BEGIN EXTENSION HEADER ... out of place'
		goto,error_exit
	endif

	if (hpos3 lt 0) then begin
		print,'FITS_WRITE: END missing from input header and was added'
		header = [header,'END     ']
		hpos2 = n_elements(header)-1
	end
;
; determine if a extention was supplied and no primary data unit (PDU)
; was written
;
	if (fcb.nextend eq -1) then begin		;no pdu written yet?
	    if (hpos2 gt 0) or (Axtension ne '') or (Aextname ne '') or $
	       (Aextver ne 0) or (Aextlevel ne 0) then begin
;
; write null image PDU
;
			if (hpos1 gt 0) and (hpos2 gt (hpos1+1)) then $
				hmain = [header[hpos1+1:hpos2-1],'END     ']
			fits_write,fcb,0,hmain,/no_abort,message=message
			if !err lt 0 then goto,error_exit
	    end
	end
;
; For extensions, do not use PDU portion of the header
;
	if (hpos2 gt 0) then header = header[hpos2+1:hpos3]
;
; create required keywords for the header
;
	h = strarr(20)
	h[0] = 'END     '

	if fcb.nextend eq -1 then begin
		sxaddpar,h,'SIMPLE','T','image conforms to FITS standard' 
	   end else begin
		if Axtension eq '' then Axtension = 'IMAGE   '
		sxaddpar,h,'XTENSION',Axtension,'extension type'
	end

	sxaddpar,h,'bitpix',bitpix,'bits per data value'
	sxaddpar,h,'naxis',naxis,'number of axes'
	if naxis gt 0 then for i=1,naxis do $
		sxaddpar,h,'naxis'+strtrim(i,2),axis[i-1]
	sxaddpar,h,'pcount',0,'size of special data area'
	sxaddpar,h,'gcount',1,'one data group (required keyword)'

	if fcb.nextend eq -1 then begin
		sxaddpar,h,'EXTEND','T','file may contain extensions'
	   end else begin
	   	tfields = sxpar(header,'tfields')
		if !err ge 0 then sxaddpar,h,'TFIELDS',tfields, $
				'number of fields in each row'
		if Aextname ne '' then sxaddpar,h,'extname',Aextname
		if Aextver gt 0 then sxaddpar,h,'extver',Aextver
		if Aextlevel gt 0 then sxaddpar,h,'extlevel',Aextlevel
	end
;
; delete special keywords from user supplied header
;
	pcount = sxpar(header,'pcount')
        groups = sxpar(header,'groups')
        sxdelpar,header,['SIMPLE','BITPIX','NAXIS','NAXIS1','NAXIS2','NAXIS3', $
			'NAXIS4','NAXIS5','NAXIS6','NAXIS7','NAXIS8','EXTEND', $
                        'PCOUNT','GCOUNT','GROUPS','TFIELDS']
        if groups then if (pcount gt 0) then for i=1,pcount do $
                        sxdelpar,header,['ptype','pscal','pzero']+strtrim(i,2)
;
; combine the two headers
;
	last = where(strmid(h,0,8) eq 'END     ')
	header = [h[0:last[0]-1],header]
;
; convert header to bytes and write
;
write_header:
	last = where(strmid(header,0,8) eq 'END     ')
	n = last[0] + 1
	byte_header = replicate(32b,80,n)
	for i=0,n-1 do byte_header[0,i] = byte(header[i])
	writeu,fcb.unit,byte_header
;
; pad header to 2880 byte records
;
	npad = 2880 - (80L*n mod 2880)	
	if npad eq 2880 then npad = 0
	if (npad gt 0) then writeu,fcb.unit,replicate(32b,npad)
	nbytes_header =  npad + n*80
	if keyword_set(no_data) then return
;
; process data
;
	if naxis gt 0 then begin
;
; find NaNvalues in real*4 and real*8 data
;
	    if (n_elements(NaNvalue) gt 0) and (bitpix lt 0) then $
		NaN_points = where(data eq NaNvalue, n_NaN) else n_NaN = 0
;
; convert to IEEE
;
	    newdata = data
	    host_to_ieee, newdata
;
; insert IEEE NaN values
;
  	    if n_NaN gt 0 then begin
	      if idltype eq 4 then $
		data[NaN_points] = float([127b,255b,255b,255b],0,1) else $
		data[NaN_points] = double([127b,replicate(255b,7)],0,1)
	    end
;
; write the data
;
	    nbytes = n_elements(data) * abs(bitpix)/8
	    npad = 2880 - (nbytes mod 2880)
	    if npad eq 2880 then npad = 0
	    writeu,fcb.unit,newdata
	    if npad gt 0 then writeu,fcb.unit,replicate(32b,npad)
	    nbytes_data = nbytes + npad
	  end else begin
	    nbytes_data = 0
	end
;
; done, update file control block
;
	fcb.nextend = fcb.nextend + 1
	if fcbtype eq 7 then fits_close,fcb else file_or_fcb = fcb
	!err = 1
	return
;
; error exit
;
ioerror:
	message = !err_string
error_exit:
	if fcbtype eq 7 then free_lun,fcb.unit
	!err = -1
	if keyword_set(no_abort) then return
	print,'FITS_WRITE ERROR: '+message
	retall
end
