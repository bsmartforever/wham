;+
;			FUSE_REGISTER
;
; Interative routine for registering FUSE spectra (driver routine for
; xregister_1d.pro
;
; CALLING SEQUENCE:
;	fuse_register,wave,flux,error,woffset
;
; OUTPUTS:
;	wave - wavelength vector for the coadded spectrum
;	flux - flux vector for the coadded spectrum
;	error - error vector for the coadded spectrum
;	woffset - vector of computed wavelength offsets for each of the
;		selected input spectra
;
; INTERACTIVE WIDGET CONTROLS
;
; Select Files(s) - standard file selection widget for selecting single
;		or multiple files.  Input files must be FITS binary tables
;		with columns WAVE and FLUX and optional columns ERROR and
;		QUALITY.  If the ERROR column is not present all columns
;		are set to 1.0.  If a QUALITY column is present, 0 indicates
;		bad data and nonzero values indicates good data.
;
; Remove From List - used to remove a file from the current
;		list of selected files.  To remove a file, click on its
;		file name to highlight it.  Then press <Remove From List>
;
; Remove All - remove all the files currently on the list
; Process - load the data into the spectral registration and coaddition
;		widget (xregister_1d).  See xregister_1d for description of
;		its control panel.
; Exit - exit the program
;
; HISTORY:
;	version 1, D. Lindler, April 2001
;-
;========================================================== FUSE_REGISTER_EVENT
;
; Event driver for FUSE_REGISTER
;
pro fuse_register_event,event

	widget_control,event.id,get_uvalue=uvalue
	widget_control,event.top,get_uvalue=info
	

	case uvalue of
;
; Exit
;
	'EXIT': begin
		widget_control,event.top,/destroy
		return
		end
;
; File selection
;
	'FILES': begin
		filter = info.directory + '*.fit'
		files = dialog_pickfile(title ='Select FITS binary Table(s)', $
	 			filter=filter,/multiple,/must_exist, $
				dialog_parent=event.top)
		if files(0) eq '' then return
		fdecomp,files(0),disk,dir
		info.directory = dir
		n = n_elements(files)
		nfiles = info.nfiles
		if (nfiles + n) gt 10 then begin
			istat = dialog_message('Maximum of 10 files allowed', $
					dialog_parent=event.top,/error)
			if nfiles eq 10 then return
			files = files(0:9-nfiles)
			n = 10-nfiles
		end

		if nfiles eq 0 then info.filelist = files $
			       else info.filelist = $
			       		[info.filelist(0:nfiles-1),files]
		info.nfiles = nfiles + n
		end
;
; remove all files from the list
;
	'REMOVEALL': begin
		info.nfiles = 0
		end
;
; remove selected file from the list
;
	'REMOVE': begin
		iselect = widget_info(info.file_base,/list_select)
		print,iselect
		if iselect lt 0 then begin
			istat = dialog_message('You must select a file first', $
					dialog_parent=event.top,/error)
			return
		end
		select = replicate(1,info.nfiles)
		select(iselect) = 0
		keep = where(select,nkeep)
		if nkeep gt 0 then info.filelist = info.filelist(keep)
		info.nfiles = nkeep
		end
;
; process the current list of files
;
	'PROCESS': begin
		nfiles = info.nfiles
		if nfiles eq 0 then begin
		    istat = dialog_message('You must select file(s) first', $
					dialog_parent=event.top,/error)
		    return
		end
		nprocess = 0
		filelist = info.filelist
		for i=0,nfiles-1 do begin
		    a = mrdfits(filelist(i),0,h,/silent)
		    weight = sxpar(h,'exptime')
		    a = mrdfits(filelist(i),1,h,/silent)
		    if weight le 0 then weight  = sxpar(h,'totalw')
		    if weight le 0 then weight = 1.0

		    if datatype(a) ne 'STC' then begin
		    	mess = filelist(i) +' is not a valid FITS binary table'
		    	istat = dialog_message(mess,dialog_parent=event.top, $
					/error)
			goto,nexti
		    end
		
		    cols = tag_names(a)
		    good = where(cols eq 'WAVE',ngood)
		    if ngood eq 0 then begin
		    	mess = 'Table Column WAVE not found in '+filelist(i)
		    	istat = dialog_message(mess,dialog_parent=event.top, $
					/error)
			goto,nexti
		    end
		    wave = a.wave
		    
		    good = where(cols eq 'FLUX',ngood)
		    if ngood eq 0 then begin
		    	mess = 'Table Column FLUX not found in '+filelist(i)
		    	istat = dialog_message(mess,dialog_parent=event.top, $
					/error)
			goto,nexti
		    end
		    flux = a.flux
		    
		    good = where(cols eq 'ERROR',ngood)
		    if ngood eq 0 then error = replicate(1.0,n_elements(wave)) $
		    	   	  else error = a.error
		    good = where(cols eq 'QUALITY',ngood)
		    if ngood eq 0 then mask = replicate(1,n_elements(wave)) $
		    	   	  else mask = a.quality gt 0

		    nprocess = nprocess + 1
		    fdecomp,info.filelist(i),disk,dir,name
		    xregister_1d,wave,flux,error,mask, $
		    	title=name,xtitle='Wavelength',ytitle='Flux', $
			group = event.top, init = nprocess eq 1, $
			weight = weight
nexti:
		end
		xregister_1d,/process,woffset=woffset,wout=wout,fout=fout, $
				errout=errout
		*info.pflux = fout
		*info.perror = errout
		*info.poffset = woffset

		end
	else: return
	endcase
;
; update any changes to the file list
;	
	if info.nfiles eq 0 then widget_control, info.file_base, set_value='' $
			    else widget_control, info.file_base, set_value = $
						info.filelist(0:info.nfiles-1)
	widget_control,event.top,set_uvalue=info

return
end
;============================================================ FUSE_REGISTER
;
; Main Routine
;
pro fuse_register,wave,flux,error,woffset


	tlb = widget_base(/row)
;
; define menu bar
;
	menubar = widget_base(tlb,/col)
	button = widget_button(menubar,value='Select File(s)',uvalue='FILES')
	button = widget_button(menubar,value='Remove From List',uvalue='REMOVE')
	button = widget_button(menubar,value='Remove All',uvalue='REMOVEALL')
	button = widget_button(menubar,value='Process',uvalue='PROCESS')
	button = widget_button(menubar,value='Exit',Uvalue='EXIT')
;
; file list
;
	file_base = widget_list(tlb,uvalue='LIST',xsize=60,ysize=10)
;
; create structure to save results
;	
	pwave = ptr_new(0)
	pflux = ptr_new(0)
	perror = ptr_new(0)
	poffset = ptr_new(0)

	info = {file_base:file_base,directory:'',filelist:strarr(10),nfiles:0, $
		pwave:pwave,pflux:pflux,perror:perror,poffset:poffset}
;
; create widget and execute
;
	widget_control,tlb,/realize
	widget_control,tlb,set_uvalue=info,/no_copy
	xmanager,'fuse_register',tlb
;
; extract results and clean up
;
	wave = *pwave
	flux = *pflux
	error = *perror
	woffset = *poffset

	ptr_free,pwave
	ptr_free,pflux
	ptr_free,perror
	ptr_free,poffset

	return
end
