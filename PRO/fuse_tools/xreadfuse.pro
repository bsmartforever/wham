;+
;			xreadfuse
;
; widget to read FUSE data file
;
; CALLING SEQUENCE:
;	xreadfuse,file,header,image,status
;
; INPUTS:
;	none: file is interactively selected
;
; OUTPUTS:
;	file - name of the file selected
;	header - FITS header
;	image - output image
;	status - null string if successful, otherwise error message
;
; OPTIONAL KEYWORD INPUTS:
;	group - id of widget group leader
;
; OPTIONAL KEYWORD INPUTS/OUTPUTS:
;	directory - initial directory to search for file. On output it
;		contains the directory that the file was in.
;
; HISTORY:
;	written July 13, 1999 by D. Lindler
;-
;---------------------------------------------------------------------------
;
; Widget to get X and Y bin size for binning time-tag data
;
;
; event driver
;
pro xreadfuse_bin_event,event

	widget_control,event.top,get_uvalue=a
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'YESNO' then (*a).keep = 1-event.value
	if uvalue eq 'DONE' then begin
		widget_control,(*a).xbase,get_value=n
		if (n eq 1) and (!version.os_family eq 'Windows') then begin
			istat = dialog_message('X Binsize must be at least '+ $
				' 2 for PC/Windows',dialog_parent=event.top);;;
			widget_control,(*a).xbase,set_value=2
			return
		end
		(*a).xbin = n
		widget_control,(*a).ybase,get_value=n
		(*a).ybin = n
		widget_control,event.top,/destroy
	end
	return
end
;
; xbin/ybin widget
;
pro xreadfuse_bin,xbin,ybin,keep,group=group

	base = widget_base(/col,group=group,title='Time-Tag Binning',/modal)
	button = widget_button(base,value='Done',uvalue='DONE')
	xbase = cw_field(base,uvalue='XBASE',xsize=10,value=2,/integer, $
			/return_events,title='X Binsize: ')
	ybase = cw_field(base,uvalue='YBASE',xsize=10,value=2,/integer, $
			/return_events,title='Y Binsize: ')
	lab = widget_label(base,value='Retain Time Tag events in memory?')
	button = cw_bgroup(base,/row,['Yes','No'],uvalue='YESNO',/exclusive, $
				set_value=0)


	a = ptr_new({xbase:xbase,ybase:ybase,xbin:2,ybin:2,keep:1})

	widget_control,base,set_uvalue=a
	widget_control,base,/realize
	xmanager,'xreadfuse_bin',base
	xbin = (*a).xbin
	ybin = (*a).ybin
	keep = (*a).keep
	ptr_free,a
	return
end
;------------------------------------------------------------- XREADFUSE_SORT
;
; Routine to sort observation list by start time
;
pro xreadfuse_sort,list,dtypeall,total_live,total_expo,status

	n = n_elements(list)
	mjd = dblarr(n)
	total_live = 0.0
	total_expo = 0.0
	for i = 0,n-1 do begin
;
; routine to sort observations by start time
;
		fits_open,list(i),fcb,/no_abort,message=status
		if !err lt 0 then goto,error_exit
		fits_read,fcb,data,h,/no_abort,message=status,exten_no=1, $
			/header_only
		err = !err
		fits_close,fcb
;
; determine filetype and check for consistency
;
		if err lt 0 then goto,error_exit
		if strtrim(sxpar(h,'xtension')) eq 'BINTABLE' then begin
		    case strtrim(sxpar(h,'extname')) of
		        'EVENTS': dtype = 'TIME-TAG'
			'Photon Table': dtype = 'PHOTON-TABLE'
			else: begin
				status='INVALID type of data file'
				goto,error_exit
				end
		    endcase
		end else dtype = 'HISTOGRAM'

		if i eq 0 then dtypeall = dtype

		if dtype ne dtypeall then begin
		   	status = 'Unable to mix different file types'
			goto,error_exit
		end
;
; extract times
;
		mjd(i) = double(sxpar(h,'xs-mjdrd')) + sxpar(h,'xs-mjdrf')
		total_expo = total_expo + sxpar(h,'xs-onti')
		total_live = total_live + sxpar(h,'xs-livti')
	end
;
; sort list by start time
;
	list = list(sort(list))
	status = ''
	return
error_exit:
	status = list(i)+': '+status
return
end
;------------------------------------------------------------ xreadfuse_hist
;
; Routine to read histogram file
;
pro xreadfuse_hist,file,data,h,nobin=nobin
	fits_open,file,fcb
	xbin = sxpar(fcb.hmain,'specbinx')>1
	ybin = sxpar(fcb.hmain,'specbiny')>1
	data = fltarr(16384/xbin,1024/ybin)
;
; loop on extensions
;
	for i=1,fcb.nextend do begin
		fits_read,fcb,d,h,exten_no=i
		x = sxpar(h,'xorigin')
		y = sxpar(h,'yorigin')
		data(x,y) = d
	end
	if (not keyword_set(nobin)) and (xbin eq 1) and $
	   (!version.os_family eq 'Windows') then begin
		istat = dialog_message('Histogram Data binned by 2 in X'+ $
				' (Windows IDL limitation)',/info)
		data = rebin(data*2,8192,1024/ybin)
		xbin = 2
	end
	sxaddpar,h,'specbinx',xbin
	sxaddpar,h,'specbiny',ybin
	fits_close,fcb
return
end

;-----------------------------------------------------------------------------
; Top level routine
;
pro xreadfuse,file,h,image,status,group=group,directory=directory
	common fuse_timetag_data,x,y,time,xbin,ybin,gtime1,gtime2,pha
;
; get file name
;
	if n_elements(directory) eq 0 then directory=''
	filter = directory+'*raw.fit'
	if !version.os_family eq 'Windows' then filter = '*raw.fit'
	file = dialog_pickfile(title='Select FUSE data file(s)',/must_exist, $
		filter=filter,/multiple)
	if file(0) eq '' then begin
		status = 'No File Selected'
		return
	endif
;
; sort files
;
	xreadfuse_sort,file,dtypeall,total_live,total_expo,status
	if status ne '' then goto,error_exit
	fdecomp,file(0),disk,directory
;
; get time-tag binning
;
	if (dtypeall eq 'TIME-TAG') or (dtypeall eq 'PHOTON-TABLE') then begin
		xreadfuse_bin,xbin,ybin,keep,group=group
		if (xbin lt 1) or (xbin ge 8192) then begin
			status = 'Invalid XBIN size: must be 0<XBIN<8192'
			goto,error_exit
		endif
		if (ybin lt 1) or (ybin ge 512) then begin
			status = 'Invalid YBIN size: must be 0<YBIN<512'
			goto,error_exit
		endif
		nsout = 16384/xbin
		nlout = 1024/ybin
	    end else begin
	    	xbin = 1
		ybin = 1
	end
;
; loop on files
;
	time = 0
	x = 0
	y = 0
	pha = 0
	widget_control,/hourglass
	nf = n_elements(file)
	for i = 0,nf-1 do begin
;
; read file
;
; time tag data -------------------------------------------------------------
;
		if (dtypeall eq 'TIME-TAG')then begin
		    	fits_read,file(i),data,header,exten_no=1
		    	if i eq 0 then h = header
		    	n = sxpar(header,'naxis2')
			if i eq 0 then begin
				mjd0 = double(sxpar(h,'xs-mjdrd')) +  $
					sxpar(h,'xs-mjdrf')
				mjd = mjd0
				sxaddpar,h,'xs-onti',total_expo
				sxaddpar,h,'xs-livti',total_live
			    end else begin
				mjd = double(sxpar(header,'xs-mjdrd')) +  $
					sxpar(header,'xs-mjdrf')
			end
;
;
			x1 = fix(data(4:5,*),0,n)
			y1 = fix(data(6:7,*),0,n)
			if keep then time1 = float(data(0:3,*),0,n)
			if keep then pha1 = reform(data(8,*),n,/overwrite)
			if is_ieee_big() eq 0 then begin
				ieee_to_host,x1
				ieee_to_host,y1
				if keep then ieee_to_host,time1
			end
			delt = float((mjd-mjd0)*24.0*60.0*60.0)
			if (i eq 0) or (keep eq 0) then begin
				x = temporary(x1)
				y = temporary(y1)
				if keep then time = temporary(time1)
				if keep then pha = temporary(pha1)
			   end else begin
				x = [x,x1]
				y = [y,y1]
				if keep then time = [time,temporary(time1)+delt]
				if keep then pha = [pha,pha1]
			end
;
; create 2-D image
;
		        data = 0
		        if ((i eq 0) and keep eq 0) or  $
			   ((keep eq 1) and (i eq (nf-1))) then begin
				image = hist_2d(x/xbin,y/ybin,min1=0,min2=0, $
						max1 = nsout-1, max2 = nlout-1)
			   end else begin
			        if keep eq 0 then image = image + $
						hist_2d(x/xbin, $
						y/ybin,min1=0,min2=0, $
						max1 = nsout-1, max2 = nlout-1)
			end
			if keep eq 0 then begin
				x = 0
				y = 0
			end
;
; get good time intervals
;
			a = mrdfits(file(i),2)
			if i eq 0 then begin
				gtime1 = a.start
				gtime2 = a.stop
			    end else begin
			    	gtime1 = [gtime1,a.start+delt]
				gtime2 = [gtime2,a.stop+delt]
			end
	        end
;
; Pre-launch time-tag -------------------------------------------------------
;
		if (dtypeall eq 'PHOTON-TABLE') then begin
		    	fits_read,file(i),data,header,exten_no=1
		    	if i eq 0 then h = header
		    	n = sxpar(header,'naxis2')
			if (i eq 0) or (keep eq 0) then begin
				x = fix(data(0:1,*),0,n)
				y = fix(data(2:3,*),0,n)
				if keep then time = dindgen(n)
				ntot = double(n)+100
				if keep then pha = fix(data(4:5,*),0,n)
			   end else begin
				x = [x,fix(data(0:1,*),0,n)]
				y = [y,fix(data(2:3,*),0,n)]
				if keep then $
					time = [time,dindgen(n)+ntot]
				ntot = ntot+n+100
				if keep then $
				     	pha = [pha,fix(data(4:5,*),0,n)]
			end
;
; create 2-D image
;
		        data = 0
		        if ((i eq 0) and keep eq 0) or  $
			   ((keep eq 1) and (i eq (nf-1))) then begin
				image = hist_2d(x/xbin,y/ybin,min1=0,min2=0, $
						max1 = nsout-1, max2 = nlout-1)
			   end else begin
			        if keep eq 0 then image = image + $
						hist_2d(x/xbin, $
						y/ybin,min1=0,min2=0, $
						max1 = nsout-1, max2 = nlout-1)
			end
			if keep eq 0 then begin
				x = 0
				y = 0
			end
			if keep then begin
			    if i eq 0 then begin
				gtime1 = time(0)
				gtime2 = time(n_elements(time)-1)
			      end else begin
				gtime1 = [gtime1,time(n_elements(time)-n)]
				gtime2 = [gtime2,time(n_elements(time)-1)]
			    end
			end
	        end

;
; ACCUM Mode Data -----------------------------------------------------------
;
		if (dtypeall eq 'HISTOGRAM') then begin
			xreadfuse_hist,file(i),data,header
			if i eq 0 then begin
				h = header
				xbin = sxpar(h,'specbinx')
				ybin = sxpar(h,'specbiny')
				image = temporary(data)
			   end else begin
			   	image = image + temporary(data)
			end
		end
	endfor
	status = ''
	sxaddpar,h,'XBIN',xbin
	sxaddpar,h,'YBIN',ybin
	return
;
; error exit
;
error_exit:
	result = dialog_message(status,dialog_parent = group,/error)
	return
end
