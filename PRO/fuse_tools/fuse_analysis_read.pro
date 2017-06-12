;
; 2006-02-15	wvd	Replaced all calls to findfile with file_search.
;
;----------------------------------------------------XFUSE_FIND_ROOTNAME
;
; Routine to get rootname and aperture to read
;

pro xfuse_find_rootname_event,event
common xfuse_find_rooname_common,dir,root,names,dir_base,list_base, $
	lwrs_button,mdrs_button,hirs_button,label_base,aper
	widget_control,mdrs_button,sens=0
	widget_control,hirs_button,sens=0
	widget_control,lwrs_button,sens=0
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
	    'CANCEL': begin
	    	root = ''
	    	widget_control,event.top,/destroy
		end
	    'DIRECTORY': begin
		widget_control,dir_base,get_value=dir
		dir=dir(0)
		fuse_find_rootnames,dir,names
		widget_control,list_base,set_value=names
		widget_control,label_base,set_value='Select Rootname'
		end
	    'BROWSE': begin
	    	file = dialog_pickfile(title='Select any file in desired '+ $
				'directory',dialog_parent=event.top, $
				/must_exist,filter='*.fit')
		if file(0) ne '' then begin
			fdecomp,file,disk,dir
			dir = disk+dir
			widget_control,dir_base,set_value=dir
			fuse_find_rootnames,dir,names
			widget_control,list_base,set_value=names
			widget_control,label_base,set_value='Select Rootname'
		endif
		end			
	    'TARGETS': begin
	    	widget_control,/hourglass
		widget_control,dir_base,sens=0
		fuse_find_rootnames,dir,names,targets
		widget_control,list_base,set_value=names+'    '+targets
		widget_control,label_base,set_value='Select Rootname'
		widget_control,dir_base,sens=1
		end
	     'ROOTNAMES': begin
	     	root = names(event.index)
		list = file_search(dir+root+'*2ttagfcal.fit',count=n2)
		list = file_search(dir+root+'*2histfcal.fit',count=n)
		n2 = n2+n
		list = file_search(dir+root+'*3ttagfcal.fit',count=n3)
		list = file_search(dir+root+'*3histfcal.fit',count=n)
		n3 = n3+n
		list = file_search(dir+root+'*4ttagfcal.fit',count=n4)
		list = file_search(dir+root+'*4histfcal.fit',count=n)
		n4 = n+n4
		if n2 gt 0 then widget_control,mdrs_button,sens=1
		if n3 gt 0 then widget_control,hirs_button,sens=1
		if n4 gt 0 then widget_control,lwrs_button,sens=1
		if (n2+n3+n4) eq 0 then mess = 'No Calibrated data found' $
				   else mess = 'Select Aperture'
		widget_control,label_base,set_value=mess
		end
	    else : begin
	     	aper = uvalue
		widget_control,event.top,/destroy
		end
	endcase

return
end

pro fuse_find_rootnames,directory,rootnames,targnames
;
; find all FUSE calibrated and raw data files
;
	list  = file_search(directory+'*fcal.fit',count=n)
	if n eq 0 then begin
		rootnames = ''
		targnames = ''
		return
	end
;
; extract file names
;
	names = strarr(n)
	for i=0,n-1 do begin
		fdecomp,list(i),disk,dir,name
		names(i) = name
	end
	names = strmid(names,0,11)	;extract rootnames
	sub = rem_dup(names)		;remove duplicates
	rootnames = names(sub)
;
; get targname names
;
	if n_params(0) gt 2 then begin
		targnames = strarr(n)
		for i = 0,n_elements(sub)-1 do begin
			fits_read,list(sub(i)),d,h,/header_only
			targnames(i) = sxpar(h,'targname')
			if !err lt 0 then targnames(i) = ''
		end
	end

	return
	end
;
; main widget routine -----------------
;
pro xfuse_find_rootname,directory,rootname,aperture,group=group,modal=modal

common xfuse_find_rooname_common,dir,root,names,dir_base,list_base, $
	lwrs_button,mdrs_button,hirs_button,label_base,aper
;
; initialization
;
	dir = directory
	fuse_find_rootnames,dir,names
	root = ''
	aper = ''
;
; widget layout
;
	base = widget_base(group=group,modal=modal,/col)

	basex = widget_base(base,/row,/frame)
	label = widget_label(basex,value='Directory:')
	dir_base = widget_text(basex,uvalue='DIRECTORY',value=dir,/edit, $
				xsize=40)
	button = widget_button(basex,uvalue='BROWSE',value='Browse')

	basex = widget_base(base,/row)
	basec = widget_base(basex,/col)
	button = widget_button(basec,Value='List Target Names',uvalue='TARGETS')
	n = n_elements(names)
	if n lt 9 then names = [names,replicate(' ',9-n)]
	list_base = widget_list(basec,uvalue='ROOTNAMES',value=names, $
			xsize=35,ysize=10)
	basec = widget_base(basex,/col)
	lwrs_button = widget_button(basec,value='LWRS',uvalue='LWRS')
	mdrs_button = widget_button(basec,value='MDRS',uvalue='MDRS')
	hirs_button = widget_button(basec,value='HIRS',uvalue='HIRS')
	cancel = widget_button(basec,value='Cancel',uvalue='CANCEL')
	label_base = widget_label(base,value='Select Directory or Rootname')
	widget_control,base,/realize
	widget_control,lwrs_button,sensitive=0
	widget_control,mdrs_button,sensitive=0
	widget_control,hirs_button,sensitive=0
	xmanager,'xfuse_find_rootname',base
	aperture = aper
	directory = dir
	rootname = root
	return
	end
;-------------------------------------------------------- FUSE_ANALYSIS_RDSPEC
;
; Routine to read calibrated spectrum into a structure
;
pro fuse_analysis_rdspec,dir,root,segment,channel,aperture,spec
;
; search for file
;
	case aperture of
		'MDRS': ap = '2'
		'HIRS': ap = '3'
		'LWRS': ap = '4'
	endcase
	st = dir+root+segment+strlowcase(channel)+ap
	filename = file_search(st+'ttagfcal.fit',count=n)
	if n eq 0 then filename = file_search(st+'histfcal.fit',count=n)
	if n eq 0 then begin
		spec = {filename:'',ns:0,segment:segment,channel:channel, $
			aperture:aperture}
		return
	end
	filename = filename(0)
	a = mrdfits(filename,1,h,/silent)
	junk = mrdfits(filename,0,h,/silent)
	wave = a.wave
	flux = a.flux
	bad = wherenan(flux,nbad)
	if nbad gt 0 then flux(bad) = 0.0
	bad = where((flux gt 1e30) or (flux lt 1e-30),nbad)
	if nbad gt 0 then flux(bad) = 0.0
	ns = n_elements(wave)
	if wave(ns-1) gt wave(0) then begin
		reversed = 0
	    end else begin
		wave = reverse(wave)
		flux = reverse(flux)
		reversed = 1
	end
	fdecomp,filename,disk,directory,name
	mode = strmid(name,13,3)+' '+strmid(name,11,2)
	strput,mode,strupcase(strmid(mode,0,1)),0
	strput,mode,strupcase(strmid(mode,2,1)),2

	spec = {wave:wave, flux:flux, ns:ns, w0:wave(0), w1:wave(ns-1), $
		reversed:reversed,mode:mode,filename:filename,offset:0.0, $
		title:channel+' '+segment,segment:segment, channel:channel, $
		aperture:aperture, header:h,woffset:0.0d0}
	return
	end
;----------------------------------------------------- FUSE_ANALSIS_RDRAW
;  Routine to read raw data file
;
pro fuse_analysis_rdraw,dir,rootname,segment,h,image,file, $
		nxbin=nxbin,nybin=nybin
; Inputs:
;	dir - directory
;	rootname - observation's rootname
;	segment - '1a','1b','2a', or '2b'
; Outputs:
;	h - header
;	image - data array
;	file = file read (null string if none found)
; Optional keyword inputs:
;	nxbin - xbinning for time tag data
;	nybin - ybinning for time tag data
;
	if n_elements(nxbin) eq 0 then nxbin = 1
	if n_elements(nybin) eq 0 then nybin = 1
;
; Try to find time tag file
;
	file = file_search(dir+rootname+segment+'ttagfraw.fit',count=n)
	if n gt 0 then begin
		file = file(0)
		fits_read,file,a,h
		nx = sxpar(h,'naxis2')
		x = fix(a(4:5,*),0,nx)
		ieee_to_host,x
		y = fix(a(6:7,*),0,nx)
		ieee_to_host,y
		a = 0
		nx = 16384L/nxbin
		ny = 1024/nybin
		index = temporary(x)/nxbin + temporary(y)/nybin*nx
		image = 0
		image = histogram(index, min = 0, max = nx*ny-1)
		image = reform(image,nx,ny,/overwrite)
		sxaddpar,h,'xorigin',0
		sxaddpar,h,'yorigin',0
		sxaddpar,h,'specbinx',nxbin
		sxaddpar,h,'specbiny',nybin
		return
	end
;
; Try to find ACCUM mode image
;
	file = file_search(dir+rootname+segment+'histfraw.fit',count=n)
	if n gt 0 then begin
		file=file(0)
		xreadfuse_hist,file,image,h,/nobin
		sxaddpar,h,'yorigin',0
		return
	end
	file = ''
	return
end
;---------------------------------------------------------FUSE_ANALYSIS_EXTIM
; Extract portion of image for the specified aperture
;
pro fuse_analysis_extim,filename,h,image,segment,channel,aperture,options, $
	structure
;
; if filename = null string then return empty structure
;
	if filename eq '' then begin
		structure = {filename:'',segment:segment,channel:channel, $
				aperture:aperture,nx:0,ny:0}
		return
	end
;
; determine if image is to be reversed
;
	reversed = 0
	if (strupcase(channel) eq 'LIF') and $
	   (strmid(segment,0,1) eq 2) then reversed = 1
	if (strupcase(channel) eq 'SIC') and $
	   (strmid(segment,0,1) eq 1) then reversed = 1
;
; determine region of image to exract
;
;
;		  	LWRS  				Offset	Width
	SiC1a = [ 	140+ options.offsets(0),  	120,    250	  ]
	Lif1a = [	600+ options.offsets(1),	120,    250	  ]
	SiC1b = [	 90+ options.offsets(2),	120,    250	  ]
	LIf1b = [	560+ options.offsets(3),  	120,    250	  ]
	SiC2a = [	460+ options.offsets(4),  	-75,    250	  ]
	Lif2a = [	740+ options.offsets(5),  	-75,    250	  ]
	Sic2b = [	535+ options.offsets(6),  	-60,    250	  ]
	Lif2b = [	750+ options.offsets(7),  	-60,    250	  ]

	index  = where(['LWRS','HIRS','MDRS'] eq aperture)
	index = index(0)
;
; select right array
;
	istat = execute('region = '+channel+segment)
	line0 = (region(0) + region(1)*index - region(2)/2)>0<1023
	line1 = (line0 + region(2))>0<1023
;
; convert to image coordinates
;
	s = size(image)
	nx = s(1)
	ny = n_elements(image)/nx
	y0 = sxpar(h,'yorigin')
	biny = sxpar(h,'specbiny')
	line0 = ((line0 - y0)/biny)>0
	line1 = ((line1 - y0 + biny - 1)/biny)<(ny-1)

	if line1 le line0 then begin		;region not found
		structure = {filename:'',segment:segment,channel:channel, $
				aperture:aperture,nx:0,ny:0}
		return
	endif
	data = image(*,line0:line1)
	ny = line1-line0+1
	if reversed then data = reverse(data)
	dmin = min(data,max=dmax)
	structure = {image:data, reversed:reversed, nx:nx, ny:ny, $
		binx:sxpar(h,'specbinx'),biny:biny, $
		x0:sxpar(h,'xorigin'), y0:line0*biny + y0, $
		datamin:dmin,datamax:dmax,fmin:dmin,fmax:dmax, $
		filename:filename,aperture:aperture, channel:channel, $
		segment:segment, header:h, scaled:bytarr(nx,ny), $
		nsmooth:1,s1:0,s2:0,i1:0,i2:0,offset:0}
	return
	end
;------------------------------------------------------FUSE_ANALYSIS_READ
;
; Routine to read FUSE data
;
pro fuse_analysis_read,istat,group=group

@fuse_analysis_common

;
; get root name
;
	istat = 1
	dir = widget.directory
	xfuse_find_rootname,dir,rootname,aperture,group=group,/modal
	if rootname eq '' then return
	widget.directory = dir		;save for next call
;
; zero out old observation
;
	sic1a = {nx:0,filename:''}
	sic1b = {nx:0,filename:''}
	sic2a = {nx:0,filename:''}
	sic2b = {nx:0,filename:''}
	lif1a = {nx:0,filename:''}
	lif1b = {nx:0,filename:''}
	lif2a = {nx:0,filename:''}
	lif2b = {nx:0,filename:''}
	spec_sic1a = {ns:0,filename:'',woffset:0.0}
	spec_sic1b = {ns:0,filename:'',woffset:0.0}
	spec_sic2a = {ns:0,filename:'',woffset:0.0}
	spec_sic2b = {ns:0,filename:'',woffset:0.0}
	spec_lif1a = {ns:0,filename:'',woffset:0.0}
	spec_lif1b = {ns:0,filename:'',woffset:0.0}
	spec_lif2a = {ns:0,filename:'',woffset:0.0}
	spec_lif2b = {ns:0,filename:'',woffset:0.0}
	coadd = {ns:0,title:'',woffset:0.0}
	sp1 = {ns:0,title:'',woffset:0.0}
	sp2 = {ns:0,title:'',woffset:0.0}
	sp3 = {ns:0,title:'',woffset:0.0}
	sp4 = {ns:0,title:'',woffset:0.0}
	zoom = {imname:'',xim:0,yim:0}
;
; read image files
;
	widget_control,/hourglass
	fuse_analysis_rdraw,dir,rootname,'1a',h,image,file, $
		nxbin=options.nxbin,nybin=options.nybin
	fuse_analysis_extim,file,h,image,'1a','LiF',aperture,options,lif1a
	fuse_analysis_extim,file,h,image,'1a','SiC',aperture,options,sic1a

	fuse_analysis_rdraw,dir,rootname,'1b',h,image,file, $
		nxbin=options.nxbin,nybin=options.nybin
	fuse_analysis_extim,file,h,image,'1b','LiF',aperture,options,lif1b
	fuse_analysis_extim,file,h,image,'1b','SiC',aperture,options,sic1b

	fuse_analysis_rdraw,dir,rootname,'2a',h,image,file, $
		nxbin=options.nxbin,nybin=options.nybin
	fuse_analysis_extim,file,h,image,'2a','LiF',aperture,options,lif2a
	fuse_analysis_extim,file,h,image,'2a','SiC',aperture,options,sic2a

	fuse_analysis_rdraw,dir,rootname,'2b',h,image,file, $
		nxbin=options.nxbin,nybin=options.nybin
	fuse_analysis_extim,file,h,image,'2b','LiF',aperture,options,lif2b
	fuse_analysis_extim,file,h,image,'2b','SiC',aperture,options,sic2b
;
; Read Spectra
;
	fuse_analysis_rdspec,dir,rootname,'1a','SiC',aperture,spec_sic1a
	fuse_analysis_rdspec,dir,rootname,'1b','SiC',aperture,spec_sic1b
	fuse_analysis_rdspec,dir,rootname,'2a','SiC',aperture,spec_sic2a
	fuse_analysis_rdspec,dir,rootname,'2b','SiC',aperture,spec_sic2b
	fuse_analysis_rdspec,dir,rootname,'1a','LiF',aperture,spec_lif1a
	fuse_analysis_rdspec,dir,rootname,'1b','LiF',aperture,spec_lif1b
	fuse_analysis_rdspec,dir,rootname,'2a','LiF',aperture,spec_lif2a
	fuse_analysis_rdspec,dir,rootname,'2b','LiF',aperture,spec_lif2b
	widget.rootname=rootname
	widget_control,widget.file_label,set_value=rootname+'  '+aperture
	widget_control,/hourglass
	istat=0
	return
end

