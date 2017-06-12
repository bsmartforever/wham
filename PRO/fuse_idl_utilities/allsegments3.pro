pro allsegments3, name, slitin, binsize, fluxmin=fluxmin, fluxmax=fluxmax,$
	wavemin=wavemin, wavemax=wavemax, log=log, hist=hist, catalog=catalog,$
	brightness=brightness, suffix=suffix, channel=channel, $
	overplot=overplot,$
	fluxoffset=fluxoffset, fluxrange=fluxrange, waverange=waverange,$
	colors=colors, waveoffset=waveoffset,$
	waveout=wave, fluxout=flux, qualityout=quality, errorout=error,$
	chanout=chanval, noqual=noqual, nofheader=nofheader, notitle=notitle,$
	ratio=ratio, counts=counts, countrate=countrate, silent=silent, _extra=e
;+
; NAME:
;	ALLSEGMENTS3
;
; PURPOSE:
;	This procedure plots the flux vs. wavelength for all four channels,
;	using the calibrated data files from CalFUSE v3.0 and later. For
;	earlier versions, use ALLSEGMENTS instead.
;
; CATEGORY:
;	Analysis.
;
; CALLING SEQUENCE:
;	ALLSEGMENTS3, Rootname, Slit, Binsize
;
; INPUTS:
;	Rootname:
;		Rootname of file, e.g. I9040104017. Include the path if
;		the file is not in the current directory. If an array
;		containing multiple filenames is used, the individual
;		files will be averaged before display, unless the OVERPLOT
;		keyword is used. The program currently uses an unweighted
;		average.
;	Slit:	Name of slit: 'LWRS', 'MDRS', 'HIRS' or 'PINH'. This can be
;		either a scalar string, or a vector, with a different slit
;		for each input file.
;	Binsize:Size of wavelength bin, in angstroms. Must be an integer
;		multiple of the bin size in the calibrated data file.
;
; KEYWORD PARAMETERS:
;	COUNTS:	If present, plot counts rather than flux.
;	COUNTRATE:
;		If present, plot counts per second rather than flux.
;	FLUXRANGE:
;		A two element vector which sets the minimum and maximum
;		of the flux scale. Default is [0,max in any channel]. See
;		also the FLUXMIN and FLUXMAX keywords.
;	FLUXMIN:If included, use this as the minimum of the flux scale.
;		The default is to use -0.1*FLUXMAX. Also see FLUXRANGE.
;	FLUXMAX:If included, use this as the maximum of the flux scale.
;		The default is to use the maximum value in any channel.
;		Also see FLUXRANGE.
;	WAVERANGE:
;		A two element vector which sets the minimum and maximum
;		of the wavelength scale. Default is [900,1200]. See also
;		the WAVEMIN and WAVEMAX keywords.
;	WAVEMIN:If included, use as the minimum value of the wavelength
;		scale. The default is 900 A. See also WAVERANGE.
;	WAVEMAX:If included, use as the maximum value of the wavelength
;		scale. The default is 1200 A. See also WAVERANGE.
;	LOG:	If set, use a log scale for the flux.
;	HIST:	If set, assumes the filename specifies a HIST file rather
;		than a TTAG.
;	CATALOG:Filename of a catalog to overplot. Should be in a form which
;		can be read by READCATALOG.
;	BRIGHTNESS:
;		If included, convert the flux to a brightness in Rayleighs
;		per Angstrom before plotting. This conversion assumes a
;		filled slit. FLUXMIN and FLUXMAX will also be in these units.
;	SUFFIX:	A suffix which is added to the filenames, e.g. if the
;		filenames are of the form I20514010001alif2ttagfcal-limb2.fit,
;		the suffix is '-limb2'
;	CHANNEL:Set to 'SiC1', 'LiF1', etc. to only plot one channel rather
;		than all four.
;	OVERPLOT:
;		If set and File is an array containing multiple filenames,
;		plot each file separately on the same plot rather than
;		averaging.
;	RATIO:	If set along with OVERPLOT, plot the ratio of each spectrum
;		to the first spectrum in the File list, rather than the absolute
;		flux
;	FLUXOFFSET:
;		Used in conjunction with the OVERPLOT keyword. If included,
;		offset individual spectra by this amount when plotting.
;	WAVEOFFSET:
;		A vector with one element per input spectrum, containing the
;		shift in Angstroms to be applied before plotting. Works only
;		when the OVERPLOT keyword is used.
;	COLORS:	An vector containing the color value to be used for plotting
;		each spectrum when the OVERPLOT keyword is used.
;	WAVEOUT:This variable returns an array containing the wavelengths for
;		each input exposure.
;	FLUXOUT:This variable returns an array containing the flux for
;		each input exposure.
;	QUALITYOUT:
;		The returned quality flags.
;	ERROROUT:
;		The returned error values.
;	CHANOUT:A variable which returns a text string containing the exposure
;		channel, and slit.
;	NOQUAL:	If present, don't overplot the scaled quality flag value.
;	NOFHEADER:
;		If present, don't display the file header on the plot.
;	NOTITLE:If present, don't display the filename on the title line.
;	SILENT:	If present, don't display messsages when MRDFITS reads a file.
;	Also accepts all keywords that PLOT accepts, e.g. THICK, LINESTYLE.
;
; OUTPUTS:
;
; SIDE EFFECTS:
;	Creates a plot showing flux vs. wavelength for all four channels.
;
; PROCEDURE:
;	Straighforward.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	Started with ALLSEGMENTS.PRO 11 February 2004.
;	11 February 2004 Changed Binby to Binsize and modified to work with
;	 version 3.x.
;	17 March 2004 Renamed POTHOLE column to QUALITY to reflect a recent
;	 change in v3.x. Now properly calculates the error array when binning.
;	26 March 2004 Added more graceful exit if no valid file found. Changed
;	 the way the check is done to see if there is a ttag file.
;	9 April 2004 Changed tile to show binsize rather than binby. Added
;	 SILENT keyword.
;	15 July 2004 Modified so that input file can contain either a
;	 structure of arrays or an array of structures. This is done primarily
;	 to deal with the fact that wfusecal3.pro writes the data incorrectly.
;	28 October 2004 Changed to allow bin size to be off by 1e-4 to account
;	 for rounding issues. Now exits if neither TTAG nor HIST files are
;	 found. Now checks to make sure CalFUSE version is >= 3.0. No longer
;	 crashes if all quality flags are zero.
;       2006-02-15     wvd     Replaced all calls to findfile with file_search.
;-

	chan = ['SiC1','SiC1','SiC2','SiC2','LiF1','LiF1','LiF2','LiF2']
	seg = ['A','B','A','B','A','B','A','B']
;Thse numbers should be checked:
	segmin = [1003.9,905.0,916.8,1016.3,987.1,1094.3,1085.6,978.1]
	segmax = [1091.1,992.6,1006.4,1105.0,1082.2,1187.7,1181.7,1074.6]

	nums = n_elements(chan)		;number of separate spectra per exposure
	nume = n_elements(name)	;number of separate exposures

	binsize = float(binsize)	;force binsize to be a float
	origbins = 100000		;maximum number of wavelength bins in
					; data file
	maxq = 0.95			;height of scaled quality flag

;convert slitin to the aperture number:
	nslit = n_elements(slitin)
	if (nslit eq 1) then begin
		slitin = replicate(slitin,nume)
		nslit = nume
	endif
	if (nslit ne nume) then begin
		print,'Slit must be either a single value or a vector which is'
		print,' the same size as the File vector.'
		stop
	endif
	slitin = strcompress(strupcase(slitin),/rem)	;convert to uppercase
	slitname = ['NULL','PINH','MDRS','HIRS','LWRS']	;slit order
	solidang = [0,0,1.88e-9,5.88e-10,2.12e-8]	;slit solid angle (sr)
	slit = intarr(nume)
	for k=0,nslit-1 do begin
		for i=1,4 do begin
			if (slitin[k] eq slitname[i]) then slit[k] = i
		endfor
	endfor
	strslit = strcompress(slit,/rem)

	fname1 = [['1a','1b','2a','2b']+'sic',['1a','1b','2a','2b']+'lif']
	if not keyword_set(suffix) then suffix = ''
	fname = strarr(nume,nums)
	if keyword_set(hist) then begin
		for k=0,nume-1 do $
			fname[k,*] = $
				fname1+strslit[k]+'histfcal'+suffix+'.fit'
	endif else begin
		for k=0,nume-1 do $
			fname[k,*] = $
				fname1+strslit[k]+'ttagfcal'+suffix+'.fit'
	endelse

;Set minimum and maximum of each segment to be a multiple of Binsize:
	for j=0,n_elements(seg)-1 do begin
		segmin[j] = floor(segmin[j]/binsize) * binsize
		segmax[j] = ceil(segmax[j]/binsize) * binsize
	endfor

	if keyword_set(channel) then begin	;all channels?
		goodch = where(strupcase(channel) eq strupcase(chan))
		chan = chan(goodch)
		seg = seg(goodch)
		segmin = segmin(goodch)
		segmax = segmax(goodch)
		fname = fname(*,goodch)
print,goodch
	endif

;xxxx
	binby = 1		;this value gets changed later
	newbins = long(origbins / binby)
	wave = fltarr(nume+1,nums,newbins)
	flux = fltarr(nume+1,nums,newbins)
	quality = fltarr(nume+1,nums,newbins)
	error = fltarr(nume+1,nums,newbins)
	chanval = strarr(nume+1,nums)
	;Format for these arrays is [no_exposures,segment/channel_no,wave_bins]
	;Space is also reserved for the total (averaged) spectra (at the end).
;xxxx

	tottime = 0.
	;Do a test to see if the file exists:
	data = mrdfits((name[0]+fname[0,0]),1,status=status,/silent)
	if (status ne 0) then begin		;error opening file
		print,'Could not find ',name[0]+fname[0,0]
		print,'Switching to histogram mode'
			;maybe it's a histogram
		sumstatus = 0
		for k=0,nume-1 do begin
			fname[k,*] = $
				fname1+strslit[k]+'histfcal'+suffix+'.fit'
			for kk=0,3 do begin
				data = $
					mrdfits((name[k]+fname[k,kk]),1,$
					status=status,/silent)
					sumstatus = sumstatus + abs(status)
			endfor
		endfor
		if (sumstatus eq nume*4) then begin	;if no HIST file exists
			print,'No valid HIST files found. Exiting'
			goto, endit
		endif
	endif

	;Read the files:
	good = intarr(nums)
	dbmax = -1
	for j=0,nume-1 do begin				;for all exposures
		infile = name[j] + fname[j,*]
		nums2 = n_elements(fname[0,*])
		for i=0,nums2-1 do begin		;for all segments
			filecheck = file_search(infile[i])
			if (filecheck[0] eq '') then begin
				print,'File ',infile[i],' not found'
				wave[j,i,*] = 0
				flux[j,i,*] = 0
				quality[j,i,*] = 0
				error[j,i,*] = 0
			endif else begin
				good[i] = 1
				dummy = mrdfits(infile[i],0,hdr0,silent=silent)
				data = mrdfits(infile[i],1,hdr,/fscale,$
					silent=silent)
				cf_vers = float(fxpar(hdr0,'CF_VERS'))	;CalFUSE version
				if (cf_vers lt 3.0) then begin
					print
					print,'This program only works with CalFUSE versions'
					print,' >= 3.0. Use ALLSEGMENTS.PRO instead.'
					print
					goto, endit
				endif

				;Rename the data variables to allow a structure
				;of arrays or an array of structures.
				datawave = data.wave
				dataflux = data.flux
				dataerror = data.error
				datacounts = data.counts
				dataweights = data.weights
				databkgd = data.bkgd
				dataquality = data.quality

				datasize = n_elements(datawave)
				wpc = fxpar(hdr0,'WPC')	;wavelength per channel
				w0 = fxpar(hdr0,'W0')	;wavelength at pix 0
				
				binby = float(binsize) / float(wpc)
;				if (binby ne round(binby)) then begin
				if (abs(binby-round(binby)) gt 1e-4) then begin
					print,'Binsize must be a multiple of ',$
						wpc
					goto, endit
				endif else begin
					binby = round(binby)
					print,'Binning data by ',binby
				endelse
;xxx
;				datarange = where(data.wave ge segmin[i] and $
;					data.wave le segmax[i])
;				newb = ((segmax[i]-segmin[i])/binsize) + 1

				datasize = $
;					 long(fix(float(datasize)/binby)*binby)
					 long(floor(fix(float(datasize)/binby))*binby)
					;adjust datasize to be an integer
					; multiple of binby
				db = datasize/binby
				if (db gt dbmax) then dbmax = db
				wave[j,i,0:db-1] = $
					rebin(datawave[0:datasize-1],db)
				flux[j,i,0:db-1] = $
					rebin(dataflux[0:datasize-1],db)
				if keyword_set(counts) then $
					flux[j,i,0:db-1] = $
					 rebin(datacounts[0:datasize-1],db) $
						* float(binby)
				if keyword_set(countrate) then $
					flux[j,i,0:db-1] = $
					rebin(datacounts[0:datasize-1],db) $
						* float(binby)
				quality[j,i,0:db-1] = $
					rebin(dataquality[0:datasize-1],db)
				for k1=0,db-1 do begin	;properly combine errors
					for k2=0,binby-1 do begin
						error[j,i,k1] = error[j,i,k1] + $
							(dataerror[k1*binby+k2])^2.0D
					endfor
					error[j,i,k1] = sqrt(error[j,i,k1]) / binby
				endfor
;stop
;				error[j,i,0:db-1] = $
;					rebin(data.error[0:datasize-1],db)
;xxx
;				print,'Note that errors may not be properly combined when rebinning!'
			endelse
		endfor
		good1 = where(good eq 1)
		if (good1[0] eq -1) then begin
			print,'No valid files found. Exiting.'
			goto, endit
		endif
		temp = mrdfits(infile[good1[0]],0,header)
		tottime = tottime + fxpar(header,'EXPTIME')
		if keyword_set(countrate) then flux = flux / tottime
	endfor
;trim the arrays:
	wave = wave[*,*,0:dbmax-1]
	flux = flux[*,*,0:dbmax-1]
	quality = quality[*,*,0:dbmax-1]
	error = error[*,*,0:dbmax-1]
	temp = mrdfits(infile[0],0,header)
					;read the main header of the first file
;Convert to brightness, if desired:
	yt = 'Flux (erg/cm^2/sec/A)'
	if keyword_set(brightness) then begin
		print,'Converting to brightness units'
		solid = (solidang[slit])[0]
		flux = 632.58 * flux * wave / solid	;R/A
		yt = 'Brightness (R/A)'
	endif
	if keyword_set(counts) then yt = 'Total Counts'
	if keyword_set(countrate) then yt = 'Counts per Second per Binned Pixel'
	if keyword_set(ratio) then yt = 'Flux Ratio'

;Now calculate the averages:
	quality[nume,*,*] = 1.0
	for j=0,nume-1 do begin
		wave[nume,*,*] = wave[nume,*,*] + wave[j,*,*]
		flux[nume,*,*] = flux[nume,*,*] + flux[j,*,*]
		quality[nume,*,*] = quality[nume,*,*] * quality[j,*,*]
			;this ensures that zeroes are propagated
		error[nume,*,*] = error[nume,*,*] + error[j,*,*]^2
	endfor

	wave[nume,*,*] = wave[nume,*,*] / nume
	flux[nume,*,*] = flux[nume,*,*] / nume	
;SHOULD this be a weighted average?
	error[nume,*,*] = sqrt(error[nume,*,*]) / nume

;set plotting ranges:
	if keyword_set(ratio) then begin
		fluxmin = 0.5
		fluxmax = 1.5
	endif
	if (keyword_set(fluxrange)) then begin
		fluxmin = fluxrange[0]
		fluxmax = fluxrange[1]
	endif

	if (keyword_set(fluxmax)) then begin
	endif else begin
		fluxmax = max(flux)*1.1
	endelse
	print,'Using a maximum flux of ',fluxmax
	if (keyword_set(fluxmin)) then begin
	endif else begin
		if keyword_set(log) then fluxmin = fluxmax * 0.01 $
;			else fluxmin = 0.
			else fluxmin = -0.1*fluxmax
	endelse
	print,'Using a minimum flux of ',fluxmin

	if (keyword_set(wavemin)) then begin
	endif else begin
		wavemin = 900
	endelse

	if (keyword_set(wavemax)) then begin
	endif else begin
		wavemax = 1200
	endelse

	if (keyword_set(waverange)) then begin
		wavemin = waverange[0]
		wavemax = waverange[1]
	endif

;Read the catalog:
	if not keyword_set(catalog) then catalog = 'nocatalog'
	if (catalog ne 'nocatalog') then begin
		readcatalog,catalog,linestruct
		wline = linestruct.wavelength
		text1 = strcompress(wline,/remove)
		text2 = strcompress(linestruct.transition,/rem)
		inten = float(linestruct.intensity)
	;extract only relevant wavelengths:
		temp = where((wline ge wavemin) and (wline le wavemax))
		if (n_elements(temp) ne 1) then begin
			wline = wline[temp]
			text1 = text1[temp]
			text2 = text2[temp]
			inten = inten[temp]
		endif
	endif
	catlines = n_elements(wline)

	if not keyword_set(channel) then !p.multi = [0,1,4]
	xr = [wavemin,wavemax]				;wavelength range
	yr = [fluxmin,fluxmax]				;flux range

	qscale = quality
	yr0 = yr[0] > 0.
	if (min(qscale) eq 0. and max(qscale) eq 0.) then begin
		print,'All quality flags are zero!'
		qscale2 = qscale
	endif else begin
		qheight = yr0 + 1.0*(yr[1]-yr0)
		qscale(where(qscale ne 0)) = qheight	;scaled quality flag
		qscale(where(qscale eq 0)) = yr0
		qscale[*,*,0] = qheight
		qscale[*,*,dbmax-1] = qheight
		qscale2 = quality/max(quality[0:1,*,*]) * qheight * maxq
	endelse
	for i=0,nums2-1 do begin
		spectid = chan[i]+' - '+slitname[slit]
		segid = seg[i]
		for j=0,nume-1 do begin
			chanval[j,i] = name[j] + ' - ' + chan[i]+seg[i] + ' - ' + $
				slitname[slit[j]]
					;added [j] to slit, 2/14/01
		endfor
		chanval[nume,i] = 'Average' + ' - ' + chan[i]+seg[i] + $
			' - ' + slitname[slit[0]]
		if (2*(i/2) eq i) then begin
			mtitle = name[0] +  fname[0,i]+' & '+fname[0,i+1]
			if (nume gt 1) then mtitle = mtitle + ' (& ' + $
				strcompress(nume-1,/remove)+' other exposures) '
			if keyword_set(notitle) then mtitle = ''
			if (binby ne 1) then mtitle = mtitle + ' binned to ' + $
				strcompress(binsize,/rem) + ' A'
			if keyword_set(log) then begin	;log plot
				plot_io,[1],[1],/nodata,xr=xr,yr=yr,$
					ytitle=yt,xtitle='Wavelength (A)',$
					title=mtitle,/xs,/ys,_extra=e
			endif else begin
				plot,[1],[1],/nodata,xr=xr,yr=yr,$
					ytitle=yt,xtitle='Wavelength (A)',$
					title=mtitle,/xs,/ys,_extra=e
			endelse
			ycpos = (!y.crange[1]-!y.crange[0])*0.87 + !y.crange[0]
			xcpos = (!x.crange[1]-!x.crange[0])*0.87 + !x.crange[0]
			if keyword_set(log) then ycpos = 10^ycpos
			xyouts,xcpos,ycpos,spectid		
		endif

		if keyword_set(overplot) then begin
			;plot each input file separately
			if not keyword_set(fluxoffset) then fluxoffset = 0.0
			if not keyword_set(colors) then begin
				colors = fltarr(nume) + !color
			endif else begin
				;Should check to make sure array is big enough
			endelse
			if keyword_set(waveoffset) then begin
				;Should make sure array is the right size
			endif else begin
				waveoffset = fltarr(nume)
			endelse
			for k=0,nume-1 do begin
				xval = wave[k,i,*]+waveoffset[k]
				xval = (xval<wavemax)>wavemin
				if keyword_set(ratio) then begin
					oplot,xval,$
						flux[k,i,*]/flux[0,i,*],$
							psym=10,color=colors[k],_extra=e
				endif else begin
					oplot,xval,$
						flux[k,i,*]+fluxoffset*float(k),$
							psym=10,color=colors[k],_extra=e
				endelse
					;plot the data
				if not keyword_set(noqual) then $
					oplot,xval,$
					qscale2[k,i,*],psym=3,_extra=e
					;overplot the (scaled) quality flag
			endfor
		endif else begin
			xval = (wave[nume,i,*]<wavemax)>wavemin
			oplot,xval,flux[nume,i,*],psym=10,_extra=e
				;plot the data
			oplot,xval,error[nume,i,*],psym=10,linestyle=1,$
				_extra=e
				;overplot the error
			if not keyword_set(noqual) then $
				oplot,xval,qscale2[nume,i,*],psym=3,_extra=e
				;overplot the (scaled) quality flag
		endelse

;Overplot a line at flux = 0:
		if (!y.crange[0] le 0) then begin
			oplot,!x.crange,[0.,0.],psym=-3,linestyle=2
		endif

;Overplot the catalog:
		idtop = (!y.crange[1]-!y.crange[0])*0.88 + !y.crange[0]
					;ID line location
		idlen = (!y.crange[1]-!y.crange[0])*0.5;0.15
					;maximum length of ID line
		idlab = idtop + 0.05
					;ID label Y location
		for j=0,catlines-1 do begin	;overplot the line IDs
			idscale = inten[j]/max(inten)	;scale intensity
;NOTE that intensity should be scaled separately for different species
			idbot = idtop - idlen*idscale
;idbot = !y.crange[0]
;idtop = !y.crange[1]
			if keyword_set(log) then begin
				idtop2 = 10^idtop
				idbot2 = 10^idbot
				idlab2 = 10^idlab
			endif else begin
				idtop2 = idtop
				idbot2 = idbot
				idlab2 = idlab
			endelse
			oplot,[wline(j),wline(j)],[idtop2,idbot2],psym=-3
			xyouts,wline(j),idlab2,text2(j),alignment=0.5
		endfor
;End of catalog overplot

;Shade regions with bad quality flags:
		polyfill,(wave[nume,i,*]<wavemax)>wavemin,$
			qscale[nume,i,*],/line_fill,$
			orientation=45
;Label the segments:
		smin = segmin[i] > wavemin
		smax = segmax[i] < wavemax
		seglabpos = (smax - smin) * 0.02 + smin
		if (seglabpos ge wavemin and seglabpos le wavemax) then $
			xyouts,seglabpos,ycpos,segid
		if (total(wave[nume,i,*]) eq 0) then begin
				;if no data for this segment, label as such
			xyouts,segmin[i]+40.,ycpos,'NO DATA'
		endif
	endfor
	csize = 2;0.5 for showfheader
	xs = 0.03	;0.07	;showheader position
	ys = 0.6;0.46
	if not keyword_set(nofheader) then $
		showfheader2,xs,ys,header,[1],charsize=csize,/nocounts,$
		/nointtime,/noseg,/calfuse
	inttime = 'Integration time: ' + strcompress(tottime,/rem) + ' s'
	plotit,xs*!d.x_size,ys*!d.y_size,!d.y_ch_size*csize*1.2,$
		csize,inttime,5
	dateit
	!p.multi = [0,1,1]

endit:

end
