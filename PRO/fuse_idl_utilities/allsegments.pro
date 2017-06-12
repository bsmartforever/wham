pro allsegments,name,slitin,binby,fluxmin=fluxmin,fluxmax=fluxmax,$
	wavemin=wavemin,wavemax=wavemax,log=log,hist=hist,catalog=catalog,$
	brightness=brightness,suffix=suffix,channel=channel,overplot=overplot,$
	fluxoffset=fluxoffset,fluxrange=fluxrange,waverange=waverange,$
	colors=colors,waveoffset=waveoffset,$
	waveout=wave,fluxout=flux,qualityout=quality,errorout=error,$
	chanout=chanval,noqual=noqual,nofheader=nofheader,notitle=notitle,$
	ratio=ratio,counts=counts,countrate=countrate,_extra=e
;+
; NAME:
;	ALLSEGMENTS
;
; PURPOSE:
;	This procedure plots the flux vs. wavelength for all four channels,
;	using the calibrated data files. NOTE that this program only works
;	for files processed with versions of the pipeline before 3.0. For
;	later versions, use ALLSEGMENTS3.
;
; CATEGORY:
;	Analysis.
;
; CALLING SEQUENCE:
;	ALLSEGMENTS, Exposure, Slit, Binning
;
; INPUTS:
;	Exposure:
;		Exposure number, e.g. I9040104017. Include the path if
;		the callibrated files are not in the current directory. If
;		an array containing multiple exposures is used, the individual
;		exposures will be averaged before display, unless the OVERPLOT
;		keyword is used. The program currently uses an unweighted
;		average.
;	Slit:	Name of slit: 'LWRS', 'MDRS', 'HIRS' or 'PINH'. This can be
;		either a scalar string, or a vector, with a different slit
;		for each input file.
;	Binby:	Binning factor. Must be a power of 2.
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
;	CATALOG:Filename of a catalog to overplot.
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
; 	Written by David Sahnow, 9/20/99.
;	28 September 1999 Added FLUXMIN keyword.
;	12 October 1999 Added LOG keyword.
;	21 November 1999 Added keyword to read HIST files. Previously
;	 assumed TTAG (in file name).
;	22 November 1999 Added segment labels to plots.
;	22 November 1999 Added automatic switch to try histogram mode if
;	 TTAG doesn't work.
;	23 November 1999 Now gracefully handles missing data.
;	8 December 1999 Added comment that rebinning doesn't properly
;	 combine errors.
;	23 January 2000 Added CATALOG and BRIGHTNESS keywords.
;	29 February 2000 Corrected positions of labels when WAVEMIN and
;	 WAVEMAX keywords are used.
;	2 March 2000 Corrected positions of labels when LOG keyword is used.
;	26 May 2000 Added SUFFIX keyword.
;	30 May 2000 Replaced call to SHOWFHEADER with SHOWFHEADER2.
;	2 June 2000 Added CATALOG keyword.
;	19 June 2000 Fixed bug introduced on 2 June - crashed if no catalog.
;	21 November 2000 Added CHANNEL keyword.
;	27 November 2000 Added OVERPLOT, FLUXOFFSET, FLUXRANGE, WAVERANGE,
;	 WAVEOFFSET, COLORS, keywords. Modified Slit to optionally be an array.
;	11 January 2001 Fixed bug which caused a crash if segment 1a data
;	 didn't exist.
;	12 January 2001 Now forces the solid angle to be a scalar in the
;	 conversion to brightness units.
;	25 January 2001 Now avoids plotting or shading outside of main plot
;	 area, and only plots segment labels if that segment is visible. Added
;	 WAVEOUT, FLUXOUT, QUALITYOUT, ERROROUT, and CHANOUT keywords.
;	14 February 2001 Fixed a bug in defining chanval[].
;	6 September 2001 Changed default FLUXMIN to be -0.1*FLUXMAX, and added
;	 a dashed line at flux=0. Added an overplot of the scaled quality
;	 flag. Added NOQUAL keyword.
;	27 September 2001 Changed order of the lines that set FLUXMIN and
;	 FLUXMAX so that if the default FLUXMIN is used, it is done after
;	 FLUXMAX is set.
;	22 October 2001 Added _extra keyword to allow keywords to be passed
;	 to PLOT; added NOFHEADER and NOTITLE keywords.
;	7 February 2002 Added RATIO keyword.
;	1 May 2002 Added /calfuse keyword in call to SHOWFHEADER2.
;	3 June 2002 Added COUNTS keyword.
;	11 June 2002 Added COUNTRATE keyword
;	11 February 2004 Added message about not working with v3.x.
;	2006-02-15     wvd     Replaced all calls to findfile with file_search.
;-

;catalog = '$CATALOG/mortonlist.dat'

	print,'WARNING: Does not work with pipeline versions 3.0 and later!'
	print,' Use ALLSEGMENTS3 instead.'
	print

	chan = ['SiC1','SiC1','SiC2','SiC2','LiF1','LiF1','LiF2','LiF2']
	seg = ['A','B','A','B','A','B','A','B']
	segmin = [1003.9,905.0,916.8,1016.3,987.1,1094.3,1085.6,978.1]
	segmax = [1091.1,992.6,1006.4,1105.0,1082.2,1187.7,1181.7,1074.6]

	nums = n_elements(chan)		;number of separate spectra per exposure
	nume = n_elements(name)	;number of separate exposures

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
;		fname = fname1+strslit+'ttagfcal'+suffix+'.fit'
		for k=0,nume-1 do $
			fname[k,*] = $
				fname1+strslit[k]+'ttagfcal'+suffix+'.fit'
	endelse

	if keyword_set(channel) then begin	;all channels?
		goodch = where(strupcase(channel) eq strupcase(chan))
		chan = chan(goodch)
		seg = seg(goodch)
		segmin = segmin(goodch)
		segmax = segmax(goodch)
		fname = fname(*,goodch)
print,goodch
	endif

	newbins = fix(16384 / binby)

	wave = fltarr(nume+1,nums,newbins)
	flux = fltarr(nume+1,nums,newbins)
	quality = fltarr(nume+1,nums,newbins)
	error = fltarr(nume+1,nums,newbins)
	chanval = strarr(nume+1,nums)
	;Format for these arrays is [no_exposures,segment/channel_no,wave_bins]
	;Space is also reserved for the total (averaged) spectra (at the end).

	tottime = 0.
	;Do a test to see if the file exists:
	data = mrdfits((name[0]+fname[0,0]),1)

	if (n_elements(data) eq 1) then begin
		print,'Could not find ',name[0]+fname[0,0]
		print,'Switching to histogram mode'
;		fname = fname1+strslit+'histfcal.fit'
			;maybe it's a histogram
		for k=0,nume-1 do $
			fname[k,*] = $
				fname1+strslit[k]+'histfcal'+suffix+'.fit'
	endif

	;Read the files:
	good = intarr(nums)
	for j=0,nume-1 do begin
		infile = name[j] + fname[j,*]
		nums2 = n_elements(fname[0,*])
		for i=0,nums2-1 do begin
			filecheck = file_search(infile[i])
			if (filecheck[0] eq '') then begin
				print,'file ',infile[i],' not found'
				wave[j,i,*] = 0
				flux[j,i,*] = 0
				quality[j,i,*] = 0
				error[j,i,*] = 0
			endif else begin
				good[i] = 1
				data = mrdfits(infile[i],1)
				wave[j,i,*] = rebin(data.wave,newbins)
				flux[j,i,*] = rebin(data.flux,newbins)
				if keyword_set(counts) then $
					flux[j,i,*] = rebin(data.counts,newbins) $
						* float(binby)
				if keyword_set(countrate) then $
					flux[j,i,*] = rebin(data.counts,newbins) $
						* float(binby)
				quality[j,i,*] = rebin(data.quality,newbins)
				error[j,i,*] = rebin(data.error,newbins)
				print,'Note that errors are not properly combined when rebinning!'
			endelse
		endfor
		good1 = where(good eq 1)
		temp = mrdfits(infile[good1[0]],0,header)
		tottime = tottime + fxpar(header,'EXPTIME')
		if keyword_set(countrate) then flux = flux / tottime
	endfor
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
	qheight = yr[0] + 1.0*(yr[1]-yr[0])
	qscale(where(qscale ne 0)) = qheight	;scaled quality flag
	qscale(where(qscale eq 0)) = yr[0]
	qscale[*,*,0] = qheight
	qscale[*,*,newbins-1] = qheight
	qscale2 = quality/max(quality[0:1,*,*]) * qheight
	for i=0,nums2-1 do begin
		spectid = chan[i]+' - '+slitname[slit]
		segid = seg[i]
;print,spectid,segid
for j=0,nume-1 do begin
chanval[j,i] = name[j] + ' - ' + chan[i]+seg[i] + ' - ' + slitname[slit[j]]
					;added [j] to slit, 2/14/01
endfor
		chanval[nume,i] = 'Average' + ' - ' + chan[i]+seg[i] + $
			' - ' + slitname[slit[0]]
		if (2*(i/2) eq i) then begin
			mtitle = name[0] +  fname[0,i]+' & '+fname[0,i+1]
			if (nume gt 1) then mtitle = mtitle + ' (& ' + $
				strcompress(nume-1,/remove)+' other exposures) '
			if keyword_set(notitle) then mtitle = ''
			if (binby ne 1) then mtitle = mtitle + ' binned by ' + $
				strcompress(binby,/rem)
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
		idlen = (!y.crange[1]-!y.crange[0])*0.15
					;maximum length of ID line
		idlab = idtop + 0.05
					;ID label Y location
		for j=0,catlines-1 do begin	;overplot the line IDs
			idscale = inten[j]/max(inten)	;scale intensity
;NOTE that intensity should be scaled separately for different species
			idbot = idtop - idlen*idscale
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

end
