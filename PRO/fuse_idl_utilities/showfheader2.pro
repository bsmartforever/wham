pro plotit,x,y,ystep,charsize,text,linenum	;actual plotting

	xplots = float(!p.multi[1]>1)
	yplots = float(!p.multi[2]>1)
	xscale = x/xplots + !x.window[0]
	yscale = (y-linenum*ystep)/yplots + !y.window[0]
;print,xplots,yplots,xscale,yscale
	xyouts,xscale,yscale,text,charsize=charsize/yplots,/norm
;y-linenum*ystep
end

pro showfheader2, x, y, fitsheader, image, charsize=charsize, $
	nocounts=nocounts, nointtime=nointtime, nosegment=nosegment, $
	norootname=norootname, notarget=notarget, noid=noid, $
	calfuse=calfuse, opus=opus
;+
; NAME:
;	SHOWFHEADER2
;
; PURPOSE:
;	This program puts a legend on a 2D image showing data from
;	the image's FITS header.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	SHOWFHEADER2, X, Y, Fitsheader, Image
;
; INPUTS:
;	X:	Normalized X position of FITS header data on plot.
;	Y:	Normalized Y position of FITS header data on plot.
;	Fitsheader:
;		The FITS header of the file, which contains data on
;	the data (or simulated data) in the image.
;	Image:	The 2D image
;
; KEYWORDS:
;	CHARSIZE:
;		Character size of label. Default is 1.0.
;	NOCOUNTS:
;		If set, don't display the number of counts.
;	NOINTTIME:
;		If set, don't display the integration time.
;	NOSEGMENT:
;		If set, don't display the segment name.
;	NOROOTNAME:
;		If set, don't display the root name.
;	NOTARGET:
;		If set, don't display the target name.
;	NOID:	If set, don't display the program ID.
;	CALFUSE:If set, include the CALFUSE version number
;	OPUS	:If set, include the OPUS version number
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
; 	Written by David Sahnow as SHOWFHEADER, 18 June 1997.
;	4 December 1997 Forced total counts to not be in scientific notation.
;	8 March 1999 Modified to work with OPUS files.
;	20 September 1999 Added CHARSIZE, NOCOUNTS, NOINTTIME and
;	 NOSEGMENT keywords.
;	30 May 2000 Renamed to SHOWFHEADER2. Modified to properly accommodate
;	 !p.multi plots.
;	16 August 2000 Added '>1' to !p.multi in PLOTIT to avoid divide
;	 by zero when !p.multi = 0.
;	1 May 2002 Added CALFUSE, OPUS keywords.
;	30 July 2003 Added NOROOTNAME, NOTARGET, NOID keywords.
;-


;Target Name, program ID, observation ID, exposure ID:
	target = ''
	progid = ''
	obsid = ''
	expid = ''

	if (not keyword_set(notarget)) then begin
		temp = strcompress(fxpar(fitsheader,'TARGNAME'),/remove)
		if (temp ne '0') then $
			target = 'Target: ' +  temp
	endif

	if (not keyword_set(noid)) then begin
		temp = strcompress(fxpar(fitsheader,'PRGRM_ID'),/remove)
		if (temp ne '0') then $
			progid = 'Program ID: ' +  temp

		temp = strcompress(fxpar(fitsheader,'SCOBS_ID'),/remove)
		if (temp ne '0') then $
			obsid = 'Obs ID: ' +  temp

		temp = strcompress(fxpar(fitsheader,'EXP_ID'),/remove)
		if (temp ne '0') then $
			expid = 'Exp ID: ' +  temp
	endif

;Filename, date, time:
	temp1 = strcompress(fxpar(fitsheader,'FILENAME'),/remove)
	temp2 = strcompress(fxpar(fitsheader,'ROOTNAME'),/remove)
	filename = ''
	if (not keyword_set(norootname)) then begin
		if (temp1 ne '0') then $
			filename = 'Filename: ' + temp1
		if (temp2 ne '0') then $
			filename = 'Rootname: ' + temp2
	endif

;Date:
	temp1 = strcompress(fxpar(fitsheader,'DATE'),/remove)
	temp2 = strcompress(fxpar(fitsheader,'DATEOBS'),/remove)
	date = ''
	if (temp1 ne '0') then $
		date = 'Date (DD/MM/YY): ' + temp1
	if (temp2 ne '0') then $
		date = 'Date: ' + temp2

;Time:
	temp1 = strcompress(fxpar(fitsheader,'TIME'),/remove)
	temp2 = strcompress(fxpar(fitsheader,'TIMEOBS'),/remove)
	time = ''
	if (temp1 ne '0') then $
		time = 'Time: ' + temp1
	if (temp2 ne '0') then $
		time = 'Time: ' + temp2

;Integration time:
	temp1 = strcompress(fxpar(fitsheader,'INT-TIME'),/remove)
	temp2 = strcompress(fxpar(fitsheader,'EXPTIME'),/remove)
	inttime = ''
	if (not keyword_set(nointtime)) then begin
		if (temp1 ne '0') then $
			inttime = 'Integration time: ' + temp1 + ' s'
		if (temp2 ne '0') then $
			inttime = 'Integration time: ' + temp2 + ' s'
	endif

;Count rate:
	temp = strcompress(fxpar(fitsheader,'CNT-RATE'),/remove)
	if (temp ne '0') then $
		cntrate = 'Count Rate: ' +  temp + ' Kc/s' $
	else cntrate = ''

;Total counts:
	if keyword_set(nocounts) then begin
		totcnts = ''
	endif else begin
		totcnts = 'Counts: ' + $
		strcompress(string(total(image),format='(f15.0)'),/remove)
	endelse

;Detector segment:
	temp1  = strcompress(fxpar(fitsheader,'DET_SGMT'),/remove)
	temp2  = strcompress(fxpar(fitsheader,'DETECTOR'),/remove)
	segment = ''
	if (not keyword_set(nosegment)) then begin
		if (temp1 ne '0') then $
			segment = 'Segment ' + temp1
		if (temp2 ne '0') then $
			segment = 'Segment ' + temp2
	endif

;CalFUSE version:
	temp = strcompress(fxpar(fitsheader,'CF_VERS'),/remove)
	if keyword_set(calfuse) then $
		cf_vers = 'Calfuse v ' + temp  $
	else cf_vers = ''

;OPUS version:
	temp = strcompress(fxpar(fitsheader,'OPUSVERS'),/remove)
	if keyword_set(opus) then $
		opusvers = 'OPUS v ' + temp  $
	else opusvers = ''

;Dark rate:
	temp  = strcompress(fxpar(fitsheader,'DARKRATE'),/remove)
	if (temp ne '0') then $
		darkrate = 'Dark Rate: ' + temp + ' c/s/cm^2' $
	else darkrate = ''

;Dark file:
	temp  = strcompress(fxpar(fitsheader,'DARKFILE'),/remove)
	if (temp ne '0') then $
		darkfile = 'Dark File: ' + temp $
	else darkfile = ''

;Effective area file:
	temp  = strcompress(fxpar(fitsheader,'AEFFFILE'),/remove)
	if (temp ne '0') then $
		aefffile = 'Effective Area File: ' + temp $
	else aefffile = ''

;Response file:
	temp  = strcompress(fxpar(fitsheader,'RESFILE'),/remove)
	if (temp ne '0') then $
		resfile = 'Response File: ' + temp $
	else resfile = ''

;Jitter:
	tempx  = strcompress(fxpar(fitsheader,'JITTERX'),/remove)
	tempy  = strcompress(fxpar(fitsheader,'JITTERY'),/remove)
	if ((tempx ne '0') or (tempy ne '0')) then $
		jitter = 'Jitter: (' + tempx +', '+ tempy + ') um?' $
	else jitter = ''

;MCP:
	tempx  = strcompress(fxpar(fitsheader,'MCPFWHMX'),/remove)
	tempy  = strcompress(fxpar(fitsheader,'MCPFWHMY'),/remove)
	if ((tempx ne '0') or (tempy ne '0')) then $
		mcp = 'MCP FWHM: (' + tempx +', '+ tempy + ') um' $
	else mcp = ''

;Smearing:
	tempx  = strcompress(fxpar(fitsheader,'SMEARX'),/remove)
	tempy  = strcompress(fxpar(fitsheader,'SMEARY'),/remove)
	if ((tempx ne '0') or (tempy ne '0')) then $
		smear = 'SMEAR: (' + tempx +', '+ tempy + ') um' $
	else smear = ''


	xd = float(x)
	yd = float(y)
		
	if (keyword_set(charsize)) then begin
	endif else begin
		charsize = 1.0
	endelse
	
	ystep = charsize*1.2 / 50.;!d.y_size*10.
		;use a spacing of 1.2 * char height between lines

	plotit,xd,yd,ystep,charsize,filename,0
	plotit,xd,yd,ystep,charsize,segment,1
	plotit,xd,yd,ystep,charsize,date,2
	plotit,xd,yd,ystep,charsize,time,3

	plotit,xd,yd,ystep,charsize,cntrate,4
	plotit,xd,yd,ystep,charsize,inttime,5
	plotit,xd,yd,ystep,charsize,totcnts,6
	
	plotit,xd,yd,ystep,charsize,darkrate,8
	plotit,xd,yd,ystep,charsize,darkfile,9
	plotit,xd,yd,ystep,charsize,aefffile,10
	plotit,xd,yd,ystep,charsize,resfile,11

	plotit,xd,yd,ystep,charsize,jitter,13
	plotit,xd,yd,ystep,charsize,mcp,14
	plotit,xd,yd,ystep,charsize,smear,15

	if ((darkrate eq '') and (darkfile eq '') and (aefffile eq '') and $
		(resfile eq '')) then begin
			plotit,xd,yd,ystep,charsize,target,8
			plotit,xd,yd,ystep,charsize,progid,9
			plotit,xd,yd,ystep,charsize,obsid,10
			plotit,xd,yd,ystep,charsize,expid,11
			plotit,xd,yd,ystep,charsize,cf_vers,12
			plotit,xd,yd,ystep,charsize,opusvers,13
	endif

end
